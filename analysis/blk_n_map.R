# Script to produce maps of species by month to visualize the number of
# records per block. Results in four maps per species, one each for year,
# June/July combined, all other months combined, and all blocks with no records
# ("missing").


library(rgdal)
library(sf)
library(data.table)
library(foreign)
library(tmap) # only needed for map making
library(USAboundaries) # only needed for map making
library(lubridate)
library(auk)  # needed for eBird taxonomy
library(readxl)

setwd(here::here("data"))


# pdf options ----

# the following should be updated as the user sees fit. to print a single output
# pdf, set split_fam to FALSE, and max_sp to 0.

# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE

# split by family (TRUE/FALSE). if TRUE, the output pdf will be split across
# files based on family.
split_fam <- TRUE

# max species per file (0 or a positive integer). if set to a positive
# integer, the output pdf will be split to have no more than max_sp species. set
# to 0 to suppress splitting by the number of species.
max_sp <- 20


# other options ----

# months of interest -- the months will be aggregated
mo <- c("June", "July")

# print only priority/specialty blocks?
priority_only <- TRUE
priority_lvls <- c("Priority Block", "Specialty Block")

# include eBird range map? requires downloading/unzipping maps from eBird at:
# https://ebird.org/science/status-and-trends/download-data
include_range <- TRUE

# location of directory containing optional eBird range map files; required if
# include_range is TRUE; only species with breeding and/or resident ranges will
# include range in the output map
range_dir <- "./ebird_range"


# output files ----

# base name of output pdf file if printing maps. if split_fam is TRUE, the
# family name will be appended to the file name. if max_sp > 0, a file number
# will be appended to the file name. the pdf extension will be appended so do
# not include it here.
out_pdf <- "block_n_map"


# load data ----

# block shapefile; arguments for readOGR are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# sample WBBA II data from eBird
sp <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)

# sample WBBA I from eBird
sp1 <- read.delim("ebird_data_sample_wbbai.txt", quote = "", as.is = TRUE)

# point count data
pt_count <- read_excel("point_count_data_sample_wbbaii.xlsx",
                       sheet = "sample_data")

# alpha codes to match point-count data with atlas data
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# eBird taxonomy needed to match up eBird range map with species
tax <- get_ebird_taxonomy()


# functions ----

# function to prep atlas data
prep_sp <- function(sp_df, blk, taxa, mo, priority_only, priority_lvls) {

  sp_df <- sp_df[sp_df$CATEGORY %in% taxa, ]

  sp_df <- sp_df[sp_df$COMMON.NAME != "Domestic goose sp. (Domestic type)", ]

  # create a SpatialPointsDataFrame from "sp_df"
  wgs84 <- CRS("+init=epsg:4326")  # use WGS84 as input CRS
  coordinates(sp_df) <- ~ LONGITUDE + LATITUDE
  proj4string(sp_df) <- wgs84

  # transform projection to match blocks
  nad83 <- CRS(proj4string(blk))  # use NAD83 from blk
  sp_df <- spTransform(sp_df, nad83)

  # extract blocks that overlay points; returns a data frame containing the same
  # number rows as sp_df; each row is a record from block that overlays the
  # points in sp_df
  block_over <- over(sp_df, blk)

  # COUNTY is in both data frames
  names(block_over)[names(block_over) == "COUNTY"] <- "CO_eBird"

  # ...and join them to the bird data frame
  sp_df@data <- cbind(sp_df@data, block_over)

  # some of the BREEDING.BIRD.ATLAS.CODE codes have a space at the end
  # and some don't - this removes the space
  sp_df$BREEDING.BIRD.ATLAS.CODE <- trimws(sp_df$BREEDING.BIRD.ATLAS.CODE)

  # add date columns
  sp_df$DDDD <- ymd(sp_df$OBSERVATION.DATE)

  sp_df$month <- month(sp_df$DDDD)
  sp_df$month <- month.name[sp_df$month]

  sp_df$period <- sp_df$month
  indx <- sp_df$period %in% mo
  period_levels <- c("Year", "June/July", "Not June/July")
  sp_df$period[indx] <- period_levels[2]
  sp_df$period[! indx] <- period_levels[3]

  sp_df <- setDT(sp_df@data)

  # limit to priority/specialty blocks
  if (priority_only) {
    sp_df <- sp_df[sp_df$BLOCK_STAT %in% priority_lvls, ]
  }

  # limit to C1-C4 evidence categories
  sp_df <- sp_df[BREEDING.BIRD.ATLAS.CATEGORY %in% c("C2", "C3", "C4"), ]

  # remove records duplicated across shared checklists
  sp_g <- sp_df[GROUP.IDENTIFIER != ""]  # split records with group id set
  sp_df <- sp_df[GROUP.IDENTIFIER == ""]    # records without group id are not shared
  cols <- c("COMMON.NAME", "SUBSPECIES.COMMON.NAME", "GROUP.IDENTIFIER")
  sp_g <- sp_g[! duplicated(sp_g, by = cols)]
  sp_df <- rbind(sp_df, sp_g)

  # order taxonomically
  setorder(sp_df, TAXONOMIC.ORDER, GLOBAL.UNIQUE.IDENTIFIER)

  # sum across June/July and not June/July
  sp_mo <- sp_df[, .N, by = .(COMMON.NAME, BLOCK_ID, period)]

  # sum across all months
  sp_df$period <- "Year"
  sp_df <- sp_df[, .N, by = .(COMMON.NAME, BLOCK_ID, period)]

  # add months back to sp_df
  sp_df <- rbind(sp_df, sp_mo)

  # order period levels
  sp_df$period <- factor(sp_df$period, levels = period_levels)

  #old code with 5 levels, distinguishing 2 records from 3 records
  #code <- c("1", "2", "3", "4-10", ">10")
  #sp_df$N1 <- "0"
  #sp_df[N < 4, "N1"] <- code[sp_df[N < 4, N]]
  #sp_df[N %in% 4:10, "N1"] <- code[4]
  #sp_df[N > 10, "N1"] <- code[5]
  #sp_df$N <- factor(sp_df$N1, levels = code)
  #sp_df$N1 <- NULL

  code <- c("1", "2-3", "4-10", ">10")
  sp_df$N1 <- "0"
  sp_df[N < 2, "N1"] <- code[sp_df[N < 2, N]]
  sp_df[N %in% 2:3, "N1"] <- code[2]
  sp_df[N %in% 4:10, "N1"] <- code[3]
  sp_df[N > 10, "N1"] <- code[4]
  sp_df$N <- factor(sp_df$N1, levels = code)
  sp_df$N1 <- NULL

  sp_df
}

# function to divide a vector of common names (spec_vec) into a list of vectors
# with max length max_sp
get_groups <- function(sp_vec, max_sp) {
  split(sp_vec, ceiling(seq_along(sp_vec) / max_sp))
}

# make a single species map
make_map <- function(sp, species, cnty, block_in, pal, tax, clip_box, season_pal) {
  # sp is a data.frame, potentially containing multiple species
  # species is the species to map
  # cnty is the county bounds spatial data
  # block_in is the atlas block spatial data
  # pal is the is color palette to use when plotting observations
  # tax is the eBird taxonomy tibble
  # clip_box is used to clip the eBird range maps to a reasonable size
  # season_pal is the color palette used when plotting the eBird range

  current <- sp[COMMON.NAME == species, ]
  current <- merge(block_in, current, by = "BLOCK_ID", all = TRUE,
    duplicateGeoms  = TRUE)

  current_pt <- pt_count[pt_count$common == species, ]

  # m_title <- paste(species, month.name[mo], sep = ": ")
  m_title <- c(species, rep("", length(levels(sp$period))))

  rng_map <- NULL

  if (include_range) {
    sp_code <- tax$species_code[tax$common_name == species]
    range_file <- paste0(sp_code, "-range-mr-2020.gpkg")

    # embedded file.path to remove potential trailing slash on range_dir
    ebird_range <- file.path(file.path(range_dir), range_file)

    if (file.exists(ebird_range)) {
      ebird_range <- st_read(ebird_range, "range", quiet = TRUE)
      ebird_range <- ebird_range["season_name"]

      seasons <- c("breeding", "resident")
      ebird_range <- ebird_range[ebird_range$season_name %in% seasons, ]
      ebird_range$season_name <- factor(ebird_range$season_name,
        levels = seasons)

      ebird_range$season_name <- droplevels(ebird_range$season_name)

      # clip ebird_range
      # suppressing warnings and messages because st_intersection generates
      # several of each warning about clipping with lon/lat but this is not
      # important for our purposes
      ebird_range <- suppressWarnings(
        suppressMessages(
          st_intersection(ebird_range, clip_box)
        )
      )

      if (nrow(ebird_range) > 0) {
        indx <- names(season_pal) %in% unique(ebird_range$season_name)
        spal <- season_pal[indx]
        rng_map <- tm_shape(ebird_range) +
          tm_polygons("season_name", title = "eBird range",
            border.col = spal, palette = "gray", alpha = 0.5,
            border.alpha = 0.4)
      }
    }
  }

  bg_map <- rng_map +
    tm_shape(cnty, is.master = TRUE) +
    tm_polygons(border.col = line_blue, alpha = 0, border.alpha = 0.4,
      legend.show = FALSE)

  blk_map <- bg_map +
    tm_shape(current) +
    tm_polygons("N", title = "n records/period", palette = pal,
      colorNA = "black", border.alpha = 0) +
    tm_facets(by = "period", free.coords = FALSE,
      free.scales = TRUE, nrow = 1) +
    tm_layout(title = m_title, title.size = 1,
      outer.margins = c(.02, 0.005, .02, 0),
      title.position = c("left", "bottom"), panel.label.height = 1.4,
      panel.label.size = 1.1, legend.title.size = 1.2) +
    tm_legend(bg.alpha = 0, position = c("right", "top"))

  if (nrow(current_pt) > 0) {
    pt_map <- bg_map +
      tm_shape(current_pt) +
      tm_dots("Point count", size = 0.08, col = "red") +
      tm_layout(panel.labels = "Point-count detections",
        outer.margins = c(.02, 0.015, .02, .02),
        panel.label.height = 1.5) +
      tm_legend(bg.alpha = 0, position = c("right", "top"))
  } else {
    pt_map <- bg_map
  }

  out <- tmap_arrange(blk_map, pt_map, widths = c(5, 1),
    outer.margins = NULL)

  out
}

# function to print progress of map printing
progress_message <- function(cnt, n) {
  message(paste("Finished map", cnt, "of", n))
}

# function to print estimated time to print all maps
time_est_message <- function(t0, t1, n) {
  t_el <- t1 - t0
  t_el <- round(t_el * n / 60, 1)
  message(paste("Estimated time to print:", t_el, "minutes"))
}


# data prep ----

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")

sp <- prep_sp(sp, block_in, taxa, mo, priority_only, priority_lvls)
sp1 <- prep_sp(sp1, block_in, taxa, mo, priority_only, priority_lvls)

sp1 <- sp1[sp1$period == "Year"]
wbbai_lab <- "WBBA I (year)"
period_levels <- c(wbbai_lab, levels(sp$period))
sp1$period <- factor(wbbai_lab, levels = period_levels)

sp$period <- factor(sp$period, levels = period_levels)

sp <- rbind(sp, sp1)

# add family and taxonomic order from eBird taxonomy
cols <- c("common_name", "family", "taxon_order")
sp <- merge(sp, tax[, cols], by.x = "COMMON.NAME",
  by.y = "common_name", all.x = TRUE)

# order taxonomically
setorder(sp, taxon_order)

# limit to priority/specialty blocks
if (priority_only) {
  block_in <- block_in[block_in$BLOCK_STAT %in% priority_lvls, ]
}

# aggregate point counts to reduce size of output
form <- count ~ pointid + latitude + longitude + speciescode
pt_count <- aggregate(form, pt_count, sum)

# add common name to point count data
pt_count$common <- alpha[match(pt_count$speciescode, alpha$SPEC), "COMMONNAME"]

# check for unmatched common names and fix any that should be included in
# analysis; in the full WBBA2 data, we fix Canada Jay and remove a couple
# hybrids
unique(pt_count$speciescode[is.na(pt_count$common)])
pt_count[pt_count$speciescode == "GRAJ", "common"] <- "Canada Jay"
pt_count <- pt_count[! is.na(pt_count$common), ]

pt_count[, "Point count"] <- "Presence"

# create a SpatialPointsDataFrame from "pt_count"
coordinates(pt_count) <- ~ longitude + latitude
nad83 <- CRS(proj4string(block_in))  # use NAD83 from block_in
proj4string(pt_count) <- nad83  # based on meta data

# create polygon for clipping eBird range maps; eBird range maps are clipped to
# reduce output file size
clip_box <- st_bbox(cnty)

# adjust the divisor if clipping removes too much of the eBird range map (e.g.
# if you can see the clipped edge of the range map in the printed version)
add_x <- (clip_box["xmin"] - clip_box["xmax"]) / 4
add_y <- (clip_box["ymax"] - clip_box["ymin"]) / 4

clip_box["xmax"] <- clip_box["xmax"] - add_x
clip_box["xmin"] <- clip_box["xmin"] + add_x
clip_box["ymax"] <- clip_box["ymax"] + add_y
clip_box["ymin"] <- clip_box["ymin"] - add_y

clip_box <- st_as_sfc(clip_box)


# print maps ----

# print an n records map for each species to a or more pdfs; note that this can
# be time consuming

if (print_map) {

  line_blue <- "#235FFF"
  pal <- c("#F4A582", "#CA0020",  "#6A6A6A", "black")
  n <- length(unique(sp$COMMON.NAME))

  season_pal <- c(breeding = "red", resident = "black")

  # pdf dimensions
  n_plots <- length(period_levels) + 2
  width = 7 * n_plots
  height = 7 * 1.15

  message(paste("Printing", n, "maps"))
  cnt <- 0  # file counter
  t0 <- Sys.time()

  if (split_fam) {
    fam_vec <- unique(sp$family)

    for (j in seq_along(fam_vec)) {
      fam <- fam_vec[j]
      current_fam <- sp[sp$family == fam, ]
      sp_vec <- unique(current_fam$COMMON.NAME)

      if (max_sp) {  # case split by family and number of species
        sp_groups <- get_groups(sp_vec, max_sp)

        for (i in seq_along(sp_groups)) {
          grp <- sp_groups[[i]]
          current_pdf <- paste0(out_pdf, "_", fam, i, ".pdf")
          pdf(current_pdf, width = width, height = height)

          for (species in grp) {
            out <- make_map(sp, species, cnty, block_in, pal, tax, clip_box,
              season_pal)

            print(out)

            cnt <- cnt + 1
            progress_message(cnt, n)

            if (cnt == 1) {
              t1 <- Sys.time()
              time_est_message(t0, t1, n)
            }
          }

          dev.off()
        }
      } else {  # case split only by family
        current_pdf <- paste0(out_pdf, "_", fam, ".pdf")
        pdf(current_pdf, width = width, height = height)

        for (species in sp_vec) {
          out <- make_map(sp, species, cnty, block_in, pal, tax, clip_box,
            season_pal)

          print(out)

          cnt <- cnt + 1
          progress_message(cnt, n)

          if (cnt == 1) {
            t1 <- Sys.time()
            time_est_message(t0, t1, n)
          }
        }

        dev.off()
      }
    }
  } else {
    sp_vec <- unique(sp$COMMON.NAME)

    if (max_sp) {  # case split by number of species
      sp_groups <- get_groups(sp_vec, max_sp)

      for (i in seq_along(sp_groups)) {
        grp <- sp_groups[[i]]
        current_pdf <- paste0(out_pdf, i, ".pdf")
        pdf(current_pdf, width = width, height = height)

        for (species in grp) {
          out <- make_map(sp, species, cnty, block_in, pal, tax, clip_box,
            season_pal)

          print(out)

          cnt <- cnt + 1
          progress_message(cnt, n)

          if (cnt == 1) {
            t1 <- Sys.time()
            time_est_message(t0, t1, n)
          }
        }

        dev.off()
      }
    } else {  # case all species in single file
      pdf(paste0(out_pdf, ".pdf"), width = width, height = height)

      for (species in sp_vec) {
        out <- make_map(sp, species, cnty, block_in, pal, tax, clip_box,
          season_pal)

        print(out)

        cnt <- cnt + 1
        progress_message(cnt, n)

        if (cnt == 1) {
          t1 <- Sys.time()
          time_est_message(t0, t1, n)
        }
      }

      dev.off()
    }
  }
}
