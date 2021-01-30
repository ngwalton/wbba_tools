# Script to produce maps of species by month to visualize the progression of
# observations. Observations within a given month are colored to indicate which
# quartile they were observed in. Note that this script will require internet
# access to retrieve the latest eBird taxonomy.


library(rgdal)
library(foreign)
library(tmap)
library(USAboundaries)
library(lubridate)
library(auk)  # needed for eBird taxonomy
library(RColorBrewer)
library(parallel)  # only needed for detectCores
library(data.table)

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


# output files ----

# base name of output pdf file if printing maps. if split_fam is TRUE, the
# family name will be appended to the file name. if max_sp > 0, a file number
# will be appended to the file name. the pdf extension will be appended so do
# not include it here.
out_pdf <- "date_visualizer"


# load data ----

# eBird filter
fltr <- readOGR("ebirdfilters20170817.kml", "ebirdfilters20170817")

# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# sample WBBA data from ebird
ebird_file <- "ebird_data_sample_wbbaii.txt"
n_core <- detectCores() - 1
sp <- fread(ebird_file, quote = "", nThread = n_core, check.names = TRUE)

# eBird taxonomy needed to match up eBird range map with species
ebird_taxa <- get_ebird_taxonomy()


# functions ----

# function to find quartile of day within in month of given year
mo_quartile <- function(d) {
  breaks <- quantile(seq_len(days_in_month(d)))
  out <- findInterval(day(d), breaks, rightmost.closed = TRUE)
  out
}

# function to divide a vector of common names (spec_vec) into a list of vectors
# with max length max_sp
get_groups <- function(sp_vec, max_sp) {
  split(sp_vec, ceiling(seq_along(sp_vec) / max_sp))
}

# make a single species map
make_map <- function(sp, species, cnty, fltr, pal, jitter = 0.15) {
  # sp is a data.frame, potentially containing multiple species
  # species is the species to map
  # cnty is the county bounds spatial data
  # fltr is the eBird filter spatial data
  # pal is the is color palate to use when plotting observations
  # jitter is the amount of jitter to add when plotting observations

  # subset to desired species
  current <- sp[sp$COMMON.NAME == species, ]

  # title will be common name
  m_title <- species

  # line color for county bounds
  line_gray <- "#4e4e4e"

  out <-  tm_shape(cnty) +
    tm_polygons(border.col = line_gray, alpha = 0, lwd = 0.5, border.alpha = 0.2,
      legend.show = FALSE) +
    tm_shape(current) +
    tm_dots("quartile", size = 0.1, title = "Quarter Month", pal = pal,
      jitter = jitter, legend.hist = TRUE, legend.hist.title = "Frequency") +
    tm_facets(by = "month", free.coords = FALSE, drop.empty.facets = FALSE,
      free.scales = FALSE, nrow = 1) +
    tm_shape(fltr) +
    tm_polygons(border.col = "#00cc44", alpha = 0, lwd = 1.25,
      legend.show = FALSE) +
    tm_layout(title = m_title, title.size = 1,
      title.position = c("right", "top")) +
    tm_legend(bg.alpha = 0, outside = FALSE, position = c("left", "bottom"),
      hist.bg.alpha = 0, hist.height = 0.15, hist.width = 0.3)

  out
}


# data prep ----

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")
sp <- sp[CATEGORY %in% taxa]

# this will need modification if other non-species need to be removed
sp <- sp[COMMON.NAME != "Domestic goose sp. (Domestic type)"]

# add family from eBird taxonomy
cols <- c("common_name", "family")
sp <- merge(sp, ebird_taxa[, cols], by.x = "COMMON.NAME",
               by.y = "common_name", all.x = TRUE)

# order taxonomically
setorder(sp, TAXONOMIC.ORDER)

# add date columns
sp[, DDDD := ymd(OBSERVATION.DATE)]

sp[, month := month(DDDD)]
month_levels <- sp[, .(lab = month.name[month]), by = month][order(month)]
sp[, month := factor(month, levels = month_levels$month,
  labels = month_levels$lab)]

# calculate quartile within month
sp[, quartile := mo_quartile(DDDD), by = DDDD]
sp[, quartile := factor(quartile, levels = 1:4, labels = paste0("q", 1:4))]

# set empty breeding evidence category to lowest level
sp[BREEDING.BIRD.ATLAS.CATEGORY == "" , BREEDING.BIRD.ATLAS.CATEGORY := "C1"]

cat_levels <- unique(sp$BREEDING.BIRD.ATLAS.CATEGORY)
cat_levels <- sort(cat_levels)
sp[, BREEDING.BIRD.ATLAS.CATEGORY := factor(BREEDING.BIRD.ATLAS.CATEGORY,
  levels = cat_levels)]


# make spatial ----

# coordinate reference system: WGS84
wgs84 <- CRS("+init=epsg:4326")

# create a SpatialPointsDataFrame from "sp" -- sp is no longer a data.table
coordinates(sp) <- ~ LONGITUDE + LATITUDE
proj4string(sp) <- wgs84


# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {

  # pal <- brewer.pal(4, "RdGy")
  pal <- brewer.pal(4, "PuOr")
  # shapes <- c(3, 4, 2, 1)

  # pdf dimensions
  n_plots <- 12
  width = 7 * n_plots
  height = 7

  if (split_fam) {
    fam_vec <- unique(sp$family)
    fam_vec <- vapply(fam_vec, function(x) strsplit(x, " ")[[1]][1],
      NA_character_)

    for (j in seq_along(fam_vec)) {
      fam <- fam_vec[j]
      current_fam <- sp[sp$family == names(fam), ]
      sp_vec <- unique(current_fam$COMMON.NAME)

      if (max_sp) {  # case split by family and number of species
        sp_groups <- get_groups(sp_vec, max_sp)

        for (i in seq_along(sp_groups)) {
          grp <- sp_groups[[i]]
          current_pdf <- paste0(out_pdf, "_", fam, i, ".pdf")
          pdf(current_pdf, width = width, height = height)

          for (species in grp) {
            out <- make_map(sp, species, cnty, fltr, pal)
            print(out)
          }

          dev.off()
        }
      } else {  # case split only by family
        current_pdf <- paste0(out_pdf, "_", fam, ".pdf")
        pdf(current_pdf, width = width, height = height)

        for (species in sp_vec) {
          out <- make_map(sp, species, cnty, fltr, pal)
          print(out)
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
          out <- make_map(sp, species, cnty, fltr, pal)
          print(out)
        }

        dev.off()
      }
    } else {  # case all species in single file
      pdf(paste0(out_pdf, ".pdf"), width = width, height = height)

      for (species in sp_vec) {
        out <- make_map(sp, species, cnty, fltr, pal)
        print(out)
      }

      dev.off()
    }
  }
}
