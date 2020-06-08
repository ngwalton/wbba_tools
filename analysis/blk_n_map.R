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

setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE

# months of interest -- the months will be aggregated
mo <- c("June", "July")


# output files ----

# name of output pdf file if printing maps
out_pdf <- "block_n_map.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
# source: http://www.birdpop.org/pages/birdSpeciesCodes.php
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# block shapefile; arguments for readOGR are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# sample WBBA data from ebird
sp_in <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# data prep ----

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")
sp_in <- sp_in[sp_in$CATEGORY %in% taxa, ]

# add alpha codes needed later to name species columns with < 10 chars required
# for shapefile
sp_in <- merge(sp_in, alpha[, c("COMMONNAME", "SPEC")], by.x = "COMMON.NAME",
               by.y = "COMMONNAME", all.x = TRUE, all.y = FALSE)

# if any species were unmatched in alpha, this will print there names; this will
# require additional attention if any are not matched
if (any(is.na(sp_in$SPEC))) {
  unique(sp_in$COMMON.NAME[is.na(sp_in$SPEC)])
}

# this will need modification depending on what species were not matched; in
# this case we provide a custom alpha code domestic Guineafowl and Mallard, and
# remove Domestic goose sp.
sp_in$SPEC[sp_in$COMMON.NAME == "Helmeted Guineafowl (Domestic type)"] <- "HEGU"
sp_in$SPEC[sp_in$COMMON.NAME == "Mallard (Domestic type)"] <- "MALL_DOM"
sp_in <- sp_in[sp_in$COMMON.NAME != "Domestic goose sp. (Domestic type)", ]

# order taxonomically
sp_in <- sp_in[order(sp_in$TAXONOMIC.ORDER), ]

# create a SpatialPointsDataFrame from "sp_in"
wgs84 <- CRS("+init=epsg:4326")  # use WGS84 as input CRS
sp_wgs <- sp_in
coordinates(sp_wgs) <- ~ LONGITUDE + LATITUDE
proj4string(sp_wgs) <- wgs84

# transform projection to match blocks
nad83 <- CRS(proj4string(block_in))  # use NAD83 from block_in
sp_nad <- spTransform(sp_wgs, nad83)

# extract blocks that overlay points; returns a data frame containing the same
# number rows as sp_nad; each row is a record from block that overlays the
# points in sp_nad
block_over <- over(sp_nad, block_in)

# COUNTY is in both data frames
names(block_over)[names(block_over) == "COUNTY"] <- "CO_eBird"

# ...and join them to the bird data frame
sp_nad@data <- cbind(sp_nad@data, block_over)


sp <-sp_nad

# some of the BREEDING.BIRD.ATLAS.CODE codes have a space at the end
# and some don't - this removes the space
sp$BREEDING.BIRD.ATLAS.CODE <- trimws(sp$BREEDING.BIRD.ATLAS.CODE)

# add date columns
sp$DDDD <- ymd(sp$OBSERVATION.DATE)

sp$month <- month(sp$DDDD)
sp$month <- month.name[sp$month]

sp$period <- sp$month
indx <- sp$period %in% mo
period_levels <- c("Year", "June/July", "Not June/July")
sp$period[indx] <- period_levels[2]
sp$period[! indx] <- period_levels[3]

sp <- setDT(sp@data)

# limit to priority blocks
sp <- sp[sp$BLOCK_STAT == "Priority Block", ]
block_in <- block_in[block_in$BLOCK_STAT == "Priority Block", ]

# limit to C1-C4 evidence categories
sp <- sp[BREEDING.BIRD.ATLAS.CATEGORY %in% c("C2", "C3", "C4"), ]

# sum across June/July and not June/July
sp_mo <- sp[, .N, by = .(COMMON.NAME, SPEC, BLOCK_ID, period)]

# sum across all months
sp$period <- "Year"
sp <- sp[, .N, by = .(COMMON.NAME, SPEC, BLOCK_ID, period)]

# add months back to sp
sp <- rbind(sp, sp_mo)

# order period levels
sp$period <- factor(sp$period, levels = period_levels)

code <- c("1", "2", "3", "4-10", ">10")
sp$N1 <- "0"
sp[N < 4, "N1"] <- code[sp[N < 4, N]]
sp[N %in% 4:10, "N1"] <- code[4]
sp[N > 10, "N1"] <- code[5]
sp$N <- factor(sp$N1, levels = code)
sp$N1 <- NULL


# print maps ----

# print an n records map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  sp_vec <- unique(sp$SPEC)

  line_gray <- "#4e4e4e"
  pal <- c("#cceeff", "#66ccff", "#0099e6", "#006699", "#002233")
  n <- length(sp_vec)

  # open pdf device
  n_plots <- length(period_levels) + 1
  width = 7 * n_plots
  height = 7
  pdf(out_pdf, width = width, height = height)

  for (i in seq_along(sp_vec)) {
    if (i == 1) {
      message(paste("Printing", n, "maps"))
      t0 <- Sys.time()
    }

    species <- sp_vec[i]

    current <- sp[SPEC == species, ]
    current <- merge(block_in, current, by = "BLOCK_ID", all = TRUE, duplicateGeoms  = TRUE)

    # m_title <- paste(species, month.name[mo], sep = ": ")
    m_title <- alpha[alpha$SPEC == species, "COMMONNAME"]
    m_title <- c(m_title, rep("", length(period_levels)))

    out <-  tm_shape(cnty) +
      tm_polygons(border.col = line_gray, alpha = 0, border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_shape(current) +
      tm_polygons("N", title = "n records/period", palette = pal) +
      tm_facets(by = "period", free.coords = FALSE,
                free.scales = TRUE, nrow = 1) +
      tm_layout(title = m_title, title.size = 1, title.position = c("left", "bottom")) +
      tm_legend(bg.alpha = 0, position = c("right", "top"))

    print(out)

    message(paste("Finished map", i, "of", n))

    if (i == 1) {
      t1 <- Sys.time()
      t_el <- t1 - t0
      t_el <- round(t_el * n / 60, 1)
      message(paste("Estmimated time to print:", t_el, "minutes"))
    }
  }

  # close pdf device
  dev.off()
}
