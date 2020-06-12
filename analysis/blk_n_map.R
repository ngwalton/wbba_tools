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

# print only priority blocks?
priority_only <- TRUE


# output files ----

# name of output pdf file if printing maps
out_pdf <- "block_n_map.pdf"


# load data ----

# block shapefile; arguments for readOGR are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# sample WBBA data from ebird
sp <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# data prep ----

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")
sp <- sp[sp$CATEGORY %in% taxa, ]

sp <- sp[sp$COMMON.NAME != "Domestic goose sp. (Domestic type)", ]

# create a SpatialPointsDataFrame from "sp"
wgs84 <- CRS("+init=epsg:4326")  # use WGS84 as input CRS
coordinates(sp) <- ~ LONGITUDE + LATITUDE
proj4string(sp) <- wgs84

# transform projection to match blocks
nad83 <- CRS(proj4string(block_in))  # use NAD83 from block_in
sp <- spTransform(sp, nad83)

# extract blocks that overlay points; returns a data frame containing the same
# number rows as sp; each row is a record from block that overlays the
# points in sp
block_over <- over(sp, block_in)

# COUNTY is in both data frames
names(block_over)[names(block_over) == "COUNTY"] <- "CO_eBird"

# ...and join them to the bird data frame
sp@data <- cbind(sp@data, block_over)

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
if (priority_only) {
  sp <- sp[sp$BLOCK_STAT == "Priority Block", ]
  block_in <- block_in[block_in$BLOCK_STAT == "Priority Block", ]
}

# limit to C1-C4 evidence categories
sp <- sp[BREEDING.BIRD.ATLAS.CATEGORY %in% c("C2", "C3", "C4"), ]

# remove records duplicated across shared checklists
sp_g <- sp[GROUP.IDENTIFIER != ""]  # split records with group id set
sp <- sp[GROUP.IDENTIFIER == ""]    # records without group id are not shared
cols <- c("COMMON.NAME", "SUBSPECIES.COMMON.NAME", "GROUP.IDENTIFIER")
sp_g <- sp_g[! duplicated(sp_g, by = cols)]
sp <- rbind(sp, sp_g)

# order taxonomically
setorder(sp, TAXONOMIC.ORDER, GLOBAL.UNIQUE.IDENTIFIER)

# sum across June/July and not June/July
sp_mo <- sp[, .N, by = .(COMMON.NAME, BLOCK_ID, period)]

# sum across all months
sp$period <- "Year"
sp <- sp[, .N, by = .(COMMON.NAME, BLOCK_ID, period)]

# add months back to sp
sp <- rbind(sp, sp_mo)

# order period levels
sp$period <- factor(sp$period, levels = period_levels)

#old code with 5 levels, distinguishing 2 records from 3 records
#code <- c("1", "2", "3", "4-10", ">10")
#sp$N1 <- "0"
#sp[N < 4, "N1"] <- code[sp[N < 4, N]]
#sp[N %in% 4:10, "N1"] <- code[4]
#sp[N > 10, "N1"] <- code[5]
#sp$N <- factor(sp$N1, levels = code)
#sp$N1 <- NULL

code <- c("1", "2-3", "4-10", ">10")
sp$N1 <- "0"
sp[N < 2, "N1"] <- code[sp[N < 2, N]]
sp[N %in% 2:3, "N1"] <- code[2]
sp[N %in% 4:10, "N1"] <- code[3]
sp[N > 10, "N1"] <- code[4]
sp$N <- factor(sp$N1, levels = code)
sp$N1 <- NULL


# print maps ----

# print an n records map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  sp_vec <- unique(sp$COMMON.NAME)

  line_blue <- "#235FFF"
  pal <- c("#F4A582", "#CA0020",  "#6A6A6A", "black")
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

    current <- sp[COMMON.NAME == species, ]
    current <- merge(block_in, current, by = "BLOCK_ID", all = TRUE, duplicateGeoms  = TRUE)

    # m_title <- paste(species, month.name[mo], sep = ": ")
    m_title <- c(species, rep("", length(period_levels)))

    out <-  tm_shape(cnty) +
      tm_polygons(border.col = line_blue, alpha = 0, border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_shape(current) +
      tm_polygons("N", title = "n records/period", palette = pal, colorNA = "black", border.alpha = 0) +
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
