# Script to summarize species detections from WBBA I compared to WBBA II.  The
# resulting csv and/or shp contains species in columns and blocks in rows; cell
# values indicate if a species was recorded as possible or better ("recorded"
# from here on).
# Key to output breeding evidence:
# 0: Not recorded during either WBBA
# 1: Recorded during WBBA I only
# 2: Recorded during WBBA II only
# 3: Recorded during both WBBAs


library(rgdal)    # also loads package 'sp'
library(reshape2) # for dcast function
library(foreign)  # for read.dbf (alpha codes come as dbf)
library(tmap)     # only needed for map making
library(here)

setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

out_file <- "wbba_change"  # root name for output file (csv and/or shp)

# name of output pdf file if printing maps
out_pdf <- "wbba_change_maps.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# arguments for readOGR are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

sp <- list()
sp$ii <- read.csv("WBBA2revi.csv", as.is = TRUE)
sp$i <- read.csv("WBBA1revi.csv", as.is = TRUE)

# optional county layer --  only used for map printing
cnty <- readOGR("county", "County_Boundaries_24K")


# data prep ----

# how do we want to treat domestic MALL?  currently setting to normal MALL.
dom_mall <- sp$ii$COMMON.NAME == "Mallard (Domestic type)"
sp$ii[dom_mall, "COMMON.NAME"] <- "Mallard"

# match column names
names(sp$i)[names(sp$i) == "COM_NAME"] <- "COMMON.NAME"
names(sp$i)[names(sp$i) == "BBA_CODE"] <- "BREEDING.BIRD.ATLAS.CODE"

# update_sp removes non-species taxa (i.e., hybrid, spuh, and slash taxonomic
# categories), removes records with no breeding evidence (i.e., F, NA, and the
# empty string), and adds an alpha code (for column naming).
taxa <- c("species", "issf", "domestic", "form")
update_sp <- function(x) {
  is_sp <- x$CATEGORY %in% taxa
  is_obs <- is.na(x$BREEDING.BIRD.ATLAS.CODE) |
    x$BREEDING.BIRD.ATLAS.CODE %in% c("F ", "")

  keep <- is_sp & ! is_obs
  x <- x[keep, ]

  ord <- match(x$COMMON.NAME, alpha$COMMONNAME)
  x <- cbind(x, alpha[ord, "SPEC", drop = FALSE])

  # currently no official alpha code for Great Tit. GRTI and GRET are already
  # taken, so used GTIT (no conflicts with existing alpha codes).
  x[x$COMMON.NAME == "Great Tit", "SPEC"] <- "GTIT"

  x
}

sp <- lapply(sp, update_sp)

# check that no common names in sp were unmatched in alpha
any(vapply(sp, function(x) anyNA(x$SPEC), NA))  # should return FALSE

# create a SpatialPointsDataFrame from "sp".
wgs84 <- CRS("+init=epsg:4326")      # use WGS84 as input CRS.
nad83 <- CRS(proj4string(block_in))  # will transform to NAD83 from block_in.

mk_spatial <- function(x) {
  coordinates(x) <- ~ LONGITUDE + LATITUDE
  proj4string(x) <- wgs84
  x <- spTransform(x, nad83)
  x
}

sp <- lapply(sp, mk_spatial)

# extract blocks that overlay points;
# get_blks returns a data frame containing the same number rows as x;
# each row is a record from a block that overlays the points in sp.
# lastly, join them to the bird data frame.
get_blks <- function(x) {
  block_over <- over(x, block_in)
  names(block_over)[13] <- "CO_eBird"  # COUNTY is in both data frames
  x <- cbind(x@data, block_over)
  x
}

sp <- lapply(sp, get_blks)

vapply(sp, function(x) anyNA(x$BLOCK_ID), NA)  # should return FALSe
sum(is.na(sp$ii$BLOCK_ID))                     # returns count of unmatched records if there were any
# View(sp$ii[is.na(sp$ii$BLOCK_ID), ])

# Remove records outside of blocks. Hopefully there will be another solution to
# this issue that allows us to keep these records.
sp$ii <- sp$ii[! is.na(sp$ii$BLOCK_ID), ]

# Add column for breeding evidence code.
sp$i$breed <- 1
sp$ii$breed <- 2

# This creates a list of data frames with BLOCK_ID as the 1st column, followed
# by columns for each alpha code, with either a 1 (WBBA I) or 2 (WBBA II)
# indicating some breeding evidence, and 0 indicating no breeding evidence.
sp_cast <- lapply(sp, function(x) dcast(x, BLOCK_ID ~ SPEC, fun.aggregate = max,
                 fill = 0, value.var = "breed"))

# Add missing species and order columns.
unique_to_i  <- setdiff(names(sp_cast$i),  names(sp_cast$ii))
unique_to_ii <- setdiff(names(sp_cast$ii), names(sp_cast$i))
sp_cast$i[, unique_to_ii] <- 0
sp_cast$ii[, unique_to_i] <- 0

# check that all species occur in both data frames
setequal(names(sp_cast$ii), names(sp_cast$i))  # should be TRUE

ord <- c("BLOCK_ID", sort(setdiff(names(sp_cast$ii), "BLOCK_ID")))
sp_cast <- lapply(sp_cast, "[", ord)

identical(names(sp_cast$ii), names(sp_cast$i))  # should be TRUE

# add missing blocks and order rows
sp_cast <- lapply(sp_cast, function(x)
  merge(block_in@data[, "BLOCK_ID", drop = FALSE], x, by = "BLOCK_ID",
        all = TRUE))

set0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

sp_cast <- lapply(sp_cast, set0)

# both should be TRUE
identical(dim(sp_cast$i), dim(sp_cast$ii))
identical(sp_cast$i$BLOCK_ID, sp_cast$ii$BLOCK_ID)

# calculate changes from WBBA I to II.
out <- sp_cast$i
out[, -1] <- sp_cast$i[, -1] + sp_cast$ii[, -1]

# merge species with original blocks
block_out <- merge(block_in[, c("BLOCK_ID", "BLOCK_STAT")], out,
                   by = "BLOCK_ID")


# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  block_map <- block_out

  sp_vec <- names(block_map)[- c(1:2)]

  # make evidence a factor and choose factor order -- used to order map legend
  labels <- c("Unreported", "WBBA I only", "WBBA II only", "Both")
  block_map@data[, sp_vec] <- lapply(sp_vec, function(x)
    factor(block_map@data[[x]], levels = 0:3, labels = labels))

  line_gray <- "#4e4e4e"
  pal <- c("white", "red", "green", "blue")

  n <- length(sp_vec)

  # open pdf device
  pdf(out_pdf)

  for (i in seq_along(sp_vec)) {
    if (i == 1) {
      message(paste("Printing", n, "maps"))
      t0 <- Sys.time()
    }

    species <- sp_vec[i]

    out_map <- tm_shape(block_map) +
      tm_polygons(species, title = "Legend", border.col = NULL, palette = pal) +
      tm_shape(cnty) +
      tm_polygons(border.col = line_gray, alpha = 0, border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_legend(title = species, position = c("left", "bottom"), bg.alpha = 0,
                main.title.fontface = 2, title.fontface = 2)

    print(out_map)

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


# save output files ----

# write to csv
write.csv(out, file = paste0(out_file, ".csv"), row.names = FALSE)

# optionally write to shp
writeOGR(block_out, ".", out_file, driver = "ESRI Shapefile")
