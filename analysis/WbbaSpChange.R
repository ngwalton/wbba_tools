# Script to summarize species detections from WBBA I to WBBA II.  The resulting
# csv and/or shp contains species in columns and blocks in rows; cell values
# indicate if a species was recorded as possible or better ("recorded" from here # on).
# Key to output breeding evidence:
# 0: Not recorded during either WBBA
# 1: Recorded during WBBA I only
# 2: Recorded during WBBA II only
# 3: Recorded during both WBBAs
#
# Script based on "WbbaSpMaps914.R".
#
# Author: Nicholas Walton
# Created: 4 March 2017
# Last updated: 6 March 2017


library("rgdal")    # also loads package 'sp'
library("reshape2") # for dcast function
library("foreign")  # for read.dbf (alpha codes come as dbf)

# my working directory contains the folders "AlphaCodes", "Bird", "Blocks" and
# "out/shp".
setwd("C:/git_repos/wbba_yr_comp/Data")

out_file <- "wbba_change"  # root name for output file (csv and/or shp)

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("./AlphaCodes/LIST16.DBF", as.is = TRUE)

# arguments for readOGR are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- readOGR("./Blocks", "WbbaBlocks2015_v0_2")

sp <- list()
sp$ii <- read.csv("./Bird/EBIRD_ATL_WI.csv", stringsAsFactors = FALSE)
sp$i <- read.csv("./Bird/WBBA_1_dataexported_from_eBird_20150316.csv",
                 stringsAsFactors = FALSE)

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
any(vapply(sp, function(x) anyNA(x$SPEC), NA))  # should return false

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

# sp_wgs <- SpatialPointsDataFrame(sp[, c("LONGITUDE", "LATITUDE")], sp,
#                                  coords.nrs = c(23, 22), proj4string = wgs84)

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

vapply(sp, function(x) anyNA(x$BLOCK_ID), NA)  # should be FALSe - isn't in ii!
sum(is.na(sp$ii$BLOCK_ID)) # 6
# View(sp$ii[is.na(sp$ii$BLOCK_ID), ])
# the following records fall outside of blocks!
# dput(sp$ii[is.na(sp$ii$BLOCK_ID), "GLOBAL.UNIQUE.IDENTIFIER"])
# c("URN:CornellLabOfOrnithology:EBIRD:OBS406758226",
#   "URN:CornellLabOfOrnithology:EBIRD:OBS406757661",
#   "URN:CornellLabOfOrnithology:EBIRD:OBS406773046",
#   "URN:CornellLabOfOrnithology:EBIRD:OBS406772639",
#   "URN:CornellLabOfOrnithology:EBIRD:OBS406773047",
#   "URN:CornellLabOfOrnithology:EBIRD:OBS406757660")

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

# check that all species occure in both data frames
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

# write to csv
write.csv(out, file = paste0("./out/", out_file, ".csv"), row.names = FALSE)

# optionally write to shp:
# merge species with original blocks
block_out <- merge(block_in[, c("BLOCK_ID", "BLOCK_STAT")], out,
                   by = "BLOCK_ID")

writeOGR(block_out, "./out/shp", out_file, driver = "ESRI Shapefile")
