# Script to add breeding evidence to WBBA blocks.
#
# Author: Nicholas Walton
# Created: 9 Sept 2015
# Last updated: 11 Sept 2015


library("rgdal")    # also loads package 'sp'
library("reshape2") # for dcast function
library("foreign")  # for read.dbf (alpha codes come as dbf)

# my working directory contains the folders "AlphaCodes", "Bird", and "Blocks"
setwd("C:/git_repositories/WbbaDistMaps/Data")

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("./AlphaCodes/LIST15.DBF")

# arguments for readOGR are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- readOGR("./Blocks", "WbbaBlocks2015_v0_2")

sp_in <- read.csv("./Bird/JantoMay2015AtlasData.csv", stringsAsFactors = FALSE)

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")
sp_in <- sp_in[sp_in$CATEGORY %in% taxa, ]

# add alpha codes needed later to name species columns with < 10 chars
sp_in <- merge(sp_in, alpha[, c("COMMONNAME", "SPEC")], by.x = "COMMON.NAME",
                 by.y = "COMMONNAME", all.x = TRUE, all.y = FALSE)

# check that all common names in sp_in were matched in alpha
any(is.na(sp_in$SPEC))  # should return false

# create a SpatialPointsDataFrame from "sp_in"
wgs84 <- CRS("+init=epsg:4326")  # use WGS84 as input CRS
sp_wgs <- SpatialPointsDataFrame(sp_in[, c("LONGITUDE", "LATITUDE")], sp_in,
                                 coords.nrs = c(23, 22), proj4string = wgs84)

# transform projection to match blocks
nad83 <- CRS(proj4string(block_in))  # use NAD83 from block_in
sp_nad <- spTransform(sp_wgs, nad83)

# extract blocks that overlay points;
# returns a data frame containing the same number rows as sp_nad;
# each row is a record from block that overlays the points in sp_nad
block_over <- over(sp_nad, block_in)
names(block_over)[13] <- "CO_eBird"  # COUNTY is in both data frames

# ...and join them to the bird data frame
sp <- cbind(sp_nad@data, block_over)


# Up to this point, one could have loaded the eBird records into ArcGIS as
# points, selected taxonomic categories (CATEGORY), done a spatial join with
# the blocks layer, and finally exported the table from the points.  Once eBird
# adds BLOCK_ID or similar to the downloadable data, the preceding steps will
# be unnecessary.
#
# There might be a way to do the following in ArcGIS Desktop, but I don't
# know how off the top of my head.  It could definitely be scripted in arcpy,
# but manipulating tabular data is generally much easier/more intuitive in R.


# add column for breeding evidence code
sp$conf <- 0

# list of lists containing the breeding codes;
# numbers are used at first instead of names to simply finding the highest
# breeding evidence
breeding_codes <- list(
  list(1, "Observed",  c("", "F")),
  list(2, "Possible",  c("H", "S")),
  list(3, "Probable",  c("S7", "M", "P", "T", "C", "N", "A", "B")),
  list(4, "Confirmed", c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF",
                         "FY", "FS", "NE", "NY"))
  )

# some of the BREEDING.BIRD.ATLAS.CODE codes have a space at the end
# and some don't - this removes the space
sp$BREEDING.BIRD.ATLAS.CODE <- gsub(" ", "", sp$BREEDING.BIRD.ATLAS.CODE,
                                    fixed = TRUE)

# assign numeric breeding code (1 = lowest, 4 = highest)
for (code in breeding_codes) {
  sp$conf[sp$BREEDING.BIRD.ATLAS.CODE %in% code[[3]]] <- code[[1]]
}

# The following function/dcast could be replaced with nested for loops
# (one iterating over blocks, and a 2nd over each species in a given block)
# followed by dcast if more than one value needed to be caluclated (e.g., if you
# wanted confirmation code and behavior or something).

# function to assign breeding code name;
# for a given alpha code/block combo, all values of "conf" are passed to this
# function which returns the highest breeding evidence name
code_name <- function(x){
  code_num <- max(x)
  breeding_codes[[code_num]][[2]]
}

# this creates a data frame with BLOCK_ID as the 1st column, followed by
# columns for each alpha code, with breeding evidence name as the cell values
sp_cast <- dcast(sp, BLOCK_ID ~ SPEC, fun.aggregate = code_name,
                 fill = "", value.var = "conf")


# At this point, one could also just export sp_cast to csv or dbf and join it
# with the blocks in ArcGIS.  But it's easy to do in R.


# merge species with original blocks
block_out <- merge(block_in, sp_cast, by = "BLOCK_ID")

# write to disc as a shapefile
writeOGR(block_out, "./out", "JantoMay2015AtlasData20150919",
         driver = "ESRI Shapefile")
