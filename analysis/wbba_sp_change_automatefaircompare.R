# Script to summarize species detections from WBBA I compared to WBBA II.  The
# resulting csv and/or shp contains species in columns and blocks in rows; cell
# values indicate if a species was recorded as possible or better ("recorded"
# from here on).
# Key to output breeding evidence:
# 0: Not recorded during either WBBA
# 1: Recorded during WBBA I only
# 2: Recorded during WBBA II only
# 3: Recorded during both WBBAs

# THIS VERSION OF THE CHANGE MAPS ONLY SHOWS RESULTS FOR BLOCKS 
# WITH ADEQUATE EFFORT DURING BOTH ATLAS PERIODS
# (see fair folder for that shapefile)

library(rgdal)    # also loads package 'sp'
library(reshape2) # for dcast function
library(foreign)  # for read.dbf (alpha codes come as dbf)
library(tmap)     # only needed for map making
library(USAboundaries) # only needed for map making
library(here)

setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

out_file <- "wbba_change"  # root name for output file (csv and/or shp)

# name of output pdf file if printing maps
out_pdf <- "sgcn_fair_change.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# arguments for readOGR are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

sp <- list()
sp$ii <- read.delim("SGCN_SIN_withoutsensitivesp_WBBA2.txt", quote = "", as.is = TRUE)
sp$i <- read.delim("SGCN_SIN_withoutsensitivesp_WBBA1.txt", quote = "", as.is = TRUE)


# optional county layer --  only used for map printing
cnty <- us_counties(resolution = "high", states = "WI")

# load the blank block outlines to show which blocks are fair comparison
fair<- readOGR("fair", "faircomparisonblocks")
#column rename renames the block id column so it will match later
#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
names(fair)[names(fair) == 'BLOCK_ID'] <- 'ATLAS.BLOCK'

# data prep ----

# remove not valid (reason = exotic) records
sp$ii <- subset(sp$ii, APPROVED != "0")
sp$i <- subset(sp$i, APPROVED != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
sp$ii <- transform(sp$ii, CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))
sp$i <- transform(sp$i, CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))

# update_sp removes non-species taxa (i.e., hybrid, spuh, domestic, and slash taxonomic
# categories), removes records with no breeding evidence (i.e., F, NA, and the
# empty string), and adds an alpha code (for column naming).
taxa <- c("species", "issf", "form", "pigeon")
update_sp <- function(x) {
  is_sp <- x$CATEGORY %in% taxa
  is_obs <- is.na(x$BREEDING.CODE) |
    x$BREEDING.CODE %in% c("F ", "NC", "") #########################################update this in other codes!
  
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

#merge fair column onto sp, which also limits it to only rows from fair comparison blocks
sp$i <- merge(fair@data, sp$i, by="ATLAS.BLOCK")
sp$ii <- merge(fair@data, sp$ii, by="ATLAS.BLOCK")

sapply(sp$i, class)

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

vapply(sp, function(x) anyNA(x$BLOCK_ID), NA)  # should return FALSE
sum(is.na(sp$ii$BLOCK_ID))  # returns count of unmatched records if any
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

# delete rows from fair shapefile so that the only blocks with outlines are the empty ones
#
##############################BUT HOW DO I DO THIS BY SPECIES?
#  
# making a version of block out with a new column name
#block_out_copy <-block_out
#names(block_out_copy@data)[names(block_out_copy@data) == 'BLOCK_ID'] <- 'ATLAS.BLOCK'
#foremptyblocks <- merge(fair@data, block_out_copy@data, by="ATLAS.BLOCK")
#
#############################I ALSO WANT TO FIX THE LEGEND SO IT HAS AN OUTLINE FOR NOT DETECTED BUT NO OUTLINE FOR REGULAR

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
  #         not det  at1    at2      both
  # pal <- c("white", "red", "green", "blue")
  pal <- c("white", "#ffa200",  "#00aeff", "#5c5c5c") # orange blue dkgray
 
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
      tm_polygons(species, border.col = NULL, palette = pal, legend.show = FALSE) +
      tm_shape(fair) +
            tm_borders("black", lwd=0.2)  +
      tm_shape(cnty) +
            tm_polygons(border.col = "#b0a158", alpha = 0, border.alpha = 0.3,
                  legend.show = FALSE) +
      tm_legend(title = species) +
      tm_add_legend(
                  title = "Legend",
                  type = c("fill"),
                  labels = labels,
                  col = pal,
                  shape = 21,
                  border.col = "black")
    
    
    print(out_map)
    
    message(paste("Finished map", i, "of", n))
    
    if (i == 1) {
      t1 <- Sys.time()
      t_el <- difftime(t1, t0, units = "secs")
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


