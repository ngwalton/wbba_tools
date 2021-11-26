# Script to add highest breeding evidence level to WBBA blocks. Results are exported to
# shapefile.


library(rgdal)
library(reshape2)
library(foreign)
library(tmap) # only needed for map making
library(USAboundaries) # only needed for map making
library(here)

setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

# name of output shapefile without extension
out_shp <- "evidence_by_block"

# name of output pdf file if printing maps
out_pdf <- "evidence_maps.pdf"


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

# remove not valid (reason = exotic) records
sp_in <- subset(sp_in, APPROVED != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
sp_in <- transform(sp_in, CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))

# remove spuh, slash, and domestic taxa
taxa <- c("species", "issf", "form", "hybrid", "pigeon")
sp_in <- sp_in[sp_in$CATEGORY %in% taxa, ]

# add alpha codes needed later to name species columns with < 10 chars required
# for shapefile
sp_in <- merge(sp_in, alpha[, c("COMMONNAME", "SPEC")], by.x = "COMMON.NAME",
                 by.y = "COMMONNAME", all.x = TRUE, all.y = FALSE)

# if any species were unmatched in alpha, this will print their names; this will
# require aditional attention if any are not matched
if (any(is.na(sp_in$SPEC))) {
  unique(sp_in$COMMON.NAME[is.na(sp_in$SPEC)])
}

# this will need modification depending on what species were not matched; in
# this case we provide a custom alpha code for Great Tit and and
# remove Domestic goose sp.
sp_in$SPEC[sp_in$COMMON.NAME == "Great Tit"] <- "GTIT"
sp_in <- sp_in[sp_in$COMMON.NAME != "Domestic goose sp. (Domestic type)", ]
# more fixes for WI
sp_in$SPEC[sp_in$COMMON.NAME == "Common Ground Dove)"] <- "CGDO" #Must have changed since 2018

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
names(block_over)[13] <- "CO_eBird"  # COUNTY is in both data frames

# ...and join them to the bird data frame
sp <- cbind(sp_nad@data, block_over)

# some of the BREEDING.CODE codes have a space at the end
# and some don't - this removes the space
sp$BREEDING.CODE <- trimws(sp$BREEDING.CODE)

# add column for breeding evidence code
sp$conf <- 0


# add evidence codes ----

# list of lists containing the breeding codes;
# numbers are used at first instead of names to simply finding the highest
# breeding evidence
breeding_codes <- list(
  list(1, "Observed",  c("", "F", "O", "NC")),
  list(2, "Possible",  c("H", "S")),
  list(3, "Probable",  c("S7", "M", "P", "T", "C", "N", "A", "B")),
  list(4, "Confirmed", c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF",
                         "FY", "FS", "NE", "NY"))
)

# assign numeric breeding code (1 = lowest, 4 = highest)
for (code in breeding_codes) {
  sp$conf[sp$BREEDING.CODE %in% code[[3]]] <- code[[1]]
}

# function to assign breeding code name; for a given alpha code/block combo, all
# values of "conf" are passed to this function which returns the highest
# breeding evidence name
code_name <- function(x){
  code_num <- max(x)
  breeding_codes[[code_num]][[2]]
}

# create a data frame with BLOCK_ID as the 1st column, followed by
# columns for each alpha code, with breeding evidence name as the cell values
sp_cast <- dcast(sp, BLOCK_ID ~ SPEC, fun.aggregate = code_name,
                 fill = "", value.var = "conf")

# merge species with original blocks
block_out <- merge(block_in, sp_cast, by = "BLOCK_ID")


# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  block_map <- block_out
  no_rep <- "No checklists"
  not_rep <- "Not reported"
  block_map@data[is.na(block_map@data)] <- no_rep
  block_map@data[block_map@data == ""] <- not_rep

  # make evidence a factor and choose factor order -- used to order map legend
  sp_vec <- names(sp_cast)[-1]
  ord <- c(rev(vapply(breeding_codes, "[[", NA_character_, 2)), not_rep, no_rep)
  block_map@data[, sp_vec] <- lapply(sp_vec, function(x)
    factor(block_map@data[[x]], levels = ord))

  line_gray <- "#4e4e4e"
  #          conf      prob       poss      obs        not obs    not sampled
  pal <- c("black", "#820BBB", "#BF5FFF", "#e6cef1", "#e5e5e5", "white") #ebird style
  #pal <- c("#473B00", "#B49518", "#E5D069", "white", "white", "white") #NY style

  n <- length(sp_vec)

  # open pdf device
  pdf(out_pdf)

  for (i in seq_along(sp_vec)) {
    if (i == 1) {
      message(paste("Printing", n, "maps"))
      t0 <- Sys.time()
    }

    species <- sp_vec[i]

    out <- tm_shape(block_map) +
      tm_polygons(species, title = "Evidence", border.col = NULL, palette = pal) +
      tm_shape(cnty) +
      tm_polygons(border.col = line_gray, alpha = 0, border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_legend(title = species, position = c("left", "bottom"), bg.alpha = 0,
                main.title.fontface = 2, title.fontface = 2)

    print(out)

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

# write output ----

# write shapefile
writeOGR(block_out, ".", out_shp, driver = "ESRI Shapefile")

# Optional: Uncomment to print a file that shows single breeding status
# (category) for each species for each block. Among other things, this allows
# you to see which blocks have species reported that are only coded Observed.
# When running this screen, probably want to upload a main EBD file that
# includes non-portal records.
# write.csv(block_out, file = "Species_Categories_By_Block.csv")
