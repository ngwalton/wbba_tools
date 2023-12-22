# Script to add highest breeding evidence level to WBBA blocks. Results are exported to
# shapefile.

# This version only does crossbills - working off a version where RECR type is in the Common Name column.


#This version only displays Confirmed, Probable, Possible Codes
#This version has the blocks draw over the county lines
#This version adds an optional ecological landscapes layer
#This version prints each map out as a separate pdf file

library(sf)
library(reshape2)
library(foreign)
library(tmap) # only needed for map making
library(USAboundaries) # only needed for map making
library(here)

#setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

# name of output shapefile without extension
out_shp <- "evidence_by_block"

# name of output pdf file if printing maps
out_pdf <- "evidence_maps5.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
# source: http://www.birdpop.org/pages/birdSpeciesCodes.php
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# block shapefile; arguments for st_read are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- st_read("blk", "WbbaBlocks2015_v0_2")

# optional county layer --  only used for map printing
cnty <- us_counties(resolution = "high", states = "WI")

# optional ecological landscapes layer
# ecoland <- st_read("Ecological_Landscapes_of_Wisconsin",
#   "Ecological_Landscapes_of_Wisconsin")

# sample WBBA data from ebird
sp_in <- read.delim("Atlaswi_typesasspecies_ebd_US-WI_redcro_201501_201912_relOct-2023.txt", quote = "", as.is = TRUE)

# data prep ----

# take names to uppercase
names(sp_in) <- toupper(names(sp_in))
library(stringr)   
colnames(sp_in) <- str_replace_all(colnames(sp_in), "[:punct:]", ".")

# limit to atlas portal records
sp_in <- subset(sp_in, PROJECT.CODE == "EBIRD_ATL_WI")

# remove rows with no breeding code
sp_in <- sp_in[!is.na(sp_in$BREEDING.CODE),]

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
sp_in$SPEC[sp_in$COMMON.NAME == "Common Ground Dove"] <- "CGDO" #Must have changed since 2018
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Appalachian or type 1)"] <- "RCT1" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Douglas-fir or type 4)"] <- "RCT4" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Lodgepole Pine or type 5)"] <- "RCT5" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Northeastern or type 12)"] <- "RC12" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Ponderosa Pine or type 2)"] <- "RCT2" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Sitka Spruce or type 10)"] <- "RC10" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Western Hemlock or type 3)"] <- "RCT3" 

# create a sf data.frame from "sp_in"
wgs84 <- st_crs(4326)
sp_wgs <- st_as_sf(sp_in, crs = wgs84, coords = c("LONGITUDE", "LATITUDE"))

# transform projection to match blocks
sp_nad <- st_transform(sp_wgs, st_crs(block_in))

# spatial join points with blocks: returns an sf data.frame containing the
# same number rows as sp_df; each row is an original point record from sp_df
# plus the info from the block that overlays that point
sp <- st_join(sp_nad, block_in)

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
block_out <- merge(block_in, sp_cast, by = "BLOCK_ID", all.x = TRUE)

# sort on "BLOCK_NAME"
# this is for consistency with results when using sp
block_out <- block_out[order(block_out$BLOCK_NAME), ]


# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  block_map <- block_out
  no_rep <- "No checklists"
  not_rep <- "Not reported"
  block_map[is.na(block_map)] <- no_rep
  # block_map[block_map == ""] <- not_rep
  
  # there's gotta be a better way to do this, but the "geometry" column was
  # causing an issue with block_map[block_map == ""] <- not_rep
  for (name in names(block_map)) {
    if (nchar(name) == 4) {
      block_map[block_map[[name]] == "", name] <- not_rep
    }
  }
  
  # make evidence a factor and choose factor order -- used to order map legend
  sp_vec <- names(sp_cast)[-1]
  ord <- c(rev(vapply(breeding_codes, "[[", NA_character_, 2)), not_rep, no_rep)
  block_map[, sp_vec] <- lapply(sp_vec, function(x)
    factor(block_map[[x]], levels = ord))
  
  #old color options
  #line_gray <- "#4e4e4e"
  #line_gold <- "#b5a905"
  #421559
  #5c3278"
  #BF5FFF
  #"#820BBB"
  
  #  making a transparent color which turns out to be #00000001
  #  mycol <- rgb(0, 0, 0, max = 255, alpha = 1, names = "invis")
  #  mycol
  #  invis
  
  # in this version the last 3 categories are set to invisible
  #          conf      prob       poss      obs        not obs    not sampled
  pal <- c("black", "#7145AC", "#D0B9EF", "#00000001", "#00000001", "#00000001") #muted ebird purple
  #pal <- c("#473B00", "#B49518", "#E5D069", "white", "white", "white") #NY style
  
  n <- length(sp_vec)
  
  # open pdf device
  
  
  for (i in seq_along(sp_vec)) {
    pdf(file=paste0(sp_vec[i],".pdf"),onefile = F)
    if (i == 1) {
      message(paste("Printing", n, "maps"))
      t0 <- Sys.time()
    }
    
    species <- sp_vec[i]
    
    # this is now ordered to have the blocks on top of the lines
    # and the lower categories are now set to transparent
    # and the three lowest category labels are set to blank
    
    out <-
      # tm_shape(ecoland) +
      #       tm_borders(col = "#90EE90", lwd = 0.4, lty = "dashed", alpha = 1) +
      tm_shape(cnty) +
      tm_polygons(border.col = "gray60", lwd = 0.8, alpha = 0, border.alpha = 1,
                  legend.show = FALSE) +
      tm_shape(block_map, is.master = TRUE) +
      tm_polygons(species, title = "Evidence", border.col = NULL, border.alpha = 0,
                  lwd = 0, palette = pal, labels = c("Confirmed", "Probable", "Possible", "", "", "")) +
      tm_legend(title = species, position = c("left", "bottom"), bg.alpha = 0,
                main.title.fontface = 2, title.fontface = 2)
    
    print(out)
    dev.off()# close pdf device and save last-printed plot as pdf to set pdf path and filename
    
    message(paste("Finished map", i, "of", n))
    
    if (i == 1) {
      t1 <- Sys.time()
      t_el <- difftime(t1, t0, units = "secs")
      t_el <- round(t_el * n / 60, 1)
      message(paste("Estmimated time to print:", t_el, "minutes"))
    }
  }
}

# write output ----

# write shapefile
st_write(block_out, out_shp, driver = "ESRI Shapefile")

# Optional: Uncomment to print a file that shows single breeding status
# (category) for each species for each block. Among other things, this allows
# you to see which blocks have species reported that are only coded Observed.
# When running this screen, probably want to upload a main EBD file that
# includes non-portal records.
# write.csv(block_out, file = "Species_Categories_By_Block.csv")