rm(list = ls())

library(sf) ## handling polygon vectors
library(rasterVis) ## plotting rasters 
library(viridis) ## color pallette 
library(ggthemes) ## for themes in ggplot
library(paletteer)## call in color pallette
library(extrafont) ## load in fonts from system - this has not implemented yet
library(ggnewscale) ## control spatial scale in overlayed ggplot
library(tidyverse) ## data tidy - filter, join, select, etc through pipe
library(terra) ## handling raster data
library(raster)## handling raster data 
library(ggpubr) ## for arranging multipanel maps
library(gridExtra) ## for annotating with model evaluation tables and texts
library(reshape2)
library(foreign)
library(tmap) # only needed for map making
library(USAboundaries) # only needed for map making

## Load data and set up working drive
#setwd(here::here("data"))

# source: http://www.birdpop.org/pages/birdSpeciesCodes.php
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# block shapefile; arguments for st_read are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- st_read("blk", "WbbaBlocks2015_v0_2")

# optional county layer --  only used for map printing
cnty <- us_counties(resolution = "high", states = "WI")

#optional lakes layer
#lakes<-st_read("F:\\WBBA_data\\pred_maps\\large_lakes24_hydro.shp")
# optional ecological landscapes layer
#this one just sizes things properly
ecoland <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                   "ecoshape_smooth100")
#this one looks nice
ecoland2 <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                    "EL_Simp")
# sample WBBA data from ebird
sp_in2 <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)

#sp_in <- read.delim("RECR_final_Mar23_for_type_maps3.txt", quote = "", as.is = TRUE)
sp_in <- read.csv("RECR_final_Mar23_for_type_maps3.csv")

# data prep ----

(gc)

#column names to uppercase if needed
names(sp_in) <- toupper(names(sp_in))
library(stringr)   
colnames(sp_in) <- str_replace_all(colnames(sp_in), "[:punct:]", ".")

# for testing purposes, shorten entire dataset to 2 species
#sp_in <- sp_in[grepl("Red Crossbill (Sitka Spruce or type 10)|Red Crossbill (Western Hemlock or type 3)", sp_in$COMMON.NAME), ]

# limit to atlas portal records
sp_in <- subset(sp_in, PROJECT.CODE == "EBIRD_ATL_WI")

#load crosswalk file
cw <- read.csv("filerenamecrosswalk2forrc.csv")

#cut down to common name and breeding evidence
cw2 <- dplyr::select(cw, common_name, BreedingEvidence)
#rename column
names(cw2)[names(cw2) == 'common_name'] <- 'COMMON.NAME'

#the inner join merges with just the final atlas species, dropping non-breeders
#sp_in <- inner_join(sp_in, cw2, by = "COMMON.NAME")

# A second way to do it, full join then drop ones where BreedingEvidence is NA
sp_in <- full_join(sp_in, cw2, by = "COMMON.NAME")
sp_in_filtered <- sp_in %>%
filter(!is.na(BreedingEvidence))
# in this instance you may have to delete BBMA, ETSP, KEGU, MOBL which have species accts
# but no atlas data

# remove obsposs species
remove_species <- c(
  "Laughing Gull",
  "Ring-billed Gull",
  "Herring Gull",
  "Great Black-backed Gull",
  "Forster's Tern",
  "Common Tern",
  "Caspian Tern",
  "Laughing Gull",
  "Double-crested Cormorant",
  "American White Pelican",
  "Turkey Vulture",
  "Osprey",
  "Bald Eagle",
  "Great Blue Heron",
  "Great Egret",
  "Snowy Egret",
  "Cattle Egret",
  "Black-crowned Night-Heron",
  "Yellow-crowned Night-Heron",
  "Whooping Crane",
  "Spotted Sandpiper"
)

sp_in <- sp_in[! sp_in$COMMON.NAME %in% remove_species, ]

# remove not valid (reason = exotic) records
sp_in <- subset(sp_in, APPROVED != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
sp_in <- transform(sp_in, CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))

# remove spuh, slash, and domestic taxa
# here I am removing hybrid because it was causing trouble having no species code
taxa <- c("species", "issf", "form", "pigeon")
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
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Appalachian or type 1)"] <- "RCON" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Douglas-fir or type 4)"] <- "RCFO" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Lodgepole Pine or type 5)"] <- "RCFI" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Northeastern or type 12)"] <- "RCTW" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Ponderosa Pine or type 2)"] <- "RCTO" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Sitka Spruce or type 10)"] <- "RCTE" 
sp_in$SPEC[sp_in$COMMON.NAME == "Red Crossbill (Western Hemlock or type 3)"] <- "RCTH" 

(gc)

# create a sf data.frame from "sp_in"
wgs84 <- st_crs(4269)
sp_wgs <- st_as_sf(sp_in, crs = wgs84, coords = c("LONGITUDE", "LATITUDE"))

block_in <- block_in %>%
  dplyr::select(BLOCK_ID, BLOCK_NAME)

# transform projection to match blocks
sp_nad <- st_transform(sp_wgs, st_crs(block_in))

# spatial join points with blocks: returns an sf data.frame containing the
# same number rows as sp_df; each row is an original point record from sp_df
# plus the info from the block that overlays that point
sp <- st_join(sp_nad, block_in)


# some of the BREEDING.CODE codes have a space at the end
# and some don't - this removes the space
sp$BREEDING.CODE <- trimws(sp$BREEDING.CODE)

# make any NAs Not reported
sp <- sp %>%
  mutate(BREEDING.CODE = replace_na(as.character(BREEDING.CODE), "Not reported"))

# add column for breeding evidence code
sp$conf <- 0

(gc)


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

# Remove rows where the conf value remains 0
sp <- sp %>%
  filter(conf != 0)

# function to assign breeding code name; for a given alpha code/block combo, all
# values of "conf" are passed to this function which returns the highest
# breeding evidence name

code_name <- function(x){
  # Handle the empty case first.
  if (length(x) == 0) {
    return("")
  }
  # For non-empty input, find the max code number and return its name.
  code_num <- max(x)
  breeding_codes[[code_num]][[2]]
}

# Check for any remaining NAs in the BREEDING.CODE column
sum(is.na(sp$BREEDING.CODE))

(gc)

# Select only the relevant columns for dcast
sp_clean <- sp %>% dplyr::select(BLOCK_ID, SPEC, conf)

# Then perform the dcast on the cleaned data
sp_cast <- dcast(sp_clean, BLOCK_ID ~ SPEC, fun.aggregate = code_name,
                 fill = "", value.var = "conf")

# merge species with original blocks
block_out <- merge(block_in, sp_cast, by = "BLOCK_ID", all.x = TRUE)

# sort on "BLOCK_NAME"
# this is for consistency with results when using sp
block_out <- block_out[order(block_out$BLOCK_NAME), ]

bestmapimg<-list()

block_map <- block_out
no_rep <- "No checklists"
not_rep <- "Not reported"
# block_map[block_map == ""] <- not_rep

# there's gotta be a better way to do this, but the "geometry" column was
# causing an issue with block_map[block_map == ""] <- not_rep
for (name in names(block_map)) {
  if (nchar(name) == 4) {
    block_map[is.na(block_map[[name]]), name] <- not_rep
  }
}

#species<-c("BBCU","SACR","REVI")
species <- unique(sp$SPEC)

line_gray <- "#666666"
class(block_map$RCTE)

pal <- c("Confirmed" = "#2D1C45", 
         "Probable" = "#9C7DC5", 
         "Possible" = "#D4C7E6", 
         "Observed" = "NA", 
         "Not reported" = "NA", 
         "No checklists" = "NA")

labels <- c("Confirmed", "Probable", "Possible", "Observed", "Not reported", "No checklists")



# make evidence a factor and choose factor order -- used to order map legend

ord <- c(rev(vapply(breeding_codes, "[[", NA_character_, 2)), not_rep, no_rep)

# Define ord and ensure the `pal` order matches it exactly
ord <- c("Confirmed", "Probable", "Possible", "Observed", "Not reported", "No checklists")


(gc)

block_map[, species] <- lapply(species, function(x)
  factor(block_map[[x]], levels = ord))

for(j in 1:length(species)){
  ## transform rasters into SpatialPixelsDataFrame
  plot_blocks <- block_map[, species[j]]
  # make evidence a factor and choose factor order -- used to order map legend
  plot_block_df <- as.data.frame(plot_blocks)
  names(plot_block_df) <- c("value","geometry")
  ## set bins for categroizing color scale of block level occurrence
  plot_block_df<-plot_block_df%>%st_as_sf()
  ## block level occurrences are binned to <1 (no observation), 1-2 (low observation),
  ## and any above 2 (many repeated observations across time and space within a block)
  ## model prediction maps
  plot_each<-ggplot() +  
    #geom_raster(data=plot_each_df, aes(x=x, y=y, fill=bins,color=bins)) + 
    #geom_sf(data=lakes, fill="#BFE1F4", color=NA) +  
    #invisible but standardizes size
    geom_sf(data=ecoland, fill=NA, color=NA, size=0) +
    geom_sf(data=block_in, fill=NA, color=NA, size=0) +
    coord_sf (datum = sf::st_crs(4269)) +
    geom_sf(data=cnty, fill=NA, color="gray60", size=0.6) +
    geom_sf(data=ecoland2, fill=NA, color="#87d979", linetype ="dashed", linewidth=0.2) +
    geom_sf(data=plot_block_df, aes(fill=value), color=NA) +  
    coord_sf()+
    #ggtitle(paste(species[j],": best _ quant"))+
    theme_map()+
    theme(legend.position="none") +
    theme(plot.margin = unit(c(-0.15,-0.15,-0.15,-0.15), "in")) +
    scale_fill_manual(labels=labels,
                      values=pal,
                      na.translate = F,
                      na.value = NA)
  #bestmapimg[[species[j]]]<-list(overlay_map,plot_each)
  bestmapimg[[species[j]]] <- plot_each
}

# Create a lookup table for naming
species_lookup <- sp_in %>%
  dplyr::distinct(SPEC, .keep_all = TRUE) %>%
  dplyr::select(SPEC, COMMON.NAME, BreedingEvidence)


#### for loop for ggarrange and saving into folder directory
  for(l in 1:length(species)){
    ## make ggarrange
    #path<-"F:\\review_maps\\2025_blocks_evidence"
    
    # Find the row in the species_lookup table
    species_info <- species_lookup[species_lookup$SPEC == species[l], ]
    
    # Construct the new filename
    filename_new <- paste0(species_info$BreedingEvidence, "_", species_info$COMMON.NAME, ".eps")
    
    
  
  ggsave(filename=filename_new,plot=bestmapimg[[species[l]]],  
         colormodel = "cmyk",
         device = "eps",
         scale = 1,  width = 3.75,  height = 3.9469,  units = c("in"),
         dpi = 600, 
         limitsize = FALSE,  bg = "white")
}

