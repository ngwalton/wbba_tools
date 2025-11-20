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

# THIS VERSION DOES NOT COMPUTE THE CHANGE INDEX ITSELF BUT PULLS IN
# THE BOOTSTRAPPED CHANGE INDEX AND CONFIDENCE INTERVALS

# THIS VERSION FILTERS OUT NONSTANDARD SPECIES FOR WISCONSIN

# THIS VERSION PRINTS EACH SPECIES TO AN INDIVIDUAL PDF

library(sf)       # for mapping and handling shp files
library(dplyr)    # if you want to use tidyverse functions
library(purrr)    # for working with lists
library(tidyr)    # replacement of reshape2 package
library(foreign)  # for read.dbf (alpha codes come as dbf)
library(tmap)     # only needed for map making
library(USAboundaries) # only needed for map making
library(here)     # for loading files
library(rasterVis) ## plotting rasters 
library(viridis) ## color pallette 
library(ggthemes) ## for themes in ggplot
library(paletteer)## call in color pallette
library(extrafont) ## load in fonts from system - this has not implemented yet
library(ggnewscale) ## control spatial scale in overlayed ggplot
library(tidyverse) ## data tidy - filter, join, select, etc through pipe
library(terra) ## handling raster data
library(raster)## handling raster data 

#setwd(here::here("data"))

# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# add Great Tit
alpha[nrow(alpha) + 1,] <- c(NA, "GTIT", NA, "Great Tit", "Parus major", "GRETIT", NA)

# load in bootstrapped confidence intervals and join to alpha 
# this file has the columns COMMON.NAME, Mean, LowerCI, UpperCI
boots <- read.csv("Change_Estimates_all_May2023good.csv")

# round to 1 digit after the decimal
# unless it's 99.9 then we are leaving it that way to denote not absolute certainty
# make conditional ifelse (where values higher than 99.90 become 99.90 and lower than -99.90 becomes  ##-99.90), and round it to 1 digit decimals.
no100<-function(x){round(ifelse(x>99.90,99.90,ifelse(x<(-99.90),-99.90,x)),1)}
## apply to each columns - 
boots[2:4] <- lapply(boots[2:4],no100)

# arguments for st_read are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- st_read(dsn = "blk", layer = "WbbaBlocks2015_v0_2")
#block_in <- st_read(dsn = "F:\\WBBA_data\\data\\evidence_by_block\\evidence_by_block.shp")

# read in the atlas data; each atlas project will be a list within a list
sp <- list(i = read.delim("WBBA1_ATLASWI_plussensitivepluszerocount_onlycodeds_nospuhslash_EBDMar2023_GOOD_082823.txt", quote = ""),
           ii = read.delim("WBBA2_ATLASWI_plussensitivepluszerocount_onlycodeds_nospuhslash_EBDMar2023_GOOD_082823.txt", quote = "")) %>%
  ## only keep the pertinent columns
  map(dplyr::select,
      category, common_name, breeding_code, breeding_category, 
      block_id = atlas_block, approved)

glimpse(sp) # see an overview of the dataset



#this works to add periods to column names
sp <- lapply(sp, function(y) {colnames(y) <- gsub("\\_", "\\.", colnames(y)); y})
# this works to capitalize column names
sp <- lapply(sp, function (x) dplyr::rename_with(x, toupper))


# Rename BLOCK.ID to BLOCK_ID in both i and ii data frames to standardize name
sp <- map(sp, rename, BLOCK_ID = BLOCK.ID)

# optional county layer --  only used for map printing
cnty <- us_counties(resolution = "high", states = "WI")

#this one just sizes things properly
ecoland <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                   "ecoshape_smooth100")

# load the blank block outlines to show which blocks are fair comparison
fair <- st_read(dsn = "fair", layer = "Fair_comparison_blocks_final") %>% 
  # select the columns you need for mapping
  dplyr::select(BLOCK_ID,
         BLOCK_NAME,
         BLOCK_STAT)

# data prep ----

# remove not valid (reason = exotic) records
sp <- map(sp, subset, APPROVED != "0")

# remove uncoded records
nocodes <- c("F", "", "NA", "NC")
sp <- map(sp, subset, !(BREEDING.CODE %in% nocodes))

# filter out species that don't meet requirements
filtered_sp <- read.csv("filtered_out_species_wibba.csv")

# check if any common names don't match names in the dataset
## this will print a list of species in filtered_sp that don't match species
## in sp.
filtered_sp[which(!filtered_sp[, "COMMON.NAME"] %in% 
                    unlist(map(sp, distinct, COMMON.NAME))), 
            "COMMON.NAME"]

# print the reasons for each group of species in filtered_sp
unique(filtered_sp$REASON)

# if you want to filter them one by one:
## to view which rows are being removed, remove the exclamation mark in front
## of COMMON.NAME and don't assign it to an object eg
## map(sp, subset, COMMON.NAME %in% 
## filtered_sp[which(filtered_sp[, "REASON"] == "for wisconsin"), 
##             "COMMON.NAME"])



#load crosswalk file
cw <- read.csv("filerenamecrosswalk2.csv")

#cut down to common name and breeding evidence
cw2 <- dplyr::select(cw, common_name, ChangeMap)

#rename column
names(cw2)[names(cw2) == 'common_name'] <- 'COMMON.NAME'

#the inner join merges with just the final atlas species, dropping non-breeders
#joining both sub dataframes in the sp list
sp$i <- inner_join(sp$i, cw2, by = "COMMON.NAME")
sp$ii <- inner_join(sp$ii, cw2, by = "COMMON.NAME")

# FOR WISCONSIN
#sp <- map(sp, subset, !COMMON.NAME %in% 
#            filtered_sp[which(filtered_sp[, "REASON"] == "obsposs"), 
#                        "COMMON.NAME"])

# remove species with no change map
#sp <- map(sp, subset, !COMMON.NAME %in% 
#            filtered_sp[which(filtered_sp[, "REASON"] == "no change map"), 
#                        "COMMON.NAME"])

# remove species where taxa category causing trouble - need to fix this somehow
#sp <- map(sp, subset, !COMMON.NAME %in% 
#            filtered_sp[which(filtered_sp[, "REASON"] == 
#                                "taxa category causing trouble"), 
#                        "COMMON.NAME"])

#fix problem species
# Change "American Coot (Red-shielded)" to "American Coot" in sp$i
sp$i <- sp$i %>%
  mutate(COMMON.NAME = str_replace(COMMON.NAME, "American Coot \\(Red-shielded\\)", "American Coot"))
# Change "American Coot (Red-shielded)" to "American Coot" in sp$ii
sp$ii <- sp$ii %>%
  mutate(COMMON.NAME = str_replace(COMMON.NAME, "American Coot \\(Red-shielded\\)", "American Coot"))
# Change "American Coot (Red-shielded)" to "American Coot" in sp$i
sp$i <- sp$i %>%
  mutate(COMMON.NAME = str_replace(COMMON.NAME, "Rock Pigeon \\(Feral\\)", "Rock Pigeon"))
#Change "American Coot (Red-shielded)" to "American Coot" in sp$ii
sp$ii <- sp$ii %>%
mutate(COMMON.NAME = str_replace(COMMON.NAME, "Rock Pigeon \\(Feral\\)", "Rock Pigeon"))

# remove sensitive species - figure out what to do with these later - either 
# index only or no index I think, describe in text for some.
#sp <- map(sp, subset, !COMMON.NAME %in% 
#            filtered_sp[which(filtered_sp[, "REASON"] == 
#                                "sensitive species"), 
#                        "COMMON.NAME"])

# remove species with less than ten blocks where we will not calculate 
# index - will need to run separately).
#sp <- map(sp, subset, COMMON.NAME %in% 
#            filtered_sp[which(filtered_sp[, "REASON"] == 
#                                "fewer than ten blocks"), 
#                        "COMMON.NAME"])

# if you want to filter them all at once:
#sp <- map(sp, subset, !COMMON.NAME %in% filtered_sp[, "COMMON.NAME"])

# to filter for a particular set of species (instead of filtering 
# them out):
## edit keep_birds to include the species you're interested in
keep_birds <- c("Rock Pigeon",
                "American Coot")
## then filter to just those species
sp <- map(sp, subset, COMMON.NAME %in% keep_birds)

# flag the pigeon entries so they are not removed with the rest of the domestics
#sp <- map(sp, transform, 
#          CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))

# update_sp removes non-species taxa (i.e., hybrid, spuh, domestic, and slash 
# taxonomic categories), removes records with no breeding evidence (i.e., F, 
# NA, and the empty string), and adds an alpha code (for column naming).
#taxa <- c("species", "issf", "form", "pigeon")

update_sp <- function(x) {
  is_sp <- x$CATEGORY %in% taxa
  
  is_obs <- is.na(x$BREEDING.CODE) | x$BREEDING.CODE %in% c("F ", "NC", "") 
  
  keep <- is_sp & ! is_obs
  
  x <- x[keep, ]
  
  x
}

sp <- map(sp, update_sp)

if(!setequal(unique(sp$i$COMMON.NAME), unique(sp$ii$COMMON.NAME))) {
  sp <- map(sp, full_join, 
            data.frame(COMMON.NAME = union(unique(sp$i$COMMON.NAME),
                                           unique(sp$ii$COMMON.NAME))))
}

# check the species sets to make sure they're still as expected
if(!setequal(unique(sp$i$COMMON.NAME), unique(sp$ii$COMMON.NAME))) {
  warning("Species have not been matched properly")
}

# this version sneaks the bootstrap mean and CI onto the species
sp <- map(sp, left_join, dplyr::select(alpha, 
                                COMMON.NAME = COMMONNAME, SPEC))

# no alpha code exists for Great Tit, so have to add one; GTIT does not 
# conflict with existing codes.
if(any(union(unique(sp$i$COMMON.NAME),
             unique(sp$ii$COMMON.NAME)) %in% "Great Tit")) {
  sp$i$SPEC[sp$i$COMMON.NAME == "Great Tit"] <- "GTIT"
  sp$ii$SPEC[sp$ii$COMMON.NAME == "Great Tit"] <- "GTIT"
}

# check that no common names in sp were unmatched in alpha
# should return FALSE
if (any(vapply(sp, function(x) anyNA(x$SPEC), NA))) {
  warning("Common names and alpha codes did not match as expected")
}  

# set all NAs in species columns to 0 (ie no breeding evidence in that block)
set0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

sp_vec <- sp %>% 
  map(dplyr::select, SPEC) %>%
  unlist() %>%
  unique()

# no alpha code exists for Great Tit, so have to add one; GTIT does not 
# conflict with existing codes.
if(any(union(unique(sp$i$COMMON.NAME),
             unique(sp$ii$COMMON.NAME)) %in% "Great Tit")) {
  sp$i$SPEC[sp$i$COMMON.NAME == "Great Tit"] <- "GTIT"
  sp$ii$SPEC[sp$ii$COMMON.NAME == "Great Tit"] <- "GTIT"
}

# Create a list of data frames with BLOCK_ID as the 1st column, followed
# by columns for each alpha code, with either a 1 (WBBA I) or 2 (WBBA II)
# indicating some breeding evidence, and 0 indicating no breeding evidence.  
sp_cast <- sp %>%
  # create a column with the list name (ie, i, ii)
  imap(~mutate(.x, project = .y)) %>%
  # create a column with the breeding evidence (ie 1, 2, 0)
  map(mutate, BREEDING = case_when(
    project == "i" & !is.na(BREEDING.CODE) ~ 1,
    project == "ii" & !is.na(BREEDING.CODE) ~ 2,
    is.na(BREEDING.CODE) ~ 0
  )) %>%
  # merge the two lists together
  bind_rows() %>%
  # keep one row for each species in each block in each project
  distinct(BLOCK_ID, SPEC, BREEDING, project) %>%
  # 'cast' each list
  group_by(project) %>%
  pivot_wider(names_from = SPEC, values_from = BREEDING) %>%
  # merge 'fair' column onto sp, and use a right_join to keep only rows 
  # that match with y (ie keep only fair column blocks)
  right_join(fair, by = "BLOCK_ID") %>%
  # change NA values to 0 so they can be summed
  mutate(across(all_of(sp_vec), set0)) %>%
  group_by(BLOCK_ID) %>%
  # add the values for each block to get the breeding evidence code, where
  # 0 == "Unreported", 1 == "WBBA I only", 2 == "WBBA2 only", 3 == "Both".
  mutate(across(all_of(sp_vec), sum), 
         project = NULL) %>%
  ungroup() %>%
  # get rid of duplicate rows
  distinct() %>%
  # make the dataset a spatial feature and set the crs to NAD83 (4269)
  st_as_sf(sf_column_name = "geometry", crs = 4269) 

# returns count of unmatched records if any
if(anyNA(sp_cast$BLOCK_ID)) {
  warning(paste(sum(is.na(sp_cast$BLOCK_ID)), "unmatched blocks"))
}

# order the columns and rows
cols_to_keep <- c("BLOCK_ID", "BLOCK_NAME", "BLOCK_STAT")

ord <- c(cols_to_keep,
         sort(setdiff(names(sp_cast), c(cols_to_keep, "geometry"))), 
         "geometry")

sp_cast <- sp_cast[, ord] %>%
  arrange(BLOCK_ID)

# check the data structure
glimpse(sp_cast)

# check if any NAs still exist in data
if(any(is.na(sp_cast))) {
  warning("NAs present in data")
}

# check for duplicate blocks
if(any(duplicated(sp_cast$BLOCK_ID) | duplicated(sp_cast$BLOCK_NAME))) {
  warning("Duplicate block IDs in dataset")
}

# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming





#R package, and us_counties to get the state outline
## load projected abundance map rasters for 109 species
## load county boundary polygon, lake polygons for mapping
#lakes<-st_read("F:\\WBBA_data\\pred_maps\\large_lakes24_hydro.shp")
#counties1<-st_read("F:\\WBBA_data\\Counties_WTM.shp")
## load fonts and color palettes
species<-sp_vec
# print only a subset for testing
#species<-c("BADO","HOWR", "BTNW", "BBWO")

bestmapimg<-list()

labels <- c("Unreported", "WBBA I only", "WBBA II only", "Both")

sp_cast <- modify_if(sp_cast, names(sp_cast) %in% species,
                         factor, levels = 0:3, labels = labels)

line_gray <- "#666666"
#         not det  at1    at2      both
# pal <- c("white", "red", "green", "blue")
pal <- c("gray90", "#ff9405",  "#57cfff", "#595900") # off-white orange blue dark
linecol <- c(NA, NA,  NA, NA)
#this was the outline version for blank comparable blocks# linecol <- c("#666666", NA,  NA, NA)

for(j in 1:length(species)){
  ## transform rasters into SpatialPixelsDataFrame
  plot_blocks <- sp_cast[species[j]]
  # make evidence a factor and choose factor order -- used to order map legend
  plot_block_df <- as.data.frame(plot_blocks)
  names(plot_block_df) <- c("value","geometry")
  ## set bins for categroizing color scale of block level occurrence
  plot_block_df<-plot_block_df%>%st_as_sf()
  ## block level occurrences are binned to <1 (no observation), 1-2 (low observation),
  ## and any above 2 (many repeated observations across time and space within a block)
  ## model prediction maps
  plot_each<-ggplot() + 
    #invisible but standardizes size
    geom_sf(data=ecoland, fill=NA, color=NA, size=0) +
    geom_sf(data=block_in, fill=NA, color=NA, size=0) +
    coord_sf (datum = sf::st_crs(4269)) +
    #geom_raster(data=plot_each_df, aes(x=x, y=y, fill=bins,color=bins)) + 
    #geom_sf(data=lakes, fill="#BFE1F4", color=NA) +  
    geom_sf(data=cnty, fill=NA, color="gray60", size=0.6) +
    geom_sf(data=plot_block_df, aes(fill=value, color=value)) +  
    coord_sf()+
    #ggtitle(paste(species[j],": best _ quant"))+
    theme_map()+
    theme(legend.position="none") +
    theme(plot.margin = unit(c(-0.15,-0.15,-0.15,-0.15), "in")) +
    scale_fill_manual(labels=labels,
                  values=pal,
                 na.translate = F)+
  scale_color_manual(labels=labels,
                    values=linecol,
                    na.translate = F)
    #bestmapimg[[species[j]]]<-list(overlay_map,plot_each)
  bestmapimg[[species[j]]]<-list(plot_each)
}

# Create a lookup table for naming

species_lookup <- sp$ii %>%
  dplyr::distinct(SPEC, .keep_all = TRUE) %>%
  dplyr::select(SPEC, COMMON.NAME, ChangeMap)


species ## species index
#sp<-species[1:2]
#overlay_legend<-get_legend(bestmapimg$ALFL[1]) ## extract legend 
best_composed_img<-list()## make blank list for for-loop

#### for loop for ggarrange and saving into folder directory
for(l in 1:length(species)){
  ## make ggarrange 
  #path<-"C:\\review_maps\\2025_blocks_change"
  
# Find the row in the species_lookup table
  
species_info <- species_lookup[species_lookup$SPEC == species[l], ]
  
# Construct the new filename
filename_new <- paste0(species_info$ChangeMap, "_", species_info$COMMON.NAME, ".eps")
  
  ggsave(filename=filename_new,plot=bestmapimg[[species[l]]][[1]],  
       colormodel = "cmyk",
       device = "eps",
       scale = 1,  width = 3.75,  height = 3.9469,  units = c("in"),
       dpi = 600, 
       limitsize = TRUE,  bg = "white")
}
