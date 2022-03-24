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

# this version uses headers like "common_name" instead of "COMMON.NAME"

library(sf)       # for mapping and handling shp files
library(dplyr)    # if you want to use tidyverse functions
library(purrr)    # for working with lists
library(tidyr)    # replacement of reshape2 package
library(foreign)  # for read.dbf (alpha codes come as dbf)
library(tmap)     # only needed for map making
library(USAboundaries) # only needed for map making
library(here)     # for loading files

setwd(here::here("data"))

# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

out_file <- "wbba_change"  # root name for output file (csv and/or shp)

# name of output pdf file if printing maps
out_pdf <- "wbba_change_map_test.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# arguments for st_read are input format dependent;
# with a shapefile, the first argument is the directory containing the shp,
# and the second argument is the name of the shapefile without the extension
block_in <- st_read(dsn = "blk", layer = "WbbaBlocks2015_v0_2")

# read in the atlas data; each atlas project will be a list within a list
sp <- list(i = read.delim("ebird_data_sample_wbbai.txt", quote = ""),
           ii = read.delim("ebird_data_sample_wbbaii.txt", quote = "")) %>%
  ## only keep the pertinent columns
  ## rename the atlas block column so it will match later
  map(select,
      category, common_name, breeding_code, 
      BLOCK_ID = atlas_block, approved)

glimpse(sp) # see an overview of the dataset

# optional county layer --  only used for map printing
cnty <- us_counties(resolution = "high", states = "WI")

# load the blank block outlines to show which blocks are fair comparison
fair <- st_read(dsn = "fair", layer = "faircomparisonblocks") %>% 
  # select the columns you need for mapping
  select(BLOCK_ID,
         BLOCK_NAME,
         BLOCK_STAT)

# data prep ----

# remove not valid (reason = exotic) records
sp <- map(sp, subset, approved != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
sp <- map(sp, transform, 
          category = ifelse(common_name == "Rock Pigeon", "pigeon", category))

# update_sp removes non-species taxa (i.e., hybrid, spuh, domestic, and slash 
# taxonomic categories), removes records with no breeding evidence (i.e., F, 
# NA, and the empty string), and adds an alpha code (for column naming).
taxa <- c("species", "issf", "form", "pigeon")

update_sp <- function(x) {
  is_sp <- x$category %in% taxa
  
  is_obs <- is.na(x$breeding_code) | x$breeding_code %in% c("F ", "NC", "") 
  
  keep <- is_sp & ! is_obs
  
  x <- x[keep, ]
  
  x
}

sp <- map(sp, update_sp)

if(!setequal(unique(sp$i$common_name), unique(sp$ii$common_name))) {
  sp <- map(sp, full_join, 
            data.frame(common_name = union(unique(sp$i$common_name),
                                           unique(sp$ii$common_name))))
}

# check the species sets to make sure they're still as expected
if(!setequal(unique(sp$i$common_name), unique(sp$ii$common_name))) {
  warning("Species have not been matched properly")
}

sp <- map(sp, left_join, select(alpha, 
                                common_name = COMMONNAME, SPEC))

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
  map(select, SPEC) %>%
  unlist() %>%
  unique()

# no alpha code exists for Great Tit, so have to add one; GTIT does not 
# conflict with existing codes.
if(any(union(unique(sp$i$common_name),
             unique(sp$ii$common_name)) %in% "Great Tit")) {
  sp$i$SPEC[sp$i$common_name == "Great Tit"] <- "GTIT"
  sp$ii$SPEC[sp$ii$common_name == "Great Tit"] <- "GTIT"
}

# I had some problem with this field switching off numeric, not sure if this is ongoing issue or not.
# sp$ii$approved <- as.numeric(sp$ii$approved)

# Create a list of data frames with BLOCK_ID as the 1st column, followed
# by columns for each alpha code, with either a 1 (WBBA I) or 2 (WBBA II)
# indicating some breeding evidence, and 0 indicating no breeding evidence.  
sp_cast <- sp %>%
  # create a column with the list name (ie, i, ii)
  imap(~mutate(.x, project = .y)) %>%
  # create a column with the breeding evidence (ie 1, 2, 0)
  map(mutate, BREEDING = case_when(
    project == "i" & !is.na(breeding_code) ~ 1,
    project == "ii" & !is.na(breeding_code) ~ 2,
    is.na(breeding_code) ~ 0
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

if (print_map) {
  block_map <- sp_cast
  
  # make evidence a factor and choose factor order -- used to order map legend
  labels <- c("Unreported", "WBBA I only", "WBBA II only", "Both")
  
  block_map <- modify_if(block_map, names(block_map) %in% sp_vec,
                         factor, levels = 0:3, labels = labels)
  
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
    # this label code sums the blocks for each category - thanks Gabriel Foley
    no_geom_block_map <- st_drop_geometry(block_map) 
    
    # reassign the label object so that counts aren't cumulatively pasted
    labels <- c("Unreported", "WBBA I only", "WBBA II only", "Both")
    
      for(j in seq_along(labels)) {
        counts <- NA_integer_
        counts[j] <- if(is.na(sum(no_geom_block_map[species] == labels[j]))) {
          0
        } else {
          sum(no_geom_block_map[species] == labels[j])
        }
        labels[j] <- paste0(labels[j], " (", counts[j], ")")
      }
    
    out_map <- tm_shape(block_map) +
      tm_polygons(species, 
                  border.col = NULL, palette = pal, legend.show = FALSE) +
      tm_shape(fair) +
      tm_borders("black", lwd = 0.2)  +
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
write.csv(st_drop_geometry(block_map), 
          file = paste0(out_file, ".csv"), row.names = FALSE)

# optionally write to shp
st_write(block_map, ".", out_file, driver = "ESRI Shapefile")
