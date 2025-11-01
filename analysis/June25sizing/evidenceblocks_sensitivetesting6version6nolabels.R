
library(tidyverse)
library(sf)
library(ggplot2)
library(tigris) # For centroids
library(USAboundaries) # For county boundaries
library(ggthemes)
#library(extrafont)

# fonts
# TrueType fonts from current working directory
#font_import(pattern = "ScalaSans", prompt = FALSE)

# register fonts with R's PDF and PostScript - needed only once
#loadfonts(device = "win")
#loadfonts(device = "pdf")

# should list ScalaSans
#fonts()

# --- Load sensitive species data ---
# Your CSV should have "COMMON.NAME", "County", "RealColor", and "Label_Text"
sensitive <- read.csv("SensitiveforEvidenceMap1.csv")

# --- Get Wisconsin county boundaries from USAboundaries ---
cnty <- us_counties(resolution = "high", states = "WI")

#delete duplicate state name column
cnty <- cnty[, -13] 

# Rename the 'name' column to 'County' in cnty
cnty <- cnty %>%
  select(County = name, everything())

#these two just size things properly
ecoland <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                   "ecoshape_smooth100")
block_in <- st_read("blk", "WbbaBlocks2015_v0_2")

#this one looks nice
ecoland2 <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                    "EL_Simp")

# this won't plot but it helps not break code
sp_in <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)

#use same crs
# create a sf data.frame from "sp_in"
wgs84 <- st_crs(4269)
sp_wgs <- st_as_sf(sp_in, crs = wgs84, coords = c("LONGITUDE", "LATITUDE"))

# transform projection to match blocks
sp_nad <- st_transform(sp_wgs, st_crs(block_in))

# --- Get the unique species to loop through ---
unique_species <- unique(sensitive$COMMON.NAME)

# --- Initialize empty lists to store plots ---
plot_list <- list()

# --- Loop through each species to generate plots ---
for (species_name in unique_species) {
  # Filter sensitive data for the current species
  sensitive_species_data <- sensitive %>%
    filter(COMMON.NAME == species_name)
  
  # Join county data with the filtered species data
  #    Ensure this join is a left_join so all counties are included
  #    The sensitive data only applies to some counties.
  cnty_with_labels <- cnty %>%
    left_join(sensitive_species_data, by = "County")
  
  #Now filter the joined data
  labels_data <- cnty_with_labels %>%
    dplyr::filter(!is.na(Label_Text))
  

  #set up ggplot
  p<-ggplot() +  
    #these two are invisible but standardizes size
    geom_sf(data=ecoland, fill=NA, color=NA, size=0) +
    geom_sf(data=block_in, fill=NA, color=NA, size=0) +
    coord_sf (datum = sf::st_crs(4269)) +
    geom_sf(data=ecoland2, fill=NA, color="#87d979", linetype ="dashed", linewidth=0.2) +
    geom_sf(data = cnty_with_labels, 
            aes(fill = RealColor), 
            color = "gray60", size = 0.6) +
        coord_sf()+
#   geom_sf_text(data = labels_data, 
#                 aes(label = Label_Text), 
#                 size = 1.5, 
#                 family = "ScalaSans",
#                 color = "white") +
    theme_map()+
    theme(legend.position="none") +
    theme(plot.margin = unit(c(-0.15,-0.15,-0.15,-0.15), "in")) +
    scale_fill_identity(na.value = NA)
   # Add the plot to the list
  plot_list[[species_name]] <- p
}

# Get the list of species names from the named plot_list
species_names <- names(plot_list)

# Loop through each species name to save the corresponding plot
for (species_name in species_names) {
  # Get the plot object for the current species
  p <- plot_list[[species_name]]
  
  # Construct a filename using the species name
  filename <- paste0("species_", species_name, ".eps")

  ggsave(filename=filename,
         plot=p,  
       colormodel = "cmyk",
       device = "eps",
       scale = 1,  width = 3.75,  height = 3.9469,  units = c("in"),
       dpi = 600, 
       limitsize = FALSE,  bg = "white")
  
  # 2. Embed the fonts in the saved EPS file using Ghostscript
#  tryCatch(
#    expr = {
#      extrafont::embed_fonts(filename, outfile = filename)
#    },
#    error = function(e) {
#      warning(paste("Could not embed fonts for", filename, ":", e$message))
#    }
#  )
}




