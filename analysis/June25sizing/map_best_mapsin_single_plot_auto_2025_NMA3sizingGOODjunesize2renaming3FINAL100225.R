
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

## Load data and set up working drive
#setwd("F:\\WBBA_data") # set working drive
library(USAboundaries) 

#R package, and us_counties to get the state outline
## load projected abundance map rasters for 109 species
best_quant_mods109<-brick("best_quant_mods114_sum_eval.grd", format="raster")

## load county boundary polygon, lake polygons for mapping
lakes<-st_read("large_lakes24_hydro.shp")
counties1<- us_counties(resolution = "high", states = "WI")

#load block file for sizing
block_in <- st_read(dsn = "blk", layer = "WbbaBlocks2015_v0_2")

# optional ecological landscapes layer
#this one just sizes things properly
ecoland <- st_read("Ecological_Landscapes_of_Wisconsin_100",
                   "ecoshape_smooth100")

## atlas block shapefile with cleaned block-level occurrence data for all species
#blocks<-st_read("blocks_2023_6_5.shp")
## model predictive evaluation data 
## this used ebird checklists with travel less than 3 km and duration longer than 11 minutes
#load(file="evaluation_dat.rdata")

## load fonts and color pallettes
#font_import()
#loadfonts(device = "win")
paletteer_d("palettesForR::Gold")

#best_quant_mods109<-best_quant_mods109[[names(best_quant_mods109)[!names(best_quant_mods109)%in%watrap]]]
#best_quant_mods109<-best_quant_mods109[[names(best_quant_mods109)[!names(best_quant_mods109)%in%lingers]]]
names(best_quant_mods109)

### remove values that are below 1 and replace it with NA in the block data
removelow<-function(x){ifelse(x<1,NA,x)}
#blocks<-data.frame(blocks)
#blocks[4:199]<-apply(data.frame(blocks[4:199]),2,removelow)
#blocks<-st_as_sf(blocks)
species<-names(best_quant_mods109)


bestmapimg<-list()

for(j in 1:length(species)){
  ## transform rasters into SpatialPixelsDataFrame
  plot_spdf <- as(best_quant_mods109[[species[j]]], "SpatialPixelsDataFrame")
  plot_df <- as.data.frame(plot_spdf)
  plot_each_df<-plot_df%>%dplyr::select(species[j],x,y)%>%rename(value=species[j])
  plot_each_df<-plot_each_df%>%mutate(bins=cut(value,breaks=c(1,2,5,10,20, 3000))) ## note the bins for prediction
  
  #plot_blocks <- blocks[species[j]]
  #plot_block_df <- as.data.frame(plot_blocks)
  #names(plot_block_df) <- c("value","geometry")
  ## set bins for categroizing color scale of block level occurrence
  #plot_block_df<-plot_block_df%>%mutate(pa=cut(value,breaks=c(0.999,2.0001,10^9)))%>%st_as_sf()
  ## block level occurrences are binned to <1 (no observation), 1-2 (low observation),
  ## and any above 2 (many repeated observations across time and space within a block)
  ## model prediction maps
  plot_each<-ggplot() +
    #invisible but standardizes size
    geom_sf(data=ecoland, fill=NA, color=NA, size=0) +
    geom_sf(data=block_in, fill=NA, color=NA, size=0) +
    coord_sf (datum = sf::st_crs(4269)) +
    #main colors
      geom_tile(data=plot_each_df, aes(x=x, y=y, fill=bins,color=bins)) + 
    geom_sf(data=lakes, fill="#BFE1F4", color=NA) +  
    geom_sf(data=counties1, fill=NA, color="gray60", size=0.6) +
    #ggtitle(paste(species[j],": best _ quant"))+
      theme_map()+
      theme(legend.position="none") +
      theme(plot.margin = unit(c(-0.15,-0.15,-0.15,-0.15), "in")) +
      #theme(legend.title = element_text(family="mono",face="bold",size=11),
       #     legend.text = element_text(family="mono",face="bold",size=8),
        #    legend.spacing.x = unit(0, 'cm'),
         #   legend.key.width=unit(0.9, "cm"),
          #  legend.key.height =unit(0.5, "cm"))+
    scale_fill_manual(#name=paste0(" birds/km",expression("^2")),
                  #labels=c(" <2 "," 2~5 "," 5~10 "," 10~20 "," >20 "),
                   values=c("#fbcd54","#d2985c","#c18953","#a85f3b","#651312"),
                    na.translate = F)+
    scale_color_manual(#name=paste0(" birds/km",expression("^2")),
                     #labels=c(" <2 "," 2~5 "," 5~10 "," 10~20 "," >20 "),
                    values=c("#fbcd54","#d2985c","#c18953","#a85f3b","#651312"),
                   na.translate = F)
    bestmapimg[[species[j]]]<-list(plot_each)
}

sp<-species ## species index
#sp<-species[1:2]
#overlay_legend<-get_legend(bestmapimg$ALFL[1]) ## extract legend 
best_composed_img<-list()## make blank list for for-loop


#filenames for printing
#load crosswalk file
cw <- read.csv("filerenamecrosswalk3.csv")


# sp from a character object to a dataframe
spdf <- as.data.frame(sp)
names(spdf)[names(spdf) == 'sp'] <- 'X4lettercode2023'

#join
namedf <- inner_join(spdf, cw, by = "X4lettercode2023")

# columns to paste together
cols <- c( 'EstimatedDensity' , 'common_name')

# create a new column with the columns collapsed together
namedf$filename <- apply( namedf[ , cols ] , 1 , paste , collapse = "_" )

#back to just 2 columns
sp <- namedf %>% dplyr::select(c(X4lettercode2023, filename))
#sp <- namedf %>% select(filename)

#put name back
names(sp)[names(sp) == 'X4lettercode2023'] <- 'sp'


#### for loop for ggarrange and saving into folder directory
for(l in 1:nrow(sp)){
  current_species_code <- sp$sp[l]
  filename <- sp$filename[l]
  
  # Get the plot from the bestmapimg list using the species code as an index
  current_plot <- bestmapimg[[current_species_code]][[1]]
  
  # Save the plot with the correct filename
  ggsave(filename = paste0(filename, ".pdf"),
         colormodel = "cmyk",
         plot = current_plot,
         device = "pdf", 
         scale = 1,  width = 3.75,  height = 3.9469,  units = c("in"),
         dpi = 600, 
         limitsize = TRUE,  bg = "white")
}




