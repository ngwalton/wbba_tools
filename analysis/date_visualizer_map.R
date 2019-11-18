
#Date Visualizer Map 
# Displays eBird records with different colors denoting a different date

# Author: Nicholas Anich
# Created: 7 Oct 2019
# Really really crude to start with!

library(ggmap)
library (ggplot2) 
library (lubridate)

#load data
pelican <- read.delim("ebd_US-WI_amwpel_201501_201608_relAug-2016.txt", sep="\t", header=TRUE, quote = "", stringsAsFactors = FALSE, na.strings=c(""))

#define map boundary box
wi_bbox <- make_bbox(lat = LATITUDE, lon = LONGITUDE, data = pelican)
wi_bbox

#for the moment, background is google terrain - would be fine with just making this wi counties perhaps or giving people a choice
big <- get_map(location = wi_bbox, source = "google", maptype = "terrain")

# optional county layer --  only used for map printing I want this on here but can't figure out how
#requires  
#library(tmap)
#library(USAboundaries) which may not place nice with this ggmap, so perhaps go a different route?
#cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# new date column (not sure if this steip is needed?)
pelican$DDDD <- format( as.Date(pelican$OBSERVATION.DATE), format="%y/%m/%d")

#format date as julian (requires lubridate) - not sure if Julian is the way to go
x = as.Date(pelican$DDDD)
pelican$juliandate <- yday(x)

#plots map - this works without as.factor too (probably what should happen to it), but the colors suck there and I can't figure out how to tweak colors
ggmap(big) +
  geom_point(data = pelican, mapping = aes(x = LONGITUDE, y = LATITUDE, color =as.factor(juliandate))) 


##Okay here's what I know so far - I want a map that plots different dates in different colors - this basically does that but it's not what I would call slick

## Wish list/Issues:
# 1. Would probably be nicer if the background on this was just Wisconsin counties, which could be done with using 
#library(tmap)
#library(USAboundaries) 
# 2. We should have the ability to adjust and specify the start dates and end dates perhaps by species seems like a necessary feature, for many species we
#are going to be interested in a narrow window - want to visually see the difference between end of May and beginning of June
#and the current pallette does not do that well. It's possible just a couple color pallette options would improve this situation. 
#Maybe dates for a species could be autoset based on dates of occurrence in file.
# 3. At the moment the legend is gigantic and not so useful as it just reports Julian date. Perhaps there's a more elegant way
#to code date than I have here - refer back to the chron plot for ideas? Note we will be wanting to run 5 years together
#which can be a pain depending how dates are handled.
# 4. Ideally, records coded with a different breeding status (poss, prob, conf) could get a different dot shape (triangle, square, circle, maybe an X for uncoded)
#5. Need to automate the production of these, should be labeled with species name, ideally some sort of minimally useful legend

