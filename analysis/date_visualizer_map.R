# Work in progress...

# Script to produce maps of species by month with to visualize the progression
# of observations. Observations within a given month are colored to indicate
# which quartile they were observed in.

# working notes:
# group by week
# Jitter
# ALFL
# Wide page with all 12
# try both county and filer regions
# no blocks
#
# also need:
# sim block map
# just jun/july
# n obs
# func


library(rgdal)
library(reshape2)
library(foreign)
library(tmap) # only needed for map making
library(USAboundaries) # only needed for map making
library(lubridate)
library(rvest)  # to find most up to data eBird taxonomy

setwd(here::here("data"))


# set to FALSE to suppress printing pdf of each species -- printing maps can be
# time consuming
print_map <- TRUE


# output files ----

# name of output pdf file if printing maps
out_pdf <- "date_visualizer.pdf"


# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
# source: http://www.birdpop.org/pages/birdSpeciesCodes.php
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)

# block shapefile; arguments for readOGR are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")

# eBird filter
fltr <- readOGR("ebirdfilters20170817.kml", "ebirdfilters20170817")

# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")

# sample WBBA data from ebird
sp_in <- read.delim("eBirdDataSampleWIAtlasII.txt", as.is = TRUE)
# sp_in <- read.delim("wiatlas2samplespecies3.txt", as.is = TRUE, quote = "")

# get most resent eBird taxonomy -- link me need to be updated
url <- "https://www.birds.cornell.edu/clementschecklist/download/"
pg <- read_html(url)
pg_urls <- html_attr(html_nodes(pg, "a"), "href")
tax_url <- grep("eBird_Taxonomy.+\\.csv$", pg_urls, value = TRUE)

if (! length(tax_url) == 1) {
  message(paste("There is an issue with tax_link: tax_link has length", length(tax_url)))
} else {
  ebird_taxa <- read.csv(tax_url, as.is = TRUE)
}

if ("ï..TAXON_ORDER" %in% names(ebird_taxa)) {
  names(ebird_taxa)[names(ebird_taxa) == "ï..TAXON_ORDER"] <- "TAXON_ORDER"
}

# data prep ----

# remove hybrid, spuh, and slash taxonomic categories
taxa <- c("species", "issf", "domestic", "form")
sp_in <- sp_in[sp_in$CATEGORY %in% taxa, ]

# add alpha codes needed later to name species columns with < 10 chars required
# for shapefile
sp_in <- merge(sp_in, alpha[, c("COMMONNAME", "SPEC")], by.x = "COMMON.NAME",
               by.y = "COMMONNAME", all.x = TRUE, all.y = FALSE)

# if any species were unmatched in alpha, this will print there names; this will
# require aditional attention if any are not matched
if (any(is.na(sp_in$SPEC))) {
  unique(sp_in$COMMON.NAME[is.na(sp_in$SPEC)])
}

# this will need modification depending on what species were not matched; in
# this case we provide a custom alpha code domestic Guineafowl and Mallard, and
# remove Domestic goose sp.
sp_in$SPEC[sp_in$COMMON.NAME == "Helmeted Guineafowl (Domestic type)"] <- "HEGU"
sp_in$SPEC[sp_in$COMMON.NAME == "Mallard (Domestic type)"] <- "MALL_DOM"
sp_in <- sp_in[sp_in$COMMON.NAME != "Domestic goose sp. (Domestic type)", ]

# add family from eBird taxonomy
cols <- c("PRIMARY_COM_NAME", "FAMILY")
sp_in <- merge(sp_in, ebird_taxa[, cols], by.x = "COMMON.NAME",
               by.y = "PRIMARY_COM_NAME", all.x = TRUE)

# order taxonomically
sp_in <- sp_in[order(sp_in$TAXONOMIC.ORDER), ]

# create a SpatialPointsDataFrame from "sp_in"
wgs84 <- CRS("+init=epsg:4326")  # use WGS84 as input CRS
sp_wgs <- SpatialPointsDataFrame(sp_in[, c("LONGITUDE", "LATITUDE")], sp_in,
                                 coords.nrs = c(23, 22), proj4string = wgs84)

# transform projection to match blocks
nad83 <- CRS(proj4string(block_in))  # use NAD83 from block_in
sp_nad <- spTransform(sp_wgs, nad83)

# extract blocks that overlay points; returns a data frame containing the same
# number rows as sp_nad; each row is a record from block that overlays the
# points in sp_nad
block_over <- over(sp_nad, block_in)
names(block_over)[13] <- "CO_eBird"  # COUNTY is in both data frames

# ...and join them to the bird data frame
sp_nad@data <- cbind(sp_nad@data, block_over)

sp <- sp_nad

# some of the BREEDING.BIRD.ATLAS.CODE codes have a space at the end
# and some don't - this removes the space
sp$BREEDING.BIRD.ATLAS.CODE <- trimws(sp$BREEDING.BIRD.ATLAS.CODE)

# add date columns
sp$DDDD <- as.Date(sp$OBSERVATION.DATE, format="%m/%d/%Y")
# sp$DDDD <- date(sp$OBSERVATION.DATE)

#format date as julian (requires lubridate) - not sure if Julian is the way to go
sp$juliandate <- yday(sp$DDDD)
sp$juliandate <- factor(sp$juliandate, levels = sort(unique(sp$juliandate)))
sp$month <- month(sp$DDDD)
month_levels <- unique(data.frame(num = sp$month, lab = month.name[sp$month]))
month_levels <- month_levels[order(month_levels$num), ]
sp$month <- factor(sp$month, levels = month_levels$num, labels = month_levels$lab)

# function to find quartile of day within in month of given year
mo_quartile <- function(d) {
  breaks <- quantile(1:days_in_month(d))
  out <- findInterval(day(d), breaks, rightmost.closed = TRUE)
  out
}
sp$quartile <- vapply(seq_along(sp$DDDD), function(i) mo_quartile(sp$DDDD[i]), NA_real_)
sp$quartile <- factor(sp$quartile, levels = 1:4, labels = paste0("q", 1:4))

# sp$BREEDING.BIRD.ATLAS.CODE <- factor(sp$BREEDING.BIRD.ATLAS.CODE,
#                                       levels = unique(sp$BREEDING.BIRD.ATLAS.CODE))
sp$BREEDING.BIRD.ATLAS.CATEGORY[sp$BREEDING.BIRD.ATLAS.CATEGORY == ""] <- "C1"
sp$BREEDING.BIRD.ATLAS.CATEGORY <- factor(sp$BREEDING.BIRD.ATLAS.CATEGORY,
                                          levels = sort(unique(sp$BREEDING.BIRD.ATLAS.CATEGORY)))

# print maps ----

# print an evidence map for each species to a single pdf; note that this can be
# time consuming

if (print_map) {
  # block_map <- block_out
  # no_rep <- "No checklists"
  # not_rep <- "Not reported"
  # block_map@data[is.na(block_map@data)] <- no_rep
  # block_map@data[block_map@data == ""] <- not_rep

  # make evidence a factor and choose factor order -- used to order map legend
  sp_vec <- unique(sp$SPEC)
  # ord <- c(rev(vapply(breeding_codes, "[[", NA_character_, 2)), not_rep, no_rep)
  # block_map@data[, sp_vec] <- lapply(sp_vec, function(x)
  #   factor(block_map@data[[x]], levels = ord))

  line_gray <- "#4e4e4e"
  # pal <- c("black", "#820BBB", "#BF5FFF", "#e6cef1", "#e5e5e5", "white")
  # pal <- c("#2d03ff","#d5ff03", "#03ff2d", "#ff03d5")
  # pal <- c("#E53F00", "#00CB38", "#0089BF", "brown")
  pal <- c("#E53F00", "#98583F", "#00CB38", "#0089BF")
  shapes <- c(3, 4, 2, 1)
  n <- length(sp_vec)

  # open pdf device
  n_plots <- 12
  width = 7 * n_plots
  height = 7
  pdf(out_pdf, width = width, height = height)

  for (i in seq_along(sp_vec)) {
    # if (i == 1) {
    #   message(paste("Printing", n, "maps"))
    #   t0 <- Sys.time()
    # }

    species <- sp_vec[i]

    # current <- sp[sp$SPEC == species & month(sp$DDDD) == mo, ]
    # if (nrow(current) == 0) {
    #   next
    # }
    current <- sp[sp$SPEC == species, ]
    # current$juliandate <- droplevels(current$juliandate)
    # out <- tm_shape(block_map) +
    #   tm_polygons(species, title = "Evidence", border.col = NULL, palette = pal) +
    #   tm_shape(cnty) +
    #   tm_polygons(border.col = line_gray, alpha = 0, border.alpha = 0.4,
    #               legend.show = FALSE) +
    #   tm_legend(title = species, position = c("left", "bottom"), bg.alpha = 0,
    #             main.title.fontface = 2, title.fontface = 2)

    # out <- tm_shape(block_in) +
    #   tm_polygons(title = "Evidence", border.col = "black", legend.show = FALSE) +

    # m_title <- paste(species, month.name[mo], sep = ": ")
    m_title <- alpha[alpha$SPEC == species, "COMMONNAME"]
    jit <- 0.08

    out <-  tm_shape(cnty) +
      tm_polygons(border.col = line_gray, alpha = 0, border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_shape(current) +
      ### shape only
      # tm_symbols(size = 0.1, shape = "BREEDING.BIRD.ATLAS.CATEGORY",
      #            shapes = shapes, title.shape = "Code", alpha = 0.6,
      #            jitter = jit) +
      ### color only
      # tm_dots("quartile", size = 0.1, title = "Day quartile", pal = pal,
      #         jitter = jit, alpha = 0.6, shape = 1) +
      ### both
      tm_symbols(size = 0.1, col = "quartile", shape = "BREEDING.BIRD.ATLAS.CATEGORY",
                 shapes = shapes, pal = pal, title.col = "Day quartile", title.shape = "Code",
                 jitter = jit, alpha = 0.6) +
      tm_facets(by = "month", free.coords = FALSE, drop.empty.facets = TRUE,
                free.scales = TRUE, nrow = 1) +
      tm_shape(fltr) +
      tm_polygons(border.col = "#9CD800", alpha = 0, # border.alpha = 0.4,
                  legend.show = FALSE) +
      tm_layout(title = m_title, title.size = 1) +
      tm_legend(bg.alpha = 0, outside.position = c("left", "top"))
    # tm_legend(bg.alpha = 0,
    #           main.title.fontface = 2, #title.fontface = 2,
    #           main.title = m_title, main.title.size = 1,
    #           outside = TRUE,
    #           outside.position = "bottom")

    print(out)

    # message(paste("Finished map", i, "of", n))
    #
    # if (i == 1) {
    #   t1 <- Sys.time()
    #   t_el <- t1 - t0
    #   t_el <- round(t_el * n / 60, 1)
    #   message(paste("Estmimated time to print:", t_el, "minutes"))
    # }
  }

  # close pdf device
  dev.off()
}
