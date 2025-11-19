# Function to save chronology plot for each species in an eBird dataset.

library(here)
library(lubridate)
library(grid)
library(gridBase)
library(RColorBrewer)
library(dplyr)
library(purrr)
library(extrafont)

#setwd(here::here("data"))

# fonts
# TrueType fonts from current working directory
font_import(pattern = "ScalaSans", prompt = FALSE)

# register fonts with R's PDF and PostScript - needed only once
loadfonts(device = "win")
loadfonts(device = "pdf")

# should list ScalaSans
fonts()





# output pdf file -- rename as needed
# out_pdf <- "chronology_plot.pdf"

# choose a named RColorBrewer palette (multiple colors), or a single color (name
# or hex); see brewer.pal.info for list and display.brewer.all() to view all
# pallets
pal <- "Paired"

# evidence codes to lump -- uncomment/edit as needed
# this is a list of named vectors where the vector name is used to place all
# codes in the corresponding vector (e.g. 'S = c("S", "S7", "M")' replaces all
# "S", "S7", and "M" with "S"). Note that any code that is not already in
# variable "codelevels" in function "chronplot" (below) will need to be added
# there.
# lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))

# evidence codes to remove -- uncomment/edit as needed
# this is a vector of evidence codes that will be not be plotted
no_plot_codes <- c("F", "", "O", "NC", "UN")


# load data ----

# ebird data
ebird <- read.delim("WBBA2_ATLASWI_plussensitivepluszerocount_EBDMar2023_GOOD_111725.txt", quote = "", as.is = TRUE)
#ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# ebird <- read.delim("REvI2.txt", quote = "", as.is = TRUE)

#column names to uppercase if needed
names(ebird) <- toupper(names(ebird))
library(stringr)   
colnames(ebird) <- str_replace_all(colnames(ebird), "[:punct:]", ".")


# data prep ----

# format date column - ONE POSSIBILITY
# ebird$OBSERVATION.DATE <- mdy(ebird$OBSERVATION.DATE)
# class(ebird$OBSERVATION.DATE)

# other possibilities for fixing date column
# If this came out of excel and the date is screwy, this catches it and fixes it
# Actually the year will still be wrong because of excel issues, but all years are getting set to 2016 anyway
# If the program still glitches (possibly because the date format got screwed up in excel)
# Try resetting the date column format to "General" in excel and this might fix it
if(is.integer(ebird$OBSERVATION.DATE))  {ebird$OBSERVATION.DATE <- as_date(ebird$OBSERVATION.DATE)}
if(is.character(ebird$OBSERVATION.DATE))  {ebird$OBSERVATION.DATE <- as_date(ebird$OBSERVATION.DATE)}
class(ebird$OBSERVATION.DATE)

# optional, limit to atlas records only
ebird  <- ebird[ebird$PROJECT.CODE == "EBIRD_ATL_WI", ]

# Optional, remove duplicate observations by group code

# first set blanks to NA
ebird$GROUP.IDENTIFIER[ebird$GROUP.IDENTIFIER==""] <- NA

#make a new column to order
ebird$BREEDINGCATEGORYNUM <- ebird$BREEDING.CATEGORY

ebird$BREEDINGCATEGORYNUM[ebird$BREEDINGCATEGORYNUM == "C1"] <- "1"
ebird$BREEDINGCATEGORYNUM[ebird$BREEDINGCATEGORYNUM == "C2"] <- "2"
ebird$BREEDINGCATEGORYNUM[ebird$BREEDINGCATEGORYNUM == "C3"] <- "3"
ebird$BREEDINGCATEGORYNUM[ebird$BREEDINGCATEGORYNUM == "C4"] <- "4"
ebird$BREEDINGCATEGORYNUM <- as.numeric(ebird$BREEDINGCATEGORYNUM)
#sorting ensures higher categories are used rather than an uncoded group record
ebird <- ebird[order(ebird$BREEDINGCATEGORYNUM, decreasing = TRUE, na.last = TRUE), ]
#remove duplicates
ebird <- ebird %>%
  filter(!duplicated(GROUP.IDENTIFIER, incomparables = NA))

# optional, remove uncoded and blank breeding code records
ebird <- ebird %>%
  dplyr::filter(BREEDING.CATEGORY %in% c('C2', 'C3', 'C4'))

# remove not valid (reason = exotic) records
ebird <- subset(ebird, APPROVED != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
ebird <- transform(ebird, CATEGORY = ifelse(COMMON.NAME == "Rock Pigeon", "pigeon", CATEGORY))

# remove hybrid, spuh, domestic, and slash taxonomic categories (optional)
taxa <- c("species", "issf", "form", "pigeon")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

# order by TAXONOMIC.ORDER
ebird <- ebird[order(ebird$TAXONOMIC.ORDER), ]

# put all dates within the same year -- ignores leap year
ebird$OBSERVATION.DATE <- sub("^20\\d\\d", "2016", ebird$OBSERVATION.DATE)

# remove white space from evidence codes
ebird$BREEDING.CODE <- trimws(ebird$BREEDING.CODE)

# lump evidence codes if lump has been set
if (exists("lump")) {
  for (i in seq_along(lump)) {
    indx <- ebird$BREEDING.CODE %in% lump[[i]]
    ebird[indx, "BREEDING.CODE"] <- names(lump)[i]
  }
}

# remove unneeded evidence codes
if (exists("no_plot_codes")) {
  ebird <- ebird[! ebird$BREEDING.CODE %in% no_plot_codes, ]
}



# chron plot function ----

chronplot <- function(comname, ebird, pal, cex.x.axis = 0.9, cex.y.axis = 0.3) {
  # comname is the common name of the species to plot, ebird is the WBBA data
  # downloaded from ebird function assumes that column names have not been
  # changed from ebird download


  # rename columns because ebird names are long
  cols <- c("COMMON.NAME", "BREEDING.CODE", "OBSERVATION.DATE")
  newnames <- c("name", "code", "obsdate")
  ebird <- ebird[ebird$COMMON.NAME == comname, cols]
  names(ebird) <- newnames

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")

  

  
  # set order that box plots will be plotted.
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in.
  # this vector will need updating if any new codes are introduced via "lump".
  # semichronological
  # codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
  #                "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
  #                "F", "", "O", "NC")
  #low to high
  #deleted UN after DD
  codelevels <- c("H", "S", "S7", "M", "P", "T", "C", "N", "A", "B", "PE", "CN", "NB",  
                  "DD",  "ON", "FL","CF", "FY", "FS", "NE", "NY") #,  
                  # "F", "", "O", "NC", "UN")

  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (BREEDING.CODE) for",
                  comname, "are in codelevels")
    warning(warn)
  }

  # associate colors with codelevels
  # if (pal %in% rownames(brewer.pal.info)) {
  #   n <- brewer.pal.info[pal, "maxcolors"]
  #   codecolors <- colorRampPalette(brewer.pal(n, pal))(length(codelevels))
  # } else {
  #   codecolors <- rep(pal, length(codelevels))
  # }
  
  codecolors <- rep("", length(codelevels))
  names(codecolors) <- codelevels
  
  confirmed <- c("PE", "CN", "NB", "DD", "ON", "FL", "CF", "FY", "FS", "NE", "NY")
  probable <- c("S7", "M", "P", "T", "C", "N", "A", "B")
  possible <- c("H", "S")
  # observed <- c("F", "", "O", "NC")
  # deleted UN after DD
  
  codecolors[confirmed] <- "#2D1C45"
  codecolors[probable] <- "#9C7DC5"
  codecolors[possible] <- "#D4C7E6"
  # codecolors[observed] <- "#e033ff" # ugly placeholder -- shouldn't show in figs
  
  # used droplevels so that codes that where not observed are not plotted;
  # remove droplevels if you'd like unobserved codes to be included on the plot
  # ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
  #                                 ordered = TRUE))
  ebird$code <- factor(ebird$code, levels = codelevels,
                                  ordered = TRUE)
  
  transparent_red <- "#FF000000"

  # plot "empty" box plot
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = cex.y.axis,
          xaxt = "n", yaxt = "n",
          data = ebird, border = "gray92", medlwd = 0.0001, whisklwd = 0.001,medcol = "gray40",
          staplelwd = 0.001, boxlwd = 0.001, outlty = "blank", boxcol = transparent_red, outpch = NA,
          # messing with width but this just controls the boxes not the axis
          #at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
          las = 2,
          col = "gray92", 
          ylab = "", adj = -2, xlab = NULL,
          # xlab = substitute(paste(bold("Date"))),
          # ylab = substitute(paste(bold("Breeding Codes"))), # not working
          # xlab = "", ylab = "",  # there may be a better solution
          show.names = TRUE, frame.plot = FALSE)
  
  # adjusting positioning of y axis
  #title(ylab="Breeding Codes", line=2.3)
  par(family = "ScalaSans")
  #title(ylab = "Breeding Codes", line = 2.9, cex.lab = 1.5)
  
  # remove if you don't want axis lines to be full width/height
  # adjust lwd to make line wider/narrower
  box(bty="l", lwd = 0.5)
  
  
  first_obs <- min(ebird$obsdate)
  last_obs <- max(ebird$obsdate)
  
  if (first_obs == last_obs) {
    labels <- first_obs
  } else {
    # Generate potential labels for the 1st of each month
    date0 <- floor_date(first_obs, "month")
    date1 <- floor_date(last_obs, "month")
    monthly_labels <- seq(from = date0, to = date1, by = "month")
    
    # Define buffer to avoid overlap
    date_buffer <- 15
    #15 worked for most but WISN WITU CORA RWBL NOCA used 28
    
    # Filter out monthly labels that fall within the buffer zone of first_obs
    # but only if they are not the same as first_obs
    labels_to_remove <- monthly_labels > first_obs & monthly_labels <= first_obs + days(date_buffer)
    
    # ALSO, if floor_date(first_obs) is not first_obs itself, and it's within the buffer, remove it.
    if (date0 < first_obs & (first_obs - date0) <= days(date_buffer)) {
      labels_to_remove[monthly_labels == date0] <- TRUE
    }
    
    # Filter monthly labels that are too close to last_obs
    labels_to_remove[monthly_labels < last_obs & monthly_labels >= last_obs - days(date_buffer)] <- TRUE
    
    # Keep only the labels we don't want to remove
    labels <- monthly_labels[!labels_to_remove]
    
    # Add the original first and last observation dates explicitly
    labels <- unique(c(first_obs, labels, last_obs))
    
    # Sort and clean up labels
    labels <- sort(labels)
  }
  
  names(labels) <- paste(month(labels, label = TRUE, abbr = TRUE), day(labels))

  vps <- gridBase::baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)

  # label x axis; set font size in gpar(cex = relative_fontsize);
  # grid.text is can be hard to follow but allows for arbitrary rotation of
  # x labels
  # grid.text(names(labels), x = unit(labels, "native"), y = unit(-0.7, "lines"),
  #           just = "right", rot = 65, gp = gpar(cex = cex.x.axis))
  # popViewport(3)

  # add tick marks and labels to x axis
  axis(1, labels, labels = names(labels), cex.axis = cex.x.axis,
       lwd.ticks = 0.5, lwd = 0)
  
  # increase tick width of first and last report
  # adjust lwd.ticks as desired
  #axis(1, unique(c(first_obs, last_obs)), labels = FALSE,
  #     lwd.ticks = 1, lwd = 0)
  
  # y axis ticks and labels
  axis(2, at = seq_along(levels(ebird$code)), labels = levels(ebird$code),
       lwd.ticks = 0.5, lwd = 0, cex.axis = cex.y.axis, las = 2)

  # uncomment this to label the x axis a second time for sanity check
  # because grid.text can be difficult to understand
  # axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)
  
  #set boxplot color and partial transparency (where alpha is opacity)
  #run mycol to get the color code, then paste it into the next line
  #mycol <- rgb(245, 245, 245, max = 255, alpha = 0, names = "ltgrayclear")
  #mycol
  

  
  boxplot(obsdate ~ code, horizontal = TRUE, col = "#F5F5F500", yaxt = "n", xaxt = "n",
          medcol = "gray40", 
          whiskcol = "gray40", 
          staplecol = "gray40", 
          boxcol = transparent_red, 
          medlwd = 1,
          whisklwd = 0.3,
          staplelwd = 0.3,
          boxlwd = 0.3,
          data = ebird, add = TRUE, outline = FALSE, frame.plot = FALSE)
  
  

  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]

  stripchart(obsdate ~ code, data = ebird, vertical = FALSE, method = "jitter",
             pch = 16, cex= 1, col = col, add = TRUE, frame.plot = FALSE)

  
  


}

# plot species ----


# can plot individual species like so
# chronplot("Red-eyed Vireo", ebird)

# print a chronology plot for each species to a single pdf; note that this can
# be time consuming if plotting many species
sp <- unique(ebird$COMMON.NAME)

####################################start here

#set up proper filenames for printing

#load crosswalk file
cw <- read.csv("filerenamecrosswalk2.csv")

# sp from a character object to a dataframe
spdf <- as.data.frame(sp)
names(spdf)[names(spdf) == 'sp'] <- 'common_name'

#join
namedf <- inner_join(spdf, cw, by = "common_name")

# columns to paste together
cols <- c( 'BreedingPhenology' , 'common_name')

# create a new column with the columns collapsed together
namedf$filename <- apply( namedf[ , cols ] , 1 , paste , collapse = "_" )

#back to just 2 columns
sp <- namedf %>% dplyr::select(c(common_name, filename))
#sp <- namedf %>% select(filename)

# pdf(out_pdf)

### THIS IS THE BEST PLACE TO ADJUST OVERALL HEIGHT
#sizing
pdf_size_w <- 3.75 
pdf_size_h <- 5.75
#par(mar = c(bottom, left, top, right))

gc()

sp_birds <- sp %>% pull(common_name)

for (i in sp_birds) {
  filename <- sp[which(sp$common_name == i), "filename"]
  current_pdf <- paste0(filename, ".pdf")
  cairo_pdf(current_pdf, family = "ScalaSans", width = pdf_size_w, height = pdf_size_h, (mar = c(5,5,5,5)))
  #current_pdf <- paste0(gsub("[ -]", "_", tolower(i), perl = TRUE), ".pdf")
  
### THIS IS THE BEST PLACE TO ADJUST Y AXIS TICK SIZE  
  # adjust cex.x.axis and cex.y.axis to change the size of the x and y axis tick labels
  chronplot(i, ebird, pal, cex.x.axis = 1.5, cex.y.axis = 1.5)
  dev.off()
}

#dev.off()
