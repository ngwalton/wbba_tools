# Function to save chronology plot for each species in an eBird dataset.

# this version uses headers like "common_name" instead of "COMMON.NAME"

library(here)
library(lubridate)
library(grid)
library(gridBase)
library(RColorBrewer)

setwd(here::here("data"))


# output pdf file -- rename as needed
out_pdf <- "AMRO_RWBL_SOSP.pdf"

# choose a named RColorBrewer pallet (multiple colors), or a single color (name
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
# no_plot_codes <- c("PE", "UN")


# load data ----

# ebird data
#ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# data prep ----

# optional, limit to atlas records only
# ebird  <- ebird[ebird$PROJECT.CODE == "EBIRD_ATL_WI", ]

# sets any NAs to blank in breeding code
ebird$breeding_code[is.na(ebird$breeding_code)] <- "" 

# If this came out of excel and the date is screwy, this catches it and fixes it
# Actually the year will still be wrong because of excel issues, but all years are getting set to 2016 anyway
# If the program still glitches (possibly because the date format got screwed up in excel)
# Try resetting the date column format to "General" in excel and this might fix it
if(is.integer(ebird$observation_date))  {ebird$observation_date <- as_date(ebird$observation_date)}

# remove not valid (reason = exotic) records
ebird <- subset(ebird, approved != "0")

# flag the pigeon entries so they are not removed with the rest of the domestics
ebird <- transform(ebird, category = ifelse(common_name == "Rock Pigeon", "pigeon", category))

# remove hybrid, spuh, domestic, and slash taxonomic categories (optional)
taxa <- c("species", "issf", "form", "pigeon")
ebird <- ebird[ebird$category %in% taxa, ]

# order by TAXONOMIC.ORDER
ebird <- ebird[order(ebird$taxonomic_order), ]

# put all dates within the same year -- ignores leap year
ebird$observation_date <- sub("^20\\d\\d", "2016", ebird$observation_date)

# remove white space from evidence codes
ebird$breeding_code <- trimws(ebird$breeding_code)

# lump evidence codes if lump has been set
if (exists("lump")) {
  for (i in seq_along(lump)) {
    indx <- ebird$breeding_code %in% lump[[i]]
    ebird[indx, "breeding_code"] <- names(lump)[i]
  }
}

# remove unneeded evidence codes
if (exists("no_plot_codes")) {
  ebird <- ebird[! ebird$breeding_code %in% no_plot_codes, ]
}


# chron plot function ----

chronplot <- function(comname, ebird, pal, cex.x.axis = 0.9, cex.y.axis = 0.8) {
  # comname is the common name of the species to plot, ebird is the WBBA data
  # downloaded from ebird function assumes that column names have not been
  # changed from ebird download


  # rename columns because ebird names are long
  cols <- c("common_name", "breeding_code", "observation_date")
  newnames <- c("name", "code", "obsdate")
  ebird <- ebird[ebird$common_name == comname, cols]
  names(ebird) <- newnames

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")

  # set order that box plots will be plotted.
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in.
  # this vector will need updating if any new codes are introduced via "lump".
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "", "O", "NC")

  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (BREEDING.CODE) for",
                  comname, "are in codelevels")
    warning(warn)
  }

  # associate colors with codelevels
  if (pal %in% rownames(brewer.pal.info)) {
    n <- brewer.pal.info[pal, "maxcolors"]
    codecolors <- colorRampPalette(brewer.pal(n, pal))(length(codelevels))
  } else {
    codecolors <- rep(pal, length(codelevels))
  }

  names(codecolors) <- codelevels

  # used droplevels so that codes that where not observed are not plotted;
  # remove droplevels if you'd like unobserved codes to be included on the plot
  ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
                                  ordered = TRUE))

  # plot "empty" box plot
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = cex.y.axis, xaxt = "n",
          data = ebird, border = "white", main = comname, las = 2,
          xlab = "Date", ylab = "Breeding Codes", show.names = TRUE)

  date0 <- round_date(min(ebird$obsdate), "month")
  date1 <- round_date(max(ebird$obsdate), "month")
  labels <- seq(from = date0, to = date1, by = "month")

  if (length(unique(month(ebird$obsdate))) == 1) {
    labels <- c(min(ebird$obsdate), max(ebird$obsdate))
    labels <- unique(labels)  # in case there's only one obs
  } else {
    # limit labels to those within observed range
    int <- interval(min(ebird$obsdate), max(ebird$obsdate))
    labels <- labels[labels %within% int]

    if (nrow(ebird) > 1 && length(labels) == 1) {
      labels <- unique(c(min(ebird$obsdate), max(ebird$obsdate)))
    }
  }

  # use format "%m/%d" for e.g. 06/01
  # use format "%b %d" for e.g. "Aug 23"
  names(labels) <- format(labels, "%b %d")

  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)

  # label x axis; set font size in gpar(cex = relative_fontsize);
  # grid.text is can be hard to follow but allows for arbitrary rotation of
  # x labels
  grid.text(names(labels), x = unit(labels, "native"), y = unit(-0.7, "lines"),
            just = "right", rot = 65, gp = gpar(cex = cex.x.axis))
  popViewport(3)

  # add tick marks
  axis(1, labels, labels = FALSE)

  # uncomment this to label the x axis a second time for sanity check
  # because grid.text can be difficult to understand
  # axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)

  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]

  stripchart(obsdate ~ code, data = ebird, vertical = FALSE, method = "jitter",
             pch = 16, col = col, add = TRUE)

  #set boxplot color and partial transparency (where alpha is opacity)
  #run mycol to get the color code, then paste it into the next line
  #mycol <- rgb(245, 245, 245, max = 255, alpha = 0, names = "ltgrayclear")
  #mycol
  
  boxplot(obsdate ~ code, horizontal = TRUE,  col = "#F5F5F500", yaxt = "n", xaxt = "n",
          data = ebird, add = TRUE)
}


# plot species ----


# can plot individual species like so
# chronplot("Red-eyed Vireo", ebird)

# print a chronology plot for each species to a single pdf; note that this can
# be time consuming if plotting many species
sp <- unique(ebird$common_name)

pdf(out_pdf)

for (i in sp) {
  chronplot(i, ebird, pal)
}

dev.off()
