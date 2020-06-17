# Function to save chronology plot for each species in an eBird dataset.


library(here)
library(lubridate)
library(grid)
library(gridBase)
library(RColorBrewer)

setwd(here::here("data"))


# output pdf file -- rename as needed
out_pdf <- "chonology_plot.pdf"

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
# lump <- list(S = c("S", "S7", "M"), O = c("", "F"))

# evidence codes to remove -- uncomment/edit as needed
# this is a vector of evidence codes that will be not be plotted
# drop <- c("PE", "UN")


# load data ----

# ebird data
ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# data prep ----

# remove hybrid, spuh, and slash taxonomic categories (optional)
taxa <- c("species", "issf", "domestic", "form")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

# order by TAXONOMIC.ORDER
ebird <- ebird[order(ebird$TAXONOMIC.ORDER), ]

# put all dates within the same year -- ignores leap year
ebird$OBSERVATION.DATE <- sub("^20\\d\\d", "2015", ebird$OBSERVATION.DATE)

# remove white space from evidence codes
ebird$BREEDING.BIRD.ATLAS.CODE <- trimws(ebird$BREEDING.BIRD.ATLAS.CODE)

# lump evidence codes if lump has been set
if (exists("lump")) {
  for (i in seq_along(lump)) {
    indx <- ebird$BREEDING.BIRD.ATLAS.CODE %in% lump[[i]]
    ebird[indx, "BREEDING.BIRD.ATLAS.CODE"] <- names(lump)[i]
  }
}

# remove unneeded evidence codes
if (exists("drop")) {
  ebird <- ebird[! ebird$BREEDING.BIRD.ATLAS.CODE %in% drop, ]
}


# chron plot function ----

chronplot <- function(comname, ebird, pal) {
  # comname is the common name of the species to plot, ebird is the WBBA data
  # downloaded from ebird function assumes that column names have not been
  # changed from ebird download


  # rename columns because ebird names are long
  cols <- c("COMMON.NAME", "BREEDING.BIRD.ATLAS.CODE", "OBSERVATION.DATE")
  newnames <- c("name", "code", "obsdate")
  ebird <- ebird[ebird$COMMON.NAME == comname, cols]
  names(ebird) <- newnames

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")

  # set order that box plots will be plotted.
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in.
  # this vector will need updating if any new codes are introduced via "lump".
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "", "O")

  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (BREEDING.BIRD.ATLAS.CODE) for",
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
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = 0.5, xaxt = "n",
          data = ebird, border = "white", main = comname,
          xlab = "Date (Month/Day)", ylab = "Breeding Codes")

  date0 <- round_date(min(ebird$obsdate), "month")
  date1 <- round_date(max(ebird$obsdate), "month")
  labels <- seq(from = date0, to = date1, by = "month")

  # if you'd like labels like "Aug 23", use format "%b %d"
  names(labels) <- format(labels, "%m/%d")

  # limit labels to those within observed range
  int <- interval(min(ebird$obsdate), max(ebird$obsdate))
  labels <- labels[labels %within% int]

  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)

  # label x axis; set font size in gpar(cex = relative_fontsize);
  # grid.text is can be hard to follow but allows for arbitrary rotation of
  # x labels
  grid.text(names(labels), x = unit(labels, "native"), y = unit(-0.7, "lines"),
            just = "right", rot = 65, gp = gpar(cex = 0.9))
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

  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = 0.5, xaxt = "n",
          data = ebird, add = TRUE)
}


# plot species ----


# can plot individual species like so
# chronplot("Red-eyed Vireo", ebird)

# print a chronology plot for each species to a single pdf; note that this can
# be time consuming if plotting many species
sp <- unique(ebird$COMMON.NAME)

pdf(out_pdf)

for (i in sp) {
  chronplot(i, ebird, pal)
}

dev.off()
