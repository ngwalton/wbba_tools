# Function to save chronology plot for each species an eBird dataset.


library(here)
library(lubridate)

setwd(here::here("data"))


# output pdf file -- rename as needed
out_pdf <- "chonology_plot.pdf"


# load data ----

# ebird data
ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)

# remove hybrid, spuh, and slash taxonomic categories (optional)
taxa <- c("species", "issf", "domestic", "form")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

# order by TAXONOMIC.ORDER
ebird <- ebird[order(ebird$TAXONOMIC.ORDER), ]



# chron plot function ----

chronplot <- function(comname, ebird) {
  # comname is the common name of the species to plot, ebird is the WBBA data
  # downloaded from ebird function assumes that column names have not been
  # changed from ebird download

  # select records for the desired species; consider moving this outside the
  # function if more complex subsetting is needed
  cols <- c("COMMON.NAME", "BREEDING.BIRD.ATLAS.CODE", "OBSERVATION.DATE")
  newnames <- c("name", "code", "obsdate")  # just because ebirds names are long
  ebird <- ebird[ebird$COMMON.NAME == comname, cols]
  names(ebird) <- newnames

  # some of the codes have a space at the end and some don't - this removes the
  # space
  ebird$code <- trimws(ebird$code)

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")

  # set order that box plots will be plotted
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "")

  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (BREEDING.BIRD.ATLAS.CODE) for",
                  comname, "are in codelevels")
    warning(warn)
  }

  # associate colors with codelevels
  codecolors <- rainbow(length(codelevels))
  names(codecolors) <- codelevels

  # used droplevels so that codes that where not observed are not plotted;
  # remove droplevels if you'd like unobserved codes to be included on the plot
  ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
                                      ordered = TRUE))

  # plot "empty" box plot
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = 0.5, xaxt = "n",
          data = ebird, border = "white")

  # set length.out to the number of date labels desired
  date0 <- round_date(min(ebird$obsdate), "month")
  date1 <- round_date(max(ebird$obsdate), "month")
  labels <- seq(from = date0, to = date1, by = "month")

  # if you'd like labels like "Aug 23", use format "%b %d"
  axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)

  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]

  stripchart (obsdate ~ code, data = ebird, vertical = FALSE, method = "jitter",
              pch = 16, col = col, add = TRUE)

  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = 0.5, xaxt = "n",
          data = ebird, add = TRUE)

  title(comname, xlab = "Date (Month/Day)", ylab = "Breeding Codes")
}


# plot species ----


# can plot individual species like so
# chronplot("Red-eyed Vireo", ebird)

# print a chronology plot for each species to a single pdf; note that this can
# be time consuming if plotting many species
sp <- unique(ebird$COMMON.NAME)

pdf(out_pdf)

for (i in sp) {
  chronplot(i, ebird)
}

dev.off()
