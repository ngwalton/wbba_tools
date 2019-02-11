## Function to plot WBBA chronology

# Authors: N Anich and N Walton
# Created: 16 Sept 2015
# Last updated: 21 Oct 2015

library(here)

setwd(here::here("data"))

chronplot <- function(comname, ebird) {
  # comname is the species to plot, ebird is the WBBA data downloaded from ebird
  # function assumes that column names have not been changed from ebird download

  # select records for the desired species
  # consider moving this outside the function if more complex subsetting is needed
  cols <- c("COMMON.NAME", "BREEDING.BIRD.ATLAS.CODE", "OBSERVATION.DATE")
  newnames <- c("name", "code", "obsdate")  # just because ebirds names are long
  ebird <- ebird[ebird$COMMON.NAME == comname, cols]
  names(ebird) <- newnames

  # some of the codes have a space at the end
  # and some don't - this removes the space
  ebird$code <- gsub(" ", "", ebird$code, fixed = TRUE)

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%m/%d/%Y")

  # set order that box plots will be plotted
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in
  codelevels <- c("H","S","S7","M","T","P","C","B","CN","NB","A", "N","DD",
                  "ON","NE","FS","CF","NY","FY","FL","PE","UN","F","")

  # associate colors with codelevels
  codecolors <- rainbow(24)
  names(codecolors) <- codelevels

  # used droplevels so that codes that where not observed are not plotted
  # remove droplevels if you'd like unobserved codes to be included on the plot
  ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
                                      ordered = TRUE))

  boxplot (obsdate~code, horizontal=TRUE, cex.axis=0.5, xaxt="n",data=ebird)

  # set length.out to the number of labels desired
  labels <- with(ebird, seq(min(obsdate), max(obsdate), length.out = 5))

  # or if you like labels like "Aug 23", use format "%b %d"
  axis(1, labels, format(labels, "%m/%d"), col.axis="red")

  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]

  stripchart (obsdate~code, data=ebird, vertical=FALSE, method="jitter",
              pch = 16, col = col, add = TRUE)

  title(comname, xlab="Date (Month/Day)", ylab="Breeding Codes")
}


## examples of using the function:

# read in data
# my impression is that setting code a factor doesn't work as expected if
# stringsAsFactors is true (defult)
ebird <- read.delim("eBirdDataSampleWIAtlasII.txt", as.is = TRUE)

# plot a couple species
chronplot("Red-eyed Vireo", ebird)

# or all species in ebird to pdf
# remove hybrid, spuh, and slash taxonomic categories (optional)
taxa <- c("species", "issf", "domestic", "form")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

# order by TAXONOMIC.ORDER
ebird <- ebird[order(ebird$TAXONOMIC.ORDER), ]

# uncomment this and skip the next three lines if you really want all species to
# be printed to a single pdf - might take some time;
# this should be in taxonmic order after ordering ebird on TAXONOMIC.ORDER
# sp <- unique(ebird$COMMON.NAME) # or just a subset of species
                                # e.g., 'c("Cerulean Warbler", "American Robin")'

# all species will probably take a while, so subsetting to just those species
# with more than 100 records for example;
# table reports in alphabetical order which is why sp needs soring in with this
# method.
totrec <- table(ebird$COMMON.NAME)
sp <- names(totrec[totrec > 2000])
sp <- with(ebird, unique(COMMON.NAME[COMMON.NAME %in% sp]))  # ugly but works

pdf("example.pdf")
for (i in sp) {
  chronplot(i, ebird)
}
dev.off()  # close the pdf file
