# script to update sample eBird data sources because format on eBird has
# changed. This loads the new format, selects the same records that are in the
# old sample data, then writes the new data without changing its format from the
# original new file except that the output is sorted by
# "GLOBAL UNIQUE IDENTIFIER"


library(data.table)
library(parallel)  # for detectCores


setwd(here::here("data"))

# n cores used to read/write with fread/fwrite
n_core <- detectCores() - 1


# output files ----

wbba_out <- list()
wbba_out$ii <- "ebird_data_sample_wbbaii.txt"
wbba_out$i <- "ebird_data_sample_wbbai.txt"


# load data ----

# original sample data hosted on github
old <- list()
old$ii <- fread("eBirdDataSampleWIAtlasII.txt", check.names = FALSE)
old$i <- fread("eBirdDataSampleWIAtlasI.txt", check.names = FALSE)

# data in new format -- these files are not hosted on github
new <- list()
new$ii <- fread("ebd_US-WI_200901_201912_relOct-2019.txt", quote = "",
  nThread = n_core, check.names = FALSE, colClasses = "character",
  strip.white = FALSE)

new$i <- fread("ebd_US-WI_199501_200012_relOct-2019.txt", quote = "",
  nThread = n_core, check.names = FALSE, colClasses = "character",
  strip.white = FALSE)

# set key for fast subsetting -- this takes a couple of minutes
t0 <- Sys.time()

for (x in names(new)) {
  setkey(new[[x]], `GLOBAL UNIQUE IDENTIFIER`)

  # there is an empty/unnamed column at the end of the new data format -- this
  # sets the name back to the empty string
  names(new[[x]])[names(new[[x]]) == "V47"] <- ""
}

t1 <- Sys.time()
t1 - t0

# select records ----

for (x in names(old)) {
  # this removes the key/orders by the old dataset
  new[[x]] <- new[[x]][old[[x]][, `GLOBAL UNIQUE IDENTIFIER`], nomatch = NULL]
}


# error checking ----

# confirm all IDs were matched and new is sorted the same as old
# should return TRUE... but it's FALSE
all(
  vapply(c("i", "ii"), function(x)
    identical(new[[x]][, `GLOBAL UNIQUE IDENTIFIER`],
      old[[x]][, `GLOBAL UNIQUE IDENTIFIER`]),
    NA)
)

# new has fewer rows than old -- assumed this is because some records were
# deleted and moved on
data.frame(new = sapply(new, nrow), old = sapply(old, nrow))
#      new   old
# ii 23868 23898
# i   1051  1055


# write output files ----

for (x in names(wbba_out)) {
  fwrite(new[[x]], file = wbba_out[[x]], sep = "\t", nThread = n_core,
    quote = FALSE, eol = "\n")
}
