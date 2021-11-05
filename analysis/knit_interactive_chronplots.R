# Script to compile multiple chonplot html files.

# Put your EBD file in the folder: data/chrontplot_data, and delete the example file here. 
# There can only be one file in this folder. 

# The script chopes the eBird data into individuals csv files by species, then interactively loads each csv and runs the template Rmd file using the
# current data. The output file is named the same as in the corresponding input csv file but with an html extension.

# First, this section below loads your EBD file from eBird and split it into individual csv files for each species.

# The final products will be interactive html files in a folder called output on the same level as the data and analysis folders. 

# Note that as written this is only pulling the atlas portal records.

library(plyr)
library(dplyr)
library(data.table)
library(here)
setwd(here::here("analysis"))

# loads EBD data (change this to your filename)
ebird <- read.delim("../data/chronplot_data/ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)

# limits dataset to only WI atlas portal records (change this to your atlas if desired)
ebird  <- ebird[ebird$PROJECT.CODE == "EBIRD_ATL_WI", ]

# remove spuh, slash, and domestic taxa
taxa <- c("species", "issf", "form", "hybrid")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

# remove parentheses from hybrid names
ebird$COMMON.NAME<-gsub("[[:punct:]]","",as.character(ebird$COMMON.NAME))

# This gets rid of species with no or only category 1 breeding codes, which otherwise crash the program

# create duplicate of category field
ebird$highestcat <- ebird$BREEDING.CATEGORY

# remove C from category field
ebird$highestcat<-gsub("C","",as.character(ebird$highestcat))

# make new column numeric
ebird$highestcat<-lapply(ebird$highestcat,as.numeric)

# now find the maximum breeding category for each species, if it's 1, we'll remove it.

# set as data table
group <- as.data.table(ebird)

# find the single highest category for the species
maxcats <- group[group[, .I[which.max(highestcat)], by=COMMON.NAME]$V1]

# should we keep this species?
maxcats$result <- ifelse(maxcats$highestcat < 2, FALSE, TRUE)

# join the boolean column to eBird table
lookup <- maxcats[, c("COMMON.NAME","result")]
ebird <- join(ebird, lookup, by = "COMMON.NAME")

# drop unneeded tables
rm(group)
rm(lookup)
rm(maxcats)
rm(taxa)

# drop rows for the species that never have breeding categories
ebird <- subset(ebird, result != "FALSE")

#if any still read NA then make them false too
ebird$result <- as.character(ebird$result)
ebird$result[is.na(ebird$result)] <- "FALSE"

#remove unneededcolumns
ebird <- subset(ebird, select = -c(highestcat, result) )

# SPECIES EXCLUSION LIST (Optional)
# If species give you trouble for any reason, or if you don't want them run you can uncomment and remove them here. 
# Large files (over 80 MB) are likely to cause a freeze or crash!!
# Or you can just delete the csv file before the second half of the code.
# ebird <- subset(ebird, COMMON.NAME != "American Robin")
# ebird <- subset(ebird, COMMON.NAME != "American Crow")
# ebird <- subset(ebird, COMMON.NAME != "American Goldfinch")
# ebird <- subset(ebird, COMMON.NAME != "Black-capped Chickadee")
# ebird <- subset(ebird, COMMON.NAME != "Blue Jay")
# ebird <- subset(ebird, COMMON.NAME != "Mourning Dove")
# ebird <- subset(ebird, COMMON.NAME != "Northern Cardinal")
# ebird <- subset(ebird, COMMON.NAME != "Red-winged Blackbird")

# splits into individual csv files by species
d_ply(ebird, .(COMMON.NAME),
      function(ebird) write.csv(ebird,
                                file=paste(ebird$COMMON.NAME[[1]],".csv",sep="")))
  
# clearing ebird table to reduce memory used
rm(ebird)

# garbage collection to further free up memory
gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE)

#################################################################
#################################################################
# RUN THE TOP HALF FIRST, THEN STOP HERE!
# Go inspect the csvs that got created (in the Analysis folder)
# If any are over 80MB, either remove those and then proceed with the second half of the code or add them to the exclusion list above and rerun the whole code.
# This script (apparently pandoc is the culprit) really really does not like large files.
# You may get 90 or 100 MB files to run, but your computer may freeze for a half hour or crash.
# SO RUN LARGE FILES AT YOUR OWN PERIL!

# This part interactively loads each csv and runs the template Rmd file using the current data.

library(rmarkdown)

# output directory relative to working directory
out_dir <- "../output/chronplots"

# create output dir if it does not exist
if (! dir.exists(out_dir)) {
  dir.create(out_dir)
}

files <- list.files(pattern = "*.csv", ignore.case = TRUE)

# template Rmd which will be compiled with each input file in files
template <- "interactive_chronplot.Rmd"

for (f in files) {
  # current ebird data -- quoting may need to be set to the empty string if
  # input format changes
  ebird <- read.csv(f, as.is = TRUE)

  # current group/family or whatever the file is named
  group <- sub("\\.csv", "", f, ignore.case = TRUE)

  # name of output file
  out_file <- paste0(group, ".html")

  # knit using current group
  render(template, "html_document", output_file = out_file,
    output_dir = out_dir)
}
