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

# remove spuh and slash taxa
taxa <- c("species", "issf", "domestic", "form", "hybrid")
ebird <- ebird[ebird$CATEGORY %in% taxa, ]

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

#if any still read NA then make them false too
ebird$result <- as.character(ebird$result)
ebird$result[is.na(ebird$result)] <- "FALSE"

# drop rows for the species that never have breeding categories
ebird <- subset(ebird, result != "FALSE")

# (Optional) If other species give you trouble for any reason you can uncomment and remove them here. 
# AMRO was crashing for me because it was too big.
# Or you can just delete the csv file before the second half of the code.
# ebird <- subset(ebird, COMMON.NAME != "American Robin")
# ebird <- subset(ebird, COMMON.NAME != "Common Redpoll")

# splits into individual csv files by species
d_ply(ebird, .(COMMON.NAME),
      function(ebird) write.csv(ebird,
                                file=paste(ebird$COMMON.NAME[[1]],".csv",sep="")))
      
#remove unneededcolumns
ebird <- subset(ebird, select = -c(highestcat, result) )

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
