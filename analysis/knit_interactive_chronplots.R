# Script to compile multiple chonplot html files.

# Put your EBD file in the folder: data/chrontplot_data, and delete the example file here. 
# There can only be one file in this folder. 

# The script chopes the eBird data into individuals csv files by species, then interactively loads each csv and runs the template Rmd file using the
# current data. The output file is named the same as in the corresponding input
# csv file but with an html extension.

# First, this section below loads your EBD file from eBird and split it into individual csv files for each species

# The final products will be interactive html files in a folder called output on the same level as the data and analysis folders. 

# Note that as written this is only pulling the atlas portal records.

library(plyr)
library(here)
setwd(here::here("analysis"))

# loads EBD data
ebird <- read.delim("../data/chronplot_data/ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)


# limits dataset to only WI atlas portal records
ebirdatlas  <- ebird[ebird$PROJECT.CODE == "EBIRD_ATL_WI", ]

# splits into individual csv files by species
d_ply(ebirdatlas, .(COMMON.NAME),
      function(ebirdatlas) write.csv(ebirdatlas,
                                file=paste(ebirdatlas$COMMON.NAME[[1]],".csv",sep="")))

# This part interactively loads each csv in the working directory and runs the template Rmd file using the
# current data.

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
