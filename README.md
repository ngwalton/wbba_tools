# Welcome to Wisconsin Breeding Bird Atlas Tools!

Here you'll find a variety of scripts used to proof, visualize, and display eBird data. These were designed for the Wisconsin Breeding Bird Atlas and should be adaptable for other atlases using the Atlas eBird platform.

If you download the whole zip file, it comes with sample WI data, so you should be able to run these right out of the box to check if things are working, and then you can customize them with data and other files for your region. These are designed to run using eBird data downloads from the EBD, available at: https://ebird.org/data/download
 
There are currently 7 different scripts.

## 1. data_validation (Data Validation Tool)

![datascreening](https://github.com/ngwalton/wbba_tools/blob/master/datascreening.png)

This script screens your data for suspect records. It uses two filters, one based on allowable breeding codes, and the other based on allowable date values. The filters flag records as early or late, or using a rare code, and extract only those records into a separate spreadsheet for review.

You’ll need 3 files:
1.	The download of the data from eBird.org.

2.	Temporal filter element: The acceptable_dates.csv file was created from what Wisconsin is calling the Breeding Guideline Bar Chart – essentially a broader guide to “safe dates”, although Wisconsin did not employ strict safe dates. For this file you must specify a start and end date to B (thought of as “safe Breeding”) and a start and end date to E (thought of as the Either period where a species may be breeding or may be migrating). You can adjust the code to run off of B or E (it currently is only filtering to B, but if you wanted it to flag more records, you could have it flagging the E records also). At the moment this runs each batch by year. NOTE: **You'll need to change the years on the dates to run it for each year**, but a simple find/replace in excel will do that.

3.	Breeding code filter element: The species_usable_codes.csv file was created from what Wisconsin is calling the Acceptable Breeding Codes Chart. This is a grid indicating which codes are most expected for each species. A 1 indicates it is a common code for a species, a 2 indicates it is a relatively expected code for a species, a 3 indicates it is an unexpected or atypical code for a species, and a 4 indicates it is a very unexpected or impossible code for a species. As currently written, any values of 3 or 4 will be flagged.

When the code is run, it screens the data and saves to a separate spreadsheet the complete row of eBird data from flagged records, and tags the records in the following way: early records = “Before B”,  late records “After B”, suspicious code “3”, very suspicious code “4”. 

The flagged spreadsheet is then manually screened, but this saves a lot of time compared to manually screening the entire database.

This will not catch all suspect records, we still recommend other methods of checking for suspicious records including viewing the eBird maps, and checking at county and block level for unusual species. 

The primary places you’ll need to edit the data_validation.R code to customize it for your use are all near the front of the code:

````
# Output file -- rename file appropriately as needed
out_file <- "out.csv"

# Modify each of these file names as needed
ebird_file <- "ebird_data_sample_wbbaii.txt"
dates_file <- "acceptable_dates.csv"
codes_file <- "species_usable_codes.csv"
````

## 2. chronology_plot (Breeding Chronology Plot)

![chronology_plot_example.PNG](https://github.com/ngwalton/wbba_tools/blob/master/chronology_plot_example.png)

This maps out each breeding code across the season so you can visualize when each bird species is in each phase of its breeding cycle. Box plots overlay points to help you screen for data that may be an outlier.

The main thing you'll have to do is edit this line to point to your eBird data download:
````
# ebird data
ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)
````
If you need to edit the breeding codes used, or you'd like to change the order in which they appear, edit this line:
````
codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "")
````
There is also a version of the chronology plot code where similar codes are lumped together.

## 3. block_evidence (Species Block Map)

![evidence_map_example.png](https://github.com/ngwalton/wbba_tools/blob/master/evidence_map_example.png)

This takes the raw eBird data and summarizes it into a single status for each block (Observed, Possible, Probable, Confirmed) making it easy to produce standard atlas block maps (much like the live version available in Atlas eBird). This script outputs a pdf file with maps like the image above, and also a shapefile with the highest breeding category shown for each species for each block.

The main areas you'll need to edit to customize it to your region are all at the top of the code. 

The first file is the list of 4-letter codes (which is current as of 2018, but may need updating in the future). 
````
# load data ----

# birdpop alpha codes;
# common names are in "COMMONNAME", and 4-letter alpha codes are in "SPEC"
# source: http://www.birdpop.org/pages/birdSpeciesCodes.php
alpha <- read.dbf("LIST18.DBF", as.is = TRUE)
````
The second file is a block shapefile for your region. 
````
# block shapefile; arguments for readOGR are input format dependent -- with a
# shapefile, the first argument is the directory containing the shp, and the
# second argument is the name of the shapefile without the extension
block_in <- readOGR("blk", "WbbaBlocks2015_v0_2")
````
The third file is the state ouline with counties, drawing from the USAboundaries package. If you are within the US, you probably will only have to fill in your state abbreviation here. 
````
# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")
````
And the 4th file is your eBird data download. 
````
# sample WBBA data from ebird
sp_in <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)
````
## 4. wbba_sp_change (Change Map)

![change_map_example.png](https://github.com/ngwalton/wbba_tools/blob/master/change_map_example.png)

This produces a block map indicating for each species whether it was found in the block during the first atlas only, second atlas only, or both atlases. 

Settings you need to know about are similar to the Species Block Map above, except you will need to input two eBird data files, one with the first atlas data, and one with the second atlas data. 
````
sp <- list()
sp$ii <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)
sp$i <- read.delim("ebird_data_sample_wbbai.txt", quote = "", as.is = TRUE)
```` 

## 5. date_visualizer_map (Quarter-Month Viewer)

![date_visualizer_example.png](https://github.com/ngwalton/wbba_tools/blob/master/date_visualizer_example.png)

This produces a map for each species for each month, and is color-coded to show the part of the month in which each observation occurred (dark orange q1 = first quartile of the month). Dots are jittered to allow for better viewing of clustered observations.

Note: This can take a long time to run - we've found it takes about an hour to run on one year of data (650 MB) for a state. On a 4 GB file, it was a half hour before the first pdf was printed, and total run time approached 5 hours.

To customize it, enter the name of your eBird data file within the quotes here:
````
# sample WBBA data from ebird
ebird_file <- "ebird_data_sample_wbbaii.txt"
````
Also, it can be quite helpful for eBird filter editing to import a file showing checklist filter boundaries for your region (green lines in the sample above). You'll have to prepare this file separately and save it as a kml:
````
# eBird filter
fltr <- readOGR("ebirdfilters20170817.kml", "ebirdfilters20170817")
````

## 6. blk_n_map (Records Per Block Map) 

![blk_n_example.png](https://github.com/ngwalton/wbba_tools/blob/master/blk_n_example.png)

This map shows the total number of observations that have a breeding code (excluding F) in a block during a period. It is currently set up to only display priority blocks. It shows 6 maps: 1. Data from a prior atlas; 2. Year-round codes (current atlas); 3. June and July codes only (current atlas); 4. Codes all year but excluding June and July (current atlas); 5. All blocks where the species was never coded (current atlas); 6. Results from point counts (not eBird data, at point level, not rounded to block level).  

For background reference, we include a range boundary from the eBird Status and Trends Range Maps.

It may take about an hour to run, assuming 5 years of data for a state.

To customize it, enter the name of your eBird data file within the quotes here:
````
# sample WBBA data from ebird
sp_in <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)
````
And change the state for the county outline here:
````
# optional county layer --  only used for map printing
cnty <- us_boundaries(type = "county", resolution = "high", states = "WI")
````
You could also adjust the months of interest here:
````
# months of interest -- the months will be aggregated
mo <- c("June", "July")
````
This points it to data for the prior atlas map:
````
# sample WBBA I from eBird
sp1 <- read.delim("ebird_data_sample_wbbai.txt", quote = "", as.is = TRUE)
````
This is the file for the sixth map, showing point counts:
````
# point count data
pt_count <- read_excel("point_count_data_sample_wbbaii.xlsx",
                       sheet = "sample_data")
````                     
This section controls the eBird range maps, as coded it expects them extracted into a subfolder within the working directory
````
# include eBird range map? requires downloading/unzipping maps from eBird at:
# https://ebird.org/science/status-and-trends/download-data
include_range <- TRUE

# location of directory containing optional eBird range map files; required if
# include_range is TRUE; only species with breeding and/or resident ranges will
# include range in the output map
range_dir <- "./ebird_range"
 ````   
## 7. knit_interactive_chronplots (Interactive Chronology Plot)

![interactive_chronplot_example.png](https://github.com/ngwalton/wbba_tools/blob/master/interactive_chronplot_example.png)

This is an interactive version of the chronology plot that allows you to see the date of the point on mouseover, and if you click the point it will open the eBird checklist in a new browser window!

The file to open to run this is called knit_interactive_chronplots.R, but it also calls the companion markdown file interactive_chronplot.Rmd.

To load your own data, put your EBD file in the folder: data/chrontplot_data, and delete the example file here. There can only be one file in this folder. The script will chop it up into separate files by species.

Then replace the call to the sample file with a call to your own eBird data file:
````
# loads EBD data
ebird <- read.delim("ebird_data_sample_wbbaii.txt", quote = "", as.is = TRUE)
````
Note that as written this is only pulling the atlas portal records.

The output product will be interactive html files. They export into a new folder called "output" on the same level as the data and analysis folders.





