# Welcome to Wisconsin Breeding Bird Atlas Tools!

Here you'll find a variety of scripts used to proof, visualize, and display eBird data. These were designed for the Wisconsin Breeding Bird Atlas and should work well for other atlases using the Atlas eBird platform.

## 1. dv_date Data Validation tool

![datascreening](https://github.com/ngwalton/wbba_tools/blob/master/datascreening.png)

This uses two filters, one based on allowable breeding codes, and the other based on allowable data values. The filters flag records as early or late, or using a rare code, and extract only those records into a separate spreadsheet for review.

You’ll need 3 files:
1.	The download of the data from eBird.org.

2.	Temporal filter element: The acceptable_dates.csv file was created from what Wisconsin is calling the Breeding Guideline Bar Chart – essentially a broader guide to “safe dates”, although Wisconsin did not employ strict safe dates. For this file you must specify a start and end date to B (thought of as “safe Breeding”) and a start and end date to E (thought of as the Either period where a species may be breeding or may be migrating). You can adjust the code to run off of B or E (it currently is only filtering to B, but if you wanted it to flag more records, you could have it flagging the E records also)

3.	Breeding code filter element: The species_usable_codes.csv file was created from what Wisconsin is calling the Acceptable Breeding Codes Chart. This is a grid indicating which codes are most expected for each species. A 1 indicates it is a common code for a species, a 2 indicates it is a relatively expected code for a species, a 3 indicates it is an unexpected or atypical code for a species, and a 4 indicates it is a very unexpected or impossible code for a species. As currently written, any values of 3 or 4 will be flagged.

When the code is run, it screens the data and saves to a separate spreadsheet the complete row of eBird data from flagged records, and tags the records in the following way: early records = “Before B”,  late records “After B”, suspicious code “3”, very suspicious code “4”. 

The flagged spreadsheet is then manually screened, but this saves a lot of time compared to manually screening the entire database.

This will not catch all suspect records, we still recommend other methods of checking for suspicious records including viewing the eBird maps, and checking at county and block level for unusual species. 

The primary places you’ll need to edit the ebird_dv_date_code.R code to customize it for your use are all near the front of the code:

````
# Output file -- rename file appropriately as needed
out_file <- "out.csv"

# Modify each of these file names as needed
ebird_file <- "wbba2018.csv"
dates_file <- "acceptable_dates.csv"
codes_file <- "species_usable_codes.csv"
````

## 2. chron Breeding Chronology Plot

![chronexample](https://github.com/ngwalton/wbba_tools/blob/master/chronexample.PNG)

This maps out each breeding code across the season so you can visualize when each bird species is in each phase of its breeding cycle. Points overlay box plots to help you screen for data that may be an outlier.

## 3. wbbaspmap Species Map

![tuti](https://github.com/ngwalton/wbba_tools/blob/master/tuti.png)

This takes the raw eBird data and summarizes it into a single status for each block (Observed, Possible, Probable, Confirmed) making it easy to produce standard atlas block maps much like the live version available in Atlas eBird. Note that this particular example image was produced in ArcMap after the file was output from wbbaspmap.

## 4. wbbaspchange Change Map

![changemapexample](https://github.com/ngwalton/wbba_tools/blob/master/changemapexample.png)

This produces a block map indicating for each species whether it was found in the block during the first atlas only, second atlas only, or both atlases. Note that this particular example image was produced in ArcMap after the file was output from wbbaspchange.
 
