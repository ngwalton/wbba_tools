# Welcome to Wisconsin Breeding Bird Atlas Tools!

Here you'll find a variety of scripts used to proof, visualize, and display eBird data. These were designed for the Wisconsin Breeding Bird Atlas and should work well for other atlases using the Atlas eBird platform.

## 1. dv_date Data Validation tool

This uses two filters, one based on allowable breeding codes, and the other based on allowable data values. The filters flag records as early or late, or using a rare code, and extract only those records into a separate spreadsheet for review.

## 2. chron Breeding Chronology Plot

![chronexample](https://github.com/ngwalton/wbba_tools/blob/master/chronexample.PNG)

This maps out each breeding code across the season so you can visualize when each bird species is in each phase of its breeding cycle. Points overlay box plots to help you screen for data that may be an outlier.

## 3. wbbaspmap Species Map

This takes the raw eBird data and summarizes it into a single status for each block (Observed, Possible, Probable, Confirmed) making it easy to produce standard atlas block maps much like the live version available in Atlas eBird.

## 4. wbbaspchange Change Map

This produces a block map indicating for each species whether it was found in the block during the first atlas only, second atlas only, or both atlases.
 
