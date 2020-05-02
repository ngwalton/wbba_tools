# This script was written to help streamline data validation for WBBA eBird
# data. It checks if breeding code is unlikely or date is out of expected range,
# then prints a CSV file of suspect records. Validation tags for date range and
# breeding code are written to columns "dv_date" and "dv_code" respectively. NA
# values in these columns indicate that no issue was found for that particular
# parameter.


# Setup ----

# several lines here will need modification -- see comments below

# Set working directory to location where input files are located and
# output file will be written
setwd(here::here("data"))

# Output file -- rename file appropriately as needed
out_file <- "out.csv"

# Modify each of these file names as needed
ebird_file <- "ebird_data_sample_wbbaii.txt"
dates_file <- "acceptable_dates.csv"
codes_file <- "species_usable_codes.csv"

# Input data
ebird <- read.delim(ebird_file, as.is = TRUE)
dates <- read.csv(dates_file, row.names = 1, as.is = TRUE)
codes <- read.csv(codes_file, row.names = 1, as.is = TRUE)

# Setup column names for ebird data frame
# Modify these here if names change over time
ename <- "COMMON.NAME"
ecode <- "BREEDING.BIRD.ATLAS.CODE"
edate <- "OBSERVATION.DATE"

# Extract date column names from dates data frame (assumes all end in ".date")
date_cols <- grep("\\.date$", names(dates), value = TRUE)


# Shouldn't need to modify anything past this point


# Subset ebird to those species listed in codes
missing <- setdiff(ebird[, ename], rownames(codes))
keep <- ! ebird[, ename] %in% missing
ebird <- ebird[keep, ]

# Print species not included in codes
missing

# Confirm no species are missing from dates
(miss_date <- setdiff(ebird[, ename], rownames(dates)))  # Should return character(0)

# This will remove species that were in ebird and codes, but not in dates.
# Should these species should be added to dates?
ebird <- ebird[! ebird[[ename]] %in% miss_date, ]

# Add missing column for "F" breeding code
codes[, "F"] <- as.integer(1)


# Error checking ----

# The following 3 lines should return FALSE
any(is.na(dates))
any(is.na(codes))
any(is.na(ebird[, c(ename, ecode, edate)]))  # there are NAs in ecode

# Function to check date format:
# Returns TRUE if all values in x are formatted as, e.g., "10/9/2016"
check_date_format <- function(x) all(grepl("\\d{1,2}/\\d{1,2}/\\d{4}", x))

# Check that dates are all formatted the same
# The following 2 lines should both return TRUE
check_date_format(ebird[edate])
all(vapply(dates[, date_cols], check_date_format, TRUE))


# Set date class ----

# Convert date columns to class date -- allows for comparison of dates without
# using Julian days
dates[, date_cols] <- lapply(dates[, date_cols],
                             function(x) as.Date(x, "%m/%d/%Y"))

ebird[, edate] <- as.Date(ebird[, edate], "%m/%d/%Y")


# Remove space from code ----
ebird[, ecode] <- gsub(" ", "", ebird[, ecode])


# remove observed and flyover ----
keep <- ! (ebird[, ecode] %in% c("", "O", "F") | is.na(ebird[, ecode]))
ebird <- ebird[keep, ]


# evidence code ----
ebird$dv_code <- vapply(seq_len(nrow(ebird)), function(i)
  codes[ebird[i, ename], ebird[i, ecode]], NA_integer_)

# recode low level codes as NA
ebird$dv_code[ebird$dv_code < 3] <- NA_integer_


# date code ----
# temporariliy add date range information to ebird
name_ord <- c(names(ebird), names(dates)[names(dates) != ename])
ebird <- merge(ebird, dates, by.x = ename, by.y = "row.names", all.x = TRUE,
               sort = FALSE)

# incase merge reordered columns
ebird <- ebird[, name_ord]

# add date codes
ebird$dv_date <- NA_character_

# observations before Beginning.E.date
indx <- ebird[, edate] < ebird[, "Beginning.E.date"]
ebird$dv_date[indx] <- "Before E"

# observations after Beginning.E.date, but before Beginning.B.date
indx <- is.na(ebird$dv_date) & ebird[, edate] < ebird[, "Beginning.B.date"]
ebird$dv_date[indx] <- "Before B"

# observations after after Ending.E.date
indx <- is.na(ebird$dv_date) & ebird[, edate] > ebird[, "Ending.E.date"]
ebird$dv_date[indx] <- "After E"

# observations before Ending.E.date, but after Ending.B.date
indx <- is.na(ebird$dv_date) & ebird[, edate] > ebird[, "Ending.B.date"]
ebird$dv_date[indx] <- "After B"

# remove date range information
ebird <- ebird[, setdiff(names(ebird), names(dates)[names(dates) != ename])]


# remove records without potential issues ----
# i.e. remove species that are NA in both date and code
keep <- ! (is.na(ebird$dv_date) & is.na(ebird$dv_code))
ebird <- ebird[keep, ]


# write output ----

write.csv(ebird, file = out_file, row.names = FALSE)
