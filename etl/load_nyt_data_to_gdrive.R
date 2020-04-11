#install.packages('googledrive')
library("googledrive")
library(data.table)

SHARED_FOLDER_ID = "1Trgp8JktFjkAkybEs77lEDdLInVSuWBA"
DRIVE_FILENAME = "nytimes_county_cases_deaths.csv"

# Download from NYT Github
raw_nyt_dt = fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# Write temp file locally
tmpfile = tempfile(DRIVE_FILENAME)
fwrite(raw_nyt_dt, tmpfile)

# Upload to our GDrive
drive_upload(tmpfile, path = as_id(SHARED_FOLDER_ID), name = DRIVE_FILENAME, type = "text/csv", overwrite = TRUE, verbose = TRUE)
