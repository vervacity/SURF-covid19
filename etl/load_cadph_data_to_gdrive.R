#install.packages('googledrive')
library("googledrive")
library(data.table)

SHARED_FOLDER_ID = "1Trgp8JktFjkAkybEs77lEDdLInVSuWBA"
DRIVE_FILENAME = "ca_county_hospitalizations.csv"

# Download from https://data.chhs.ca.gov/dataset/california-covid-19-hospital-data-and-case-statistics
raw_dt = fread("https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid-19-data.csv")

# Write temp file locally
tmpfile = tempfile(DRIVE_FILENAME)
fwrite(raw_dt, tmpfile)

# Upload to our GDrive
drive_upload(tmpfile, path = as_id(SHARED_FOLDER_ID), name = DRIVE_FILENAME, type = "text/csv", overwrite = TRUE, verbose = TRUE)
