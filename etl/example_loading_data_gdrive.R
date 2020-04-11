#install.packages('googledrive')
library("googledrive")
library(data.table)

SHARED_FOLDER_ID = "1Trgp8JktFjkAkybEs77lEDdLInVSuWBA"
NAME_OF_FILE = "nytimes_county_cases_deaths.csv"

file_id = drive_find(
  q = paste0("'", as_id(SHARED_FOLDER_ID), "' in parents"),
  pattern = NAME_OF_FILE
)[1, ]

tmpfile = tempfile(NAME_OF_FILE)
drive_download(file_id, tmpfile)

dt = fread(tmpfile)