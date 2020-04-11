#install.packages('googledrive')
library("googledrive")
library(data.table)

SHARED_FOLDER_ID = "10jmjZNgtuAVibGhsO_ym2jvMzs1a87Cv"
OUTPUT_NAMES_AND_URLS = list(
  "README.md" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/out_of_home_activity/README.md",
  "list_of_columns.md" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/list_of_columns.md",
  "grocery_visits.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/out_of_home_activity/grocery_visits.csv",
  "healthcare_visits.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/out_of_home_activity/healthcare_visits.csv",
  "hospital_visits.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/out_of_home_activity/hospital_visits.csv",
  "poi_visits.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/out_of_home_activity/poi_visits.csv",
  "interventions.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/interventions.csv",
  "counties_only.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/counties_only.csv",
  "states_only.csv" = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/states_only.csv"
)


# Download each file and upload to GDrive
for (file in names(OUTPUT_NAMES_AND_URLS)) {
  print(file)
  tmpfile = tempfile(file)
  
  if(grepl(".csv", file)) {
    print('csv')
    dt = fread(OUTPUT_NAMES_AND_URLS[[file]])
    fwrite(dt, tmpfile)
    drive_upload(tmpfile, path = as_id(SHARED_FOLDER_ID), name = file, type = "text/csv", overwrite = TRUE, verbose = TRUE)  
    
  } else {
    download.file(OUTPUT_NAMES_AND_URLS[[file]],
                  tmpfile)  
    drive_upload(tmpfile, path = as_id(SHARED_FOLDER_ID), name = file, overwrite = TRUE, verbose = TRUE)  
  }
  
  
  Sys.sleep(2)
}
