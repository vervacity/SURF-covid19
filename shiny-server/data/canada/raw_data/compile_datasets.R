library(data.table)
library(dplyr)
library(lubridate)

setwd('/Users/angelagu/Desktop/covid/SURF-covid19/shiny-server')

canada_df <- read.csv('data/canada/province_age_severity_rates.csv', stringsAsFactors = FALSE)

beds <- read.csv('data/canada/beds-staffed-and-in-operation-2017-2018-en-web-revmay3.csv', stringsAsFactors = FALSE)
beds <- beds[grep('G', beds$Type.of.hospital), ] %>% mutate(num_acute_beds = as.numeric(Other.Acute), num_icu_beds = as.numeric(Intensive.Care)) %>% 
  mutate(Province = ifelse(Province == 'AB', 'Alberta',
                           ifelse(Province == 'BC', 'British Columbia',
                                  ifelse(Province == 'MB', 'Manitoba',
                                         ifelse(Province == 'NB', 'New Brunswick',
                                                ifelse(Province == 'NL', 'Newfoundland and Labrador',
                                                       ifelse(Province == 'NS', "Nova Scotia", 
                                                              ifelse(Province == 'NT', 'Northwest Territories',
                                                                     ifelse(Province == 'ON', 'Ontario', 
                                                                            ifelse(Province == 'PE', 'Prince Edward Island',
                                                                                   ifelse(Province == 'SK', 'Saskatchewan',
                                                                                      ifelse(Province == 'YT', 'Yukon', Province)))))))))))) %>% 
  select(Province, num_acute_beds, num_icu_beds) %>% group_by(Province) %>% summarize(num_acute_beds = sum(num_acute_beds, na.rm = TRUE), num_icu_beds = sum(num_icu_beds, na.rm = TRUE))

canada_df <- left_join(canada_df, beds, by = c('province' = 'Province'))

canada_case_history <- tryCatch(
  {read.csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", stringsAsFactors = FALSE)},
  error = function(cond) {return(NA)})

if (!is.na(canada_case_history)) {
  canada_case_history <- data.table(canada_case_history %>% mutate(date = as.Date(date, "%d-%m-%y")) %>% 
                                      select(province = prname, date, Cases = numconf))
  cases <- (canada_case_history[ , .SD[which.max(date)], by = province])[, c('province', 'cases')]
}

View(canada_df)

fwrite(canada_df, file = 'canada_df.csv')
