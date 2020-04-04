library(data.table)
library(dplyr)
library(lubridate)

setwd('/Users/angelagu/Desktop/covid/SURF-covid19/shiny-server/')

df1 <- read.csv('data/canada/ideal_data/age_distribution.csv', stringsAsFactors = FALSE)
df2 <- read.csv('data/canada/ideal_data/beds.csv', stringsAsFactors = FALSE)
df3 <- read.csv('data/canada/ideal_data/cases.csv', stringsAsFactors = FALSE)

df <- merge(df1, merge(df2, df3))

cond_rates = fread('data/imperial_rates.csv')

cond_rates = cond_rates[,.(
  age_decade = age_group,
  hospitalizations_per_case = hospitalization_given_symptomatic,
  severe_cases_rate = (1-critical_care_given_hospitalization)*hospitalization_given_symptomatic,
  critical_case_rate = critical_care_given_hospitalization*hospitalization_given_symptomatic,
  case_fatality_rate = fatality_given_symptomatic
)]

View(merge(df, cond_rates))
View(merged_pop_fat_rate)