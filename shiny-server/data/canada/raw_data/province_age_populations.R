rm(list = ls())

library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(statar)


##########################
## load and clean raw data
##########################

raw_data = read.csv('Population by age and geography.csv', stringsAsFactors = FALSE)
work_data = raw_data

# only one year in data
work_data %<>% select(-Year)

# clean up age groups
work_data %<>% 
    filter(!(str_detect(Age.group, 'All') | str_detect(Age.group, 'to'))) %>%
    filter(!Age.group %in% c('18 years and over', '65 years and over', '90 years and over')) %>%
    mutate(age = as.numeric(str_extract(Age.group, '\\d+')))

# add in pruid
# https://www150.statcan.gc.ca/n1/pub/92-162-g/2015001/tech-eng.htm
work_data %<>%
    rename(province = GEO) %>%
    mutate(pruid = NA) %>%
    mutate(pruid = ifelse(province == 'Newfoundland and Labrador', 10, pruid)) %>%
    mutate(pruid = ifelse(province == 'Prince Edward Island', 11, pruid)) %>%
    mutate(pruid = ifelse(province == 'Nova Scotia', 12, pruid)) %>%
    mutate(pruid = ifelse(province == 'New Brunswick', 13, pruid)) %>%
    mutate(pruid = ifelse(province == 'Quebec', 24, pruid)) %>%
    mutate(pruid = ifelse(province == 'Ontario', 35, pruid)) %>%
    mutate(pruid = ifelse(province == 'Manitoba', 46, pruid)) %>%
    mutate(pruid = ifelse(province == 'Saskatchewan', 47, pruid)) %>%
    mutate(pruid = ifelse(province == 'Alberta', 48, pruid)) %>%
    mutate(pruid = ifelse(province == 'British Columbia', 59, pruid)) %>%
    mutate(pruid = ifelse(province == 'Yukon', 60, pruid)) %>%
    mutate(pruid = ifelse(province == 'Northwest Territories', 61, pruid)) %>%
    mutate(pruid = ifelse(province == 'Nunavut', 62, pruid))


##############
## get buckets
##############

# get buckets from johannes' data
buckets_vec = read.csv('../data/county_age_populations_v5.csv', stringsAsFactors = FALSE) %>% 
    pull(age_decade) %>% 
    unique

# put things in buckets
work_data %<>%
    mutate(age_decade = NA) %>%
    mutate(age_decade = ifelse(age >= 0 & age <= 9, '0-9', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 10 & age <= 19, '10-19', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 20 & age <= 29, '20-29', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 30 & age <= 39, '30-39', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 40 & age <= 49, '40-49', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 50 & age <= 59, '50-59', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 60 & age <= 69, '60-69', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 70 & age <= 79, '70-79', age_decade)) %>%
    mutate(age_decade = ifelse(age >= 80, '80+', age_decade))


######################
## collapse and output
######################

work_data %<>% 
    group_by(province, pruid, age_decade) %>%
    summarise(pop = sum(VALUE)) %>%
    ungroup

# output
write.csv(work_data, 'province_age_populations.csv', row.names = FALSE)
