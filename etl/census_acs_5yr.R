rm(list = ls())

library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(statar)


##########################
## load and clean raw data
##########################

raw_data = read.csv('data/nhgis0038_csv/nhgis0038_ds239_20185_2018_county.csv', stringsAsFactors = FALSE)
work_data = raw_data

# get fips
work_data %<>%
    mutate(STATEA = str_pad(STATEA, 2, 'left', '0'),
           COUNTYA = str_pad(COUNTYA, 3, 'left', '0')) %>%
    filter(as.numeric(STATEA) != 72) %>%
    mutate(FIPS = as.numeric(paste0(STATEA, COUNTYA)))
    
    
####################################################
## confirm fips and county names with johannes' data
####################################################

# merge with johannes' data and confirm correct fips by comparing county names
j_data = read.csv('data/county_age_populations_v5.csv', stringsAsFactors = FALSE)

j_data_county_names = j_data %>% 
    select(FIPS, CTYNAME) %>%
    distinct

tmp_work_data = work_data %>% select(FIPS, COUNTY) %>% distinct

jmerge = full_join(j_data_county_names, tmp_work_data, by = 'FIPS')

stopifnot(all.equal(jmerge$CTYNAME, jmerge$COUNTY))


##############
## get buckets
##############

# get buckets from johannes' data
buckets_vec = j_data %>% 
    pull(age_decade) %>% 
    unique

# put things in buckets!
work_data %<>%
    mutate(pop_0_9 = AJWBE003 + AJWBE004 + AJWBE027 + AJWBE028,
           pop_10_19 = AJWBE005 + AJWBE006 + AJWBE007 + AJWBE029 + AJWBE030 + AJWBE031,
           pop_20_29 = AJWBE008 + AJWBE009 + AJWBE010 + AJWBE011 + AJWBE032 + AJWBE033 + AJWBE034 + AJWBE035,
           pop_30_39 = AJWBE012 + AJWBE013 + AJWBE036 + AJWBE037,
           pop_40_49 = AJWBE014 + AJWBE015 + AJWBE038 + AJWBE039,
           pop_50_59 = AJWBE016 + AJWBE017 + AJWBE040 + AJWBE041,
           pop_60_69 = AJWBE018 + AJWBE019 + AJWBE020 + AJWBE021 + AJWBE042 + AJWBE043 + AJWBE044 + AJWBE045,
           pop_70_79 = AJWBE022 + AJWBE023 + AJWBE046 + AJWBE047,
           pop_80 = AJWBE024 + AJWBE025 + AJWBE048 + AJWBE049) %>%
    select(FIPS, STATE, COUNTY, starts_with('pop'))


##################
## long and output
##################

# reshape to long
work_long = work_data %>%
    gather(starts_with('pop'), key = 'age_decade', value = 'pop') %>%
    mutate(age_decade = str_replace_all(age_decade, '^pop_', '')) %>%
    mutate(age_decade = str_replace_all(age_decade, '_', '-')) %>%
    mutate(age_decade = str_replace(age_decade, '^80$', '80+')) %>%
    rename(STNAME = STATE, CTYNAME = COUNTY)

# make sure identifiers match
stopifnot(nrow(work_long) == nrow(j_data))
stopifnot(nrow(inner_join(work_long, j_data, by = c('FIPS', 'STNAME', 'CTYNAME', 'age_decade'))) == nrow(work_long))

# output
write.csv(work_long, 'data/county_age_populations_acs.csv', row.names = FALSE)


###########################################
## comparison with johannes' census numbers
###########################################

# rename
work_long %<>% rename(pop_acs = pop)
j_data %<>% rename(pop_j = pop)

# merge and get diff variable
merged = inner_join(work_long, j_data, by = c('FIPS', 'STNAME', 'CTYNAME', 'age_decade'))

merged %<>% 
    mutate(diff = pop_acs - pop_j) %>%
    arrange(-diff)

# outputs
merged %>% sum_up(diff, d = TRUE)
hist(merged$diff, xlab = 'Population in ACS - population in original')
