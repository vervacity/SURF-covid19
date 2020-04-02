library(shiny)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)
library(usmap)
library(scales)

# code stolen from Johannes https://rpubs.com/jferstad/ca_county_covid_hospitalizations

start_date = '2020-03-30'
num_days_to_project = 100
doubling_times = c(7,10,14)
ICU_LOS = 9
ACUTE_LOS = 7

get_df <- function(version = 'v1') { # version can be 'v1' or 'v2'
  df <- read.csv('data/county_age_severity_rates_v6.csv', stringsAsFactors = FALSE)
  df$County <- gsub('city', 'City', df$County)
  df = df %>% filter(State == "California") %>% group_by(FIPS, County) %>%
    summarise(
      total_population = sum(population_in_age_group),
      wtd_critical_case_rate =  weighted.mean(critical_case_rate, population_in_age_group),
      wtd_acute_case_rate =  weighted.mean(severe_cases_rate, population_in_age_group)) %>%
    mutate(
      wtd_prop_hosp_in_icu = wtd_critical_case_rate/(wtd_critical_case_rate+wtd_acute_case_rate)) %>%
    select(County, total_population, wtd_prop_hosp_in_icu)
  df = df %>% mutate(County = gsub(' County', '', County))
  
  if (version == 'v1') {
    acute_beds_dt = fread('data/acute_byFIPS.csv')
    icu_beds_dt = fread('data/icu_byFIPS.csv')
    bed_dt = merge(acute_beds_dt, icu_beds_dt, by = "FIPS")
    df_w_beds = merge(df, bed_dt, by="FIPS", all.x = TRUE)
    
    hosp = fread('state_county_request/data/nigam_hospital_data_0330.csv')[,.(County, original_hosp = `COVID-19 Positive Patients March 30 2020`)]
    df_w_beds_hosp = as.data.table(merge(df_w_beds, hosp, by = 'County', all.x = TRUE))
  } else {
    acute_beds_dt = fread('data/acute_byFIPS.csv')
    df_w_beds = merge(df, acute_beds_dt, by = "FIPS")

    hosp = fread('state_county_request/data/nigam_hospital_data_0330.csv')[,.(County, original_hosp = `COVID-19 Positive Patients March 30 2020`,
                                                         num_icu_beds = `Available ICU beds March 30 2020`)]
    df_w_beds_hosp = as.data.table(merge(df_w_beds, hosp, by = 'County', all.x = TRUE))
  }
  return(df_w_beds_hosp)
}

# function from Shiny app
get_hospitalizations = function(cumulative_cases, los, doubling_time) {
  
  days_to_hospitalization = 0
  
  # project back los + days to hospitalization days
  back_vec = c(rep(NA, los + days_to_hospitalization), cumulative_cases)
  for (i in (los + days_to_hospitalization):1) {
    back_vec[i] = back_vec[i + 1]/2^(1/doubling_time)
  }
  
  # get indices of original vectors
  original_start = los + days_to_hospitalization + 1
  original_end = los + days_to_hospitalization + length(cumulative_cases)
  stopifnot(all.equal(back_vec[original_start:original_end], cumulative_cases))
  stopifnot(length(back_vec) == original_end)
  
  # get indices of vectors shifted by days to hospitalization
  shifted_start = original_start - days_to_hospitalization
  shifted_end = original_end - days_to_hospitalization
  
  # subtract off for length of stay
  return_vec = back_vec[shifted_start:shifted_end] - back_vec[(shifted_start - los):(shifted_end - los)]
  
  return(list(result = return_vec, back_vec = back_vec[1:los]))
}

get_time_series <- function(version = 'v1') {
  df_w_beds_hosp <- get_df(version)
  
  dts_cross_dates = as.data.table(expand.grid(
    doubling_times,
    as.Date(start_date) + seq(0, num_days_to_project)
  ))
  setnames(dts_cross_dates, c('doubling_time', 'date'))
  
  long_dt = merge(
    df_w_beds_hosp[original_hosp>0][, i:=1],
    dts_cross_dates[,i:=1],
    by = 'i',
    allow.cartesian = TRUE
  )[,i:=NULL]
  
  long_dt[, days_since_start_date := as.numeric(date - as.Date(start_date)), by=.I]
  long_dt[, est_hosp := original_hosp * 2^(days_since_start_date/doubling_time)]
  long_dt[, `:=`(
    est_acute_hosp = (1-wtd_prop_hosp_in_icu) * est_hosp,
    est_icu_hosp = wtd_prop_hosp_in_icu * est_hosp
  )]
  
  
  long_dt[,icu_census := get_hospitalizations(est_icu_hosp, ICU_LOS, doubling_time=6), by = c('County', 'doubling_time')]
  long_dt[,acute_census := get_hospitalizations(est_acute_hosp, ACUTE_LOS, doubling_time=6), by = c('County', 'doubling_time')]
  
  return(long_dt)
}

# Original code begins here

get_plot <- function(icu_or_acute, version = 'v1') { # icu_or_acute can be "ICU" or "Acute"
  long_dt <- get_time_series(version)
  
  if (icu_or_acute == 'ICU') {
    df <- data.table(long_dt %>% filter(icu_census >= 0.9*num_icu_beds) %>% 
                       select(FIPS, num_icu_beds, icu_census, days_since_start_date) %>% rename(fips = FIPS)) 
    
  } else {
    df <- data.table(long_dt %>% filter(acute_census >= 0.9*num_acute_beds) %>% 
                 select(FIPS, num_acute_beds, acute_census, days_since_start_date) %>% rename(fips = FIPS)) 
  }
  df <- df[ , .SD[which.min(days_since_start_date)], by = fips]
 
  geom_args <- list()
  geom_args[["color"]] <- "black"
  geom_args[["size"]] <- 0.2
  map_df <- map_with_data(df, values = "days_since_start_date", include = "California") %>% mutate(group = county)
  geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, y = map_df$y,
                                         group = map_df$group, fill = map_df[, "days_since_start_date"])
  polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)
  
  if (icu_or_acute == 'ICU') {
    p <- ggplot(data = map_df, aes(text = paste0(county, '\n', num_icu_beds, ' beds \n',
                                                 days_since_start_date, ' days remaining \n',
                                                 as.Date(start_date) + days_since_start_date)))
  } else {
    p <- ggplot(data = map_df, aes(text = paste0(county, '\n', num_acute_beds, ' beds \n',
                                               days_since_start_date, ' days remaining \n',
                                               as.Date(start_date) + days_since_start_date)))
  }
   
  p <- p + polygon_layer + ggplot2::coord_equal() +
    scale_fill_viridis(option='inferno') +
    theme(legend.position = "right") + labs(title = paste0("Days to 90% Utilization (", icu_or_acute, " Beds)"), fill = paste("Number of Days \n from", start_date)) + theme_void() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "white"))
  
  return(p)
}
