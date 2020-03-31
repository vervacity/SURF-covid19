library(shiny)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)

setwd("/Users/angelagu/Desktop/covid/SURF-covid19/shiny-server")

df <- read.csv('county_age_severity_rates_v6.csv', stringsAsFactors = FALSE)
df$County <- gsub('city', 'City', df$County)
acute_beds_dt = fread('acute_byFIPS.csv')
icu_beds_dt = fread('icu_byFIPS.csv')
bed_dt = merge(acute_beds_dt, icu_beds_dt, by = "FIPS")

county_case_history <- tryCatch(
  # {read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv", stringsAsFactors = FALSE)},
  {read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", stringsAsFactors = FALSE)},
  error = function(cond) {return(NA)})
if (!is.na(county_case_history)) {
  # most_recent_col = ncol(county_case_history)
  # county_cases <- county_case_history[, c(1,2, most_recent_col)]
  # while (sum(!is.na(county_cases[, c(ncol(county_cases))])) == 0) {
  #   most_recent_col = most_recent_col - 1
  #   county_cases <- county_case_history[, c(1,2, most_recent_col)]
  # }
  # county_cases <- county_cases %>% rename_at(vars(colnames(county_cases)), ~ c("FIPS", 'County', 'Cases')) %>%
  #   filter(FIPS != 0) %>% mutate(FIPS = as.numeric(FIPS)) %>% select(FIPS, Cases)
  county_case_history <- county_case_history %>% mutate(fips = as.integer(fips), date = as.Date(date))
  county_cases <- left_join(county_case_history %>% group_by(fips) %>% summarize(date = max(date)), county_case_history) %>% 
    rename(FIPS = fips, Cases = cases) %>% select(FIPS, Cases)
  df <- left_join(df, county_cases, by = 'FIPS')
}

df <- left_join(df, bed_dt, by = 'FIPS')


# PARAMETERS

state1 <- "California"
state_all_selector <- TRUE
county1 <- "Santa Clara County"
input_radio <- 1 # select 1 if all cases, 2 if hospitalizations
doubling_time <- 6
num_cases <- 1000
case_scaler <- 5
num_days <- 30
los_critical <- 12
los_severe <- 7
day_change_1 <- NA
double_change_1 <- NA
total_acute_beds <- 1000
total_icu_beds <- 500

get_dt_changes <- function() {
  return(c())
  # if interventions: return c(double1, day1, double2, day2, ...)
}

# FUNCTIONS
get_county_df <- function(state1, state_all_selector, county1 = NA) {
  state <- state1
  state_df <- df %>% filter(State == state)
  counties <- county1
  if (!state_all_selector) {
    return(state_df %>% filter(County %in% counties))
  } else {
    return (state_df)
  }
}

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

get_naive_estimations <- function(state1, state_all_selector, county1,
                                  input_radio, doubling_time, num_cases, case_scaler) {
  
  county_df <- get_county_df(state1, state_all_selector, county1)
  
  hospitalizations_input_instead_of_cases <- (input_radio == 2)

  combined_counties_severity_rates <- county_df %>% 
    summarise(
      total_population = sum(population_in_age_group),
      wtd_case_fatality_rate = weighted.mean(case_fatality_rate, population_in_age_group),
      wtd_critical_case_rate = weighted.mean(critical_case_rate, population_in_age_group),
      wtd_severe_cases_rate = weighted.mean(severe_cases_rate, population_in_age_group),
      wtd_hosp_rate = weighted.mean(severe_cases_rate + critical_case_rate, population_in_age_group)
    )
  
  if(!hospitalizations_input_instead_of_cases) {
    # Scale total cases if confirmed cases are given instead of hospitalizations
    num_cases <- num_cases * case_scaler
  } else {
    # Infer total cases from demographics if hospitalizations are given
    num_cases <- num_cases / combined_counties_severity_rates$wtd_hosp_rate[1]
  }
  
  return(list(
    total_population = combined_counties_severity_rates$total_population[1],
    estimated_fatal_cases = num_cases * combined_counties_severity_rates$wtd_case_fatality_rate[1],
    estimated_critical_cases = num_cases * combined_counties_severity_rates$wtd_critical_case_rate[1],
    estimated_severe_cases = num_cases * combined_counties_severity_rates$wtd_severe_cases_rate[1]
  ))
}

get_case_numbers <- function(state1, state_all_selector, county1,
                             input_radio, doubling_time, num_cases, case_scaler,
                             num_days, los_critical, los_severe) {
  
  naive_estimations <- get_naive_estimations(state1, state_all_selector, county1,
                                             input_radio, doubling_time, num_cases, case_scaler)
  
  n_days <- num_days
  fatal_cases <- rep(naive_estimations$estimated_fatal_cases, n_days+1)
  critical_cases <- rep(naive_estimations$estimated_critical_cases, n_days+1)
  severe_cases <- rep(naive_estimations$estimated_severe_cases, n_days+1)

  critical_without_intervention = critical_cases
  severe_without_intervention = severe_cases
  for (i in 1:n_days) {
    critical_without_intervention[i+1] = critical_without_intervention[i]*2^(1/doubling_time)
    severe_without_intervention[i+1] = severe_without_intervention[i]*2^(1/doubling_time)
  }

  critical_hospitalizations_obj = get_hospitalizations(critical_without_intervention, los_critical, doubling_time)
  critical_without_intervention = critical_hospitalizations_obj$result
  severe_hospitalizations_obj = get_hospitalizations(severe_without_intervention, los_severe, doubling_time)
  severe_without_intervention = severe_hospitalizations_obj$result
  
  # store the backwards projected numbers to append onto the intervened vectors
  critical_back_vec = critical_hospitalizations_obj$back_vec
  severe_back_vec = severe_hospitalizations_obj$back_vec
  
  # cases with intervention
  dt_changes = get_dt_changes()
  
  for (i in 1:n_days) {
    if (length(dt_changes) > 0) {
      days <- dt_changes[c(FALSE, TRUE)]
      if ((i-1) %in% days) {
        doubling_time <- dt_changes[2*min(which(days == i-1))-1]
      }
    }
    # cases[i+1] = cases[i]*2^(1/doubling_time)
    fatal_cases[i+1] = fatal_cases[i]*2^(1/doubling_time)
    critical_cases[i+1] = critical_cases[i]*2^(1/doubling_time)
    severe_cases[i+1] = severe_cases[i]*2^(1/doubling_time)
  }
  
  # no backwards projection here; tack on back_vec from before and subtract accordingly
  critical_cases = c(critical_back_vec, critical_cases)
  critical_cases = critical_cases[(los_critical + 1):(los_critical + n_days + 1)] - critical_cases[1:(n_days + 1)]
  
  severe_cases = c(severe_back_vec, severe_cases)
  severe_cases = severe_cases[(los_severe + 1):(los_severe + n_days + 1)] - severe_cases[1:(n_days + 1)]

  total_population <- naive_estimations$total_population
  validate(
    need((severe_cases[n_days+1] + critical_cases[n_days + 1]) < 0.2*total_population, 
         "Current data are insufficient to reliably model infection rates this high. The model will be updated as more data become available. To proceed, reduce the initial number; or reduce the days to model; or increase the doubling time.")
  )
  
  return_cases <- list(
    "fatal" = fatal_cases, 
    "critical" = critical_cases, 
    "severe" = severe_cases,
    "critical_without_intervention" = critical_without_intervention,
    "severe_without_intervention" = severe_without_intervention)
  return(return_cases)
}

# Function to get data table with time series of cases (used for both graphical and tabular representation)
get_time_series_dt <- function(state1, state_all_selector, county1,
                               input_radio, doubling_time, num_cases, case_scaler,
                               num_days, los_critical, los_severe) {
  
  case_numbers <- get_case_numbers(state1, state_all_selector, county1,
                                   input_radio, doubling_time, num_cases, case_scaler,
                                   num_days, los_critical, los_severe)
  
  n_days <- num_days
  day_list <- c(0:n_days)
  
  chart_data = data.table(
    Date = Sys.Date() + day_list,
    `Estimated<br />Acute Hospitalizations<br />(without intervention)` = round(case_numbers$severe_without_intervention),
    `Estimated<br />ICU Hospitalizations<br />(without intervention)` = round(case_numbers$critical_without_intervention)
  )
  
  if (length(get_dt_changes()) > 0) {
    chart_data[,`:=`(
      `Estimated<br />Acute Hospitalizations<br />(with intervention)` = round(case_numbers$severe),
      `Estimated<br />ICU Hospitalizations<br />(with intervention)` = round(case_numbers$critical)
    )]
  } else {
    chart_data[,`:=`(
      `Estimated<br />Total Hospitalizations<br />(without intervention)` =
        `Estimated<br />Acute Hospitalizations<br />(without intervention)` + `Estimated<br />ICU Hospitalizations<br />(without intervention)`)]
  }
  
  chart_data
}

plot1 <- function(state1, state_all_selector, county1,
                  input_radio, doubling_time, num_cases, case_scaler,
                  num_days, los_critical, los_severe,
                  total_acute_beds, total_icu_beds) {

  num_acute_beds_available = total_acute_beds
  num_icu_beds_available = total_icu_beds
  num_total_beds_available = num_acute_beds_available + num_icu_beds_available
  
  n_days <- num_days
  y_axis <- 'Cases'
  
  chart_data = melt(get_time_series_dt(state1, state_all_selector, county1,
                                       input_radio, doubling_time, num_cases, case_scaler,
                                       num_days, los_critical, los_severe), id.vars = c('Date'))
  
  ymax = chart_data[,max(value)]
  
  gp = ggplot(chart_data,
              aes(x=Date, y=value, group=variable, text = sprintf("Date:  %s \n cases: %i", Date, value))) +
    geom_line(aes(linetype = variable, color = variable)) +  guides(linetype=FALSE) + guides(size=FALSE) +
    scale_color_manual(values=c("dodgerblue", "red", "black")) +
    scale_linetype_manual(values=c("solid", "solid", "solid")) +
    theme_minimal() +
    ylab("Number of cases") + xlab('Date')  +
    coord_cartesian(ylim=c(0, ymax)) +
    scale_x_date(name="Date", labels = date_format("%b %d",tz = "EST")) +
    ggtitle("Daily number of people hospitalized for COVID-19 (not cumulative)")
  
  if (length(get_dt_changes()) > 0) {
    dt_changes = get_dt_changes()
    days <- dt_changes[c(FALSE, TRUE)]
    
    gp = gp + scale_color_manual(values=c("dodgerblue", "red", "dodgerblue", "red")) +
      scale_linetype_manual(values=c("dashed", "dashed", "solid", "solid"))
    
    for (i in days) {
      gp = gp +
        geom_vline(xintercept = as.numeric(Sys.Date() + i), color = 'grey', linetype = 'dotted') +
        annotate("text", x = Sys.Date() + i, y = ymax, size = 3, color = 'gray35',
                 label = "Intervention")
    }
  }

  if(is.finite(num_acute_beds_available)) {
    gp = gp +
      geom_hline(yintercept = num_acute_beds_available, linetype = "dashed", color = 'grey') + 
      annotate("text", x = Sys.Date() + 0.75*n_days, y = num_acute_beds_available*1.02, label = "Acute Beds for COVID Patients", vjust=1, hjust=0, size = 3, color = 'gray35')
  }
  
  if(is.finite(num_icu_beds_available)) {
    gp = gp +
      geom_hline(yintercept = num_icu_beds_available, linetype = "dashed", color = 'grey') + 
      annotate("text", x = Sys.Date() + 0.75*n_days, y = num_icu_beds_available*1.02, label = "ICU Beds for COVID Patients", vjust=1, hjust=0, size = 3, color = 'gray35')
  }
  
  ggplotly(gp, tooltip = 'text', height = 640) %>% 
    layout(
      legend = list(x = 0.02, y = 0.9, bgcolor = 'rgba(0,0,0,0)'),
      xaxis=list(fixedrange=TRUE),
      yaxis=list(fixedrange=TRUE)) %>% 
    config(displayModeBar = F)
  
}

plot1(state1, state_all_selector, county1,
                        input_radio, doubling_time, num_cases, case_scaler,
                        num_days, los_critical, los_severe,
                        total_acute_beds, total_icu_beds)
