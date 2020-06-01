# helper functions

library(dplyr)

get_initial_conditions <- function(inputs) {
  # using available data, produces an obj that has relevant data
  # input: cohort_df: dataframe with relevant population statistics
  # in our case, often on the county level
  
  #print(inputs$population_df)
  
  # get severity rates, weighted by age group
  severity_rates <- inputs$population_df %>% 
    summarise(
      total_population=sum(population_in_age_group),
      wtd_case_fatality_rate=weighted.mean(case_fatality_rate, population_in_age_group),
      wtd_critical_case_rate=weighted.mean(critical_case_rate, population_in_age_group),
      wtd_severe_cases_rate=weighted.mean(severe_cases_rate, population_in_age_group),
      wtd_hosp_rate=weighted.mean(severe_cases_rate + critical_case_rate, population_in_age_group)
    )
  
  # adjust if input was hospitalizations instead of cases
  if(!inputs$hospitalizations_input_instead_of_cases) {
    # Scale total cases if confirmed cases are given instead of hospitalizations
    num_cases <- inputs$num_cases * inputs$case_scaler
  } else {
    validate(
      need(inputs$num_cases > 0, "To run the model enter a non-zero number of hospitalizations.")
    )
    # Infer total cases from demographics if hospitalizations are given
    num_cases <- inputs$num_cases / severity_rates$wtd_hosp_rate[1]
  }
  
  # combine results to object and return
  model_inputs <- list(
    total_population = severity_rates$total_population[1],
    estimated_fatal_cases = num_cases * severity_rates$wtd_case_fatality_rate[1],
    estimated_critical_cases = num_cases * severity_rates$wtd_critical_case_rate[1],
    estimated_severe_cases = num_cases * severity_rates$wtd_severe_cases_rate[1],
    estimated_case_fatality_rate = severity_rates$wtd_case_fatality_rate[1]
  )
  return(model_inputs)
}
