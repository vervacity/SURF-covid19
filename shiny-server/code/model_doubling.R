# code for modelling/predictions using a doubling model


# Function to get hospitalizations from cumulative cases, with projection backwards from current cases to prevent jump at day LOS
get_hospitalizations <- function(predictions, los, doubling_time) {
  # get hospitalizations: start with case total predictions, length of stay (los), and doubling time
  # first, project backwards for (length of stay) days with appropriate doubling time,
  # when shifted to match day 0, this gives the num cases that should have be resolved by day 0.
  # this can be thought of as the resolved cases predictions.
  # so (total cases) - (resolved cases) = hospitalizations.
  
  days_to_hospitalization = 0
  
  # project back los + days to hospitalization days
  back_vec = c(rep(NA, los + days_to_hospitalization), predictions)
  for (i in (los + days_to_hospitalization):1) {
    back_vec[i] = back_vec[i + 1]/2^(1/doubling_time)
  }
  
  # get indices of original vectors
  original_start = los + days_to_hospitalization + 1
  original_end = los + days_to_hospitalization + length(predictions)
  stopifnot(all.equal(back_vec[original_start:original_end], predictions))
  stopifnot(length(back_vec) == original_end)
  
  # get indices of vectors shifted by days to hospitalization
  shifted_start = original_start - days_to_hospitalization
  shifted_end = original_end - days_to_hospitalization
  
  # subtract off for length of stay
  return_vec = back_vec[shifted_start:shifted_end] - back_vec[(shifted_start - los):(shifted_end - los)]
  
  # keep both the prediction and the back_vec
  
  return(list(result = return_vec, back_vec = back_vec[1:los]))
}


run_doubling <- function(initial_num, doubling_time, num_days) {
  # helper function: generate a vector of predictions (cases/day)
  
  # initialize vector
  predictions <- rep(initial_num, num_days+1)
  
  # update each day
  for (i in 1:num_days) {
    predictions[i+1] <- predictions[i] * 2^(1/doubling_time)
  }
  
  return(predictions)
}

run_doubling_with_dt_changes <- function(initial_num, orig_doubling_time, new_doubling_times, num_days) {
  # helper function: generate a vector of predictions (cases/day)
  # where the doubling time changes through the course
  
  # initialize vector and doubling time
  predictions <- rep(initial_num, num_days+1)
  dt <- orig_doubling_time
  
  # double each day, accounting for doubling time changes
  for (i in 1:num_days) {
    
    # if there are changes in doubling time:
    if (length(new_doubling_times) > 0) {
      # pull the day values, and check if we're on a day that had a dt change
      dt_changed_days <- new_doubling_times[c(FALSE, TRUE)]
      if ((i-1) %in% dt_changed_days) {
        # change doubling time
        dt <- new_doubling_times[2*min(which(dt_changed_days == i-1))-1]
      }
    }
    
    # with updated doubling time, update day's cases
    predictions[i+1] <- predictions[i] * 2^(1/dt)
  }
  
  return(predictions)
}

predict_doubling <- function(inputs) {
  # this model just doubles the results
  
  # first validate necessary inputs
  # initial conditions must have: num_days, 1) acute cases, 2) ICU cases
  
  # inputs
  total_population <- inputs$total_population # total population
  num_days <- inputs$num_days # num days to predict out
  doubling_time <- inputs$doubling_time # doubling time of cases
  initial_fatal <- inputs$initial_fatal # initial fatal cases (death)
  initial_acute <- inputs$initial_acute # initial acute cases (hosptialized)
  los_acute <- inputs$los_acute # length of stay for acute cases
  initial_icu <- inputs$initial_icu # initial critical cases (icu)
  los_icu <- inputs$los_icu # length of stay for critical cases (icu)
  dt_changes <- inputs$dt_changes # doubling time changes
  
  # predict cases based on doubling, NO intervention
  case_predictions_acute <- run_doubling(initial_acute, doubling_time, num_days)
  case_predictions_icu <- run_doubling(initial_icu, doubling_time, num_days)
  
  # convert cases to hospitalizations
  # TODO only predict forward? the backwards calc is clever but doesn't work for SEIR or other models
  # maybe consider calculating from length of stay days ago?
  
  # number hospitalized at any one time (without intervention)
  severe_hospitalizations_obj = get_hospitalizations(case_predictions_acute, los_acute, doubling_time)
  severe_without_intervention = severe_hospitalizations_obj$result
  critical_hospitalizations_obj = get_hospitalizations(case_predictions_icu, los_icu, doubling_time)
  critical_without_intervention = critical_hospitalizations_obj$result

  # store the backwards projected numbers to append onto the intervened vectors (for calc hospitalizations)
  critical_back_vec = critical_hospitalizations_obj$back_vec
  severe_back_vec = severe_hospitalizations_obj$back_vec  
  
  # run interventions
  fatal_cases <- run_doubling(initial_fatal, doubling_time, num_days)
  severe_cases <- run_doubling_with_dt_changes(initial_acute, doubling_time, dt_changes, num_days)
  critical_cases <- run_doubling_with_dt_changes(initial_icu, doubling_time, dt_changes, num_days)
  
  # no backwards projection here; tack on back_vec from before and subtract accordingly
  critical_cases = c(critical_back_vec, critical_cases)
  critical_cases = critical_cases[(los_icu + 1):(los_icu + num_days + 1)] - critical_cases[1:(num_days + 1)]
  severe_cases = c(severe_back_vec, severe_cases)
  severe_cases = severe_cases[(los_acute + 1):(los_acute + num_days + 1)] - severe_cases[1:(num_days + 1)]
  
  # confirm model validity
  validate(
    need((severe_cases[num_days+1] + critical_cases[num_days + 1]) < 0.2*total_population, 
         "Current data are insufficient to reliably model infection rates this high. The model will be updated as more data become available. To proceed, reduce the initial number; or reduce the days to model; or increase the doubling time.")
  )
  
  # return prediction vectors
  return_cases <- list(
    "fatal" = fatal_cases, 
    "critical" = critical_cases, 
    "severe" = severe_cases,
    "critical_without_intervention" = critical_without_intervention,
    "severe_without_intervention" = severe_without_intervention)
  return(return_cases)
    
}


