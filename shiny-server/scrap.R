n_days = 30

cases = rep(2, n_days + 1)
doubling_time = 2
    
for (i in 1:n_days) {
    cases[i+1] = cases[i]*2^(1/doubling_time)
}    


cumulative_cases = cases
los = 40
days_to_hospitalization = 2

get_hospitalizations = function(cumulative_cases, los, doubling_time) {
    
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
    
    return(return_vec)
}


get_hospitalizations = function(cumulative_cases, los, doubling_time) {
    
    # project back to 1, then fill in the rest (if any) with zeros
    back_vec = c(rep(NA, los + days_to_hospitalization), cumulative_cases)
    for (i in (los + days_to_hospitalization):1) {
        back_vec[i] = back_vec[i + 1]/2^(1/doubling_time)
    }
    
    # round to the 12th decimal place for numerical issues
    back_vec = round(back_vec, 12)
    
    # make elements < 1 equal to 0
    if (sum(back_vec < 1) > 0) {
        back_vec[1:max(which(back_vec < 1))] = 0
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
    
    return(return_vec)
}


# we need to subtract off length of stay for everybody up to (days to hospitalization) before the original vector
# and then return index of (days to hospitalization) before the start of the original vector up to length of cumulative_cases
# so we need to project back enough so that (days to hospitalization) before the start of the original vector has a length of stay to subtract off


get_hospitalizations = function(cumulative_cases, los, doubling_time) {
    
    # Project cases backwards LOS days
    cum_cases_w_backwards_projection = c(rep(NA, los + days_to_hospitalization), cumulative_cases)
    for (i in (los + days_to_hospitalization):1) {
        cum_cases_w_backwards_projection[i] = cum_cases_w_backwards_projection[i+1]/2^(1/doubling_time)
    } 
    
    # Calculate hospitalizations from day 1 through projection period
    stopifnot(all.equal(cumulative_cases, cum_cases_w_backwards_projection[(los + days_to_hospitalization + 1):length(cum_cases_w_backwards_projection)]))
    return(cumulative_cases - cum_cases_w_backwards_projection[1:length(cumulative_cases)])
}
