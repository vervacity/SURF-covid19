# SEIR modelling

library(deSolve)

# NOTE: drawn heavily from Alison Hill at Harvard

set_seir_initial <- function(inputs) {
  # set up SEIR initial conditions
  asympt_to_sympt_ratio <- inputs$fract_asymptomatic / (1 - inputs$fract_asymptomatic)
  exposed_to_mild_i_ratio <- 1 # total guess
  
  
  # inputs
  total_population <- inputs$total_population # total populationn
  initial_fatal <- 0 #inputs$initial_fatal # initial fatal cases (death) <- currently non-functional
  initial_acute <- inputs$initial_acute # initial acute cases (hospitalized)
  initial_icu <- inputs$initial_icu # initial critical cases (icu)
  
  # I
  I_acute <- initial_acute
  I_icu <- initial_icu
  I_mild <- asympt_to_sympt_ratio*(initial_acute + initial_icu)
  I_tot <- I_mild + I_acute + I_icu
  
  # E: estimate based on mild? it's like I_mild but 5 days ago
  E <- exposed_to_mild_i_ratio * I_mild
  
  # S
  S <- total_population - (E + I_tot)
  
  # R
  R <- 0 # endpoint, doesn't need to be accurate for now?
  
  # D
  D <- 0 # initial_fatal # endpoint, doesn't need to be accurate for now
  
  return(c(S,E,I_mild,I_acute,I_icu,R,D))
}

set_seir_odes <- function(t, y, p){
  # defines the differential equation system describing the SEIR model
  # t: time step vector
  # y: states
  # p: named list of param values
  
  # states
  S <- y[1]
  E <- y[2]
  I_mild <- y[3]
  I_acute <- y[4]
  I_icu <- y[5]
  R <- y[6]
  D <- y[7]
  
  # set ODEs
  with(as.list(p),{
    
    dS.dt <- -(b*I_mild + 0*I_acute + 0*I_icu)*S
    dE.dt <- (b*I_mild + 0*I_acute + 0*I_icu)*S - a*E
    dI_mild.dt <- a*E - g1*I_mild - p1*I_mild
    dI_acute.dt <- p1*I_mild - g2*I_acute - p2*I_acute
    dI_icu.dt <- p2*I_acute - (g3+u)*I_icu
    dR.dt <- g1*I_mild + g2*I_acute + g3*I_icu
    dD.dt <- u*I_icu
    
    return(list(c(dS.dt, dE.dt, dI_mild.dt, dI_acute.dt, dI_icu.dt, dR.dt, dD.dt)))
  })
}

run_seir_odes <- function(params, num_days, y0){
  # This function numerically intergrates the system of differential equations for a given set of parameter values, initial conditions, and maximum time
  # INPUT: p- named list of parameter values
  #        Tmax - max time to integrate for
  #        y0 - named list of initial conditions for each variable
  # OUTPUT: Dataframe with rows as timepoints and columns as variables
  
  # time sequence
  t <- seq(from=0, to=num_days, by=1)
  
  # run ODEs
  out <- ode(y=y0, times=t, func=set_seir_odes, parms=params)
  
  # results
  df <- as.data.frame(out)
  return(df)
}

run_seir_odes_with_interventions <- function(params, num_days, interventions, y0) {
  # this function runs interventions at the appropriate times
  
  # first, split up intervals based on the intervention start days
  
  # then run each interval, using the last intervals output as y0 for the next interval

}

dt_to_beta <- function(y0, doubling_time, alpha, total_initial_infectious) {
  # beta: calculate based on doubling time (doubling of E)
  beta <- ((2*y0[2] / doubling_time) + (alpha * y0[2])) / (total_initial_infectious * y0[1])
  return(beta)
}

predict_SEIR <- function(inputs) {
  # use an SEIR model to get predictions
  # utilizes current county level data, some lit-based estimates, and user input
  incubation_period <- 5 # TODO verify. days to symptoms (assumes only infectious when symptomatic, and all E goes to I)
  length_mild_infection <- 14 # TODO verify. complete guess
  inputs$fract_asymptomatic <- 0.3 # TODO verify, this is lit-based estimate (Alison Hill) 
  
  # inputs: initial conditions
  num_days <- inputs$num_days # num days to predict out
  total_population <- inputs$total_population # total populationn
  doubling_time <- inputs$doubling_time # doubling time of total cases
  initial_fatal <- inputs$initial_fatal # initial fatal cases (death)
  initial_acute <- inputs$initial_acute # initial acute cases (hospitalized)
  initial_icu <- inputs$initial_icu # initial critical cases (icu)
  
  # set up initial conditions (S, E, I, R, D)
  y0 <- set_seir_initial(inputs)
  print(y0)
  
  # inputs: model params
  CFR <- inputs$case_fatality_rate # case fatality rate
  length_hospitalization <- inputs$los_acute # length of hospitalization until ICU/recovery
  length_icu <- inputs$los_icu # length of ICU stay until death/recovery
  
  # fractions - need to be determined experimentally?
  total_initial_infectious <- sum(y0[3], y0[4], y0[5])
  fract_mild <- y0[3] / total_initial_infectious # rate of infections that are mild
  fract_severe <- y0[4] / total_initial_infectious # rate of infections that are severe (require hospitalization)
  fract_critical <- y0[5] / total_initial_infectious # rate of infections that are critical (require ICU)
  #fract_mild <- 0.80 # lit-based
  #fract_severe <- 0.15 # lit-based
  #fract_critical <- 0.05 # lit-based
  
  # convert from clinical to SEIR params (Alison Hill, Harvard)
  alpha <- 1/incubation_period
  g1 <- (1/length_mild_infection) * fract_mild # gamma1, rate of progression from I_mild to R
  p1 <- (1/length_mild_infection) - g1 # p1, rate of progression from I_mild to I_acute
  p2 <- (1/length_hospitalization) * (fract_critical/ (fract_severe+fract_critical)) # p2, rate of progression from I_acute to I_icu
  g2 <- (1/length_hospitalization) - p2 # gamma2, rate of progression from I_acute to R
  mu <- (1/length_icu) * (CFR/fract_critical) # rate of progression from I_icu -> death
  g3 <- (1/length_icu) - mu # gamma3, rate of progression from I_icu -> R
  
  # beta: calculate based on doubling time (doubling of E)
  beta <- dt_to_beta(y0, doubling_time, alpha, total_initial_infectious)

  # TODO link up to UI to input doubling time changes
  param_changes <- inputs$param_changes
  
  # set up param object
  params <- list(
    "b"=beta,
    "a"=alpha,
    "g1"=g1,
    "p1"=p1,
    "g2"=g2,
    "p2"=p2,
    "g3"=g3,
    "u"=mu
  )
  print(params)
  
  # run modelling
  seir_predictions <- run_seir_odes(params, num_days, y0)
  print(head(seir_predictions))
  
  # TODO run modelling with intervention
  
  
  # return the following, order in prediction cols is (time,S,E,I,I,I,R,D)
  predictions <- list(
    "fatal" = seir_predictions[,8], 
    "severe" = seir_predictions[,5],
    "critical" = seir_predictions[,6], 
    "severe_without_intervention" = seir_predictions[,5],
    "critical_without_intervention" = seir_predictions[,6]
  )
  return(predictions)
  
}