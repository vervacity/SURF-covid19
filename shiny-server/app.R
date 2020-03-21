#
# This is a Shiny web application on MatrixDS. 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shiny_project <- ""
prod <- paste0("/srv/shiny-server/", shiny_project)
dev <- paste0("~/shiny-server/", shiny_project)
if (!dir.exists(prod) & !dir.exists(dev)) {
  message(" using getwd() for shiny_path")
  shiny_path <- getwd()
} else {
  .libPaths(c(.libPaths(), "/srv/.R/library"))
  options(java.parameters = "-Xmx8048m")
  if (dir.exists(prod)) message("prod"); shiny_path <- prod
  if (dir.exists(dev)) message("dev"); shiny_path <- dev
}

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)
library(usmap)

df <- read.csv('county_age_severity_rates_v6.csv', stringsAsFactors = FALSE)
df$County <- gsub('city', 'City', df$County)
bed_df <- read.csv('Hospitals.csv', stringsAsFactors = FALSE) %>%
  filter(BEDS != -999, STATE != 'PR') %>% 
  filter(TYPE %in% c('CRITICAL ACCESS', 'MILITARY', 'SPECIAL', 'GENERAL ACUTE CARE')) %>% 
  select(COUNTYFIPS, BEDS) %>% rename(FIPS = COUNTYFIPS, beds = BEDS)
bed_df <- bed_df %>% group_by(FIPS) %>%
  summarize(num_beds = sum(beds)) %>% mutate(FIPS = as.numeric(FIPS))
county_cases <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
county_cases <- county_cases[, c(1,2, ncol(county_cases))]
county_cases <- county_cases %>% rename_at(vars(colnames(county_cases)), ~ c("FIPS", 'County', 'Cases')) %>% 
  filter(FIPS != 0) %>% mutate(FIPS = as.numeric(FIPS)) %>% select(FIPS, Cases)
df <- left_join(df, bed_df, by = 'FIPS')
df <- left_join(df, county_cases, by = 'FIPS')

ui <- shinyUI(
  
  navbarPage("Projecting Severe Cases of COVID-19",
             
             tabPanel("Calculator",
                      sidebarPanel(
                        fluidRow(
                          column(12, 
                                 uiOutput("state_selector_1"),
                                 uiOutput("county_selector_1"),
                                 HTML("<b>Estimated Doubling Time for Cases Requiring Hospitalization (Days)</b>"),
                                 p("To generate a forecast, enter a doubling time below."),
                                 numericInput("doubling_time", NULL, value = NA, min = 1, max = 20),
                                 hr(),
                                 #h4("User Inputs"),
                                 uiOutput("num_cases"),
                                 sliderInput("num_days", "Number of Days to Model Ahead", 30, min = 1, max = 60),
                                 numericInput("los_severe", "Length of Stay (Days) for Severe", 11, min = 1, max = 90),
                                 numericInput("los_critical", "Length of Stay (Days) for Critical", 13, min = 1, max = 90),
                                 sliderInput("prop_bed_for_covid", "% of Beds for COVID-19 Cases", 50, min = 0, max = 100),
                                 actionButton("reset", "Reset to default user inputs"),
                                 hr(),
                                 h4("Simulation of Intervention"),
                                 p("To simulate the impact of an intervention, enter a date and a new doubling time."),
                                 fluidRow(
                                   column(6, tags$b("On Day")),
                                   column(6, tags$b("New DT"))
                                 ),
                                 fluidRow(
                                   column(6,
                                          numericInput("day_change_1", "", NA, min = 1) #,
                                          #numericInput("day_change_2", "", NA, min = 1),
                                          #numericInput("day_change_3", "", NA, min = 1)
                                   ),
                                   column(6,
                                          numericInput("double_change_1", "", NA, min = 1) #,
                                          #numericInput("double_change_2", "", NA, min = 1),
                                          #numericInput("double_change_3", "", NA, min = 1)
                                   ),
                                   column(12, style="display:center-align",
                                          actionButton("submit", "Submit DT changes"),
                                          actionButton("clear", "Clear DT changes")
                                   )
                                 )
                          )
                        ),
                        width = 3
                      ),
                      mainPanel(
                        HTML("<h3>This is a planning tool, not a prediction. To generate a forecast, enter the doubling time for your region (and modify the total confirmed number of COVID-19 cases if necessary). See the Documentation tab for methodology.</h3>"
                        ),
                        hr(),
                        htmlOutput("text1"),
                        tags$head(tags$style("ul, li {margin-left: 0.5em; padding-left: 0; font-size: 18px}")),
                        tags$head(tags$style("li {margin-top: 1em;}")),
                        br(),
                        tableOutput("table1"),
                        hr(),
                        plotlyOutput("plot1"),
                        hr(),
                        plotlyOutput("plot2"),
                        width = 9
                      )
             ),
             
             tabPanel("Nationwide Heatmap",
                      mainPanel(
                        img(src = "usmap.png")
                      )
             ),
             
             # tabPanel("Tuite and Fisman (2020)",
             #          sidebarPanel(
             #            sliderInput("serial_interval", "Serial interval (days)",
             #                        min = 5, max = 10,
             #                        value = 7),
             #            
             #            sliderInput("start_date_outbreak", "Outbreak start date",
             #                        min = ymd('2019-11-01'), max = ymd('2019-12-31'),
             #                        value = ymd('2019-12-01')),
             #            
             #            sliderInput("start_date_control", "Control start date",
             #                        min = ymd('2020-01-01'), max = ymd('2020-01-31'),
             #                        value = ymd('2020-01-15')),
             #            
             #            sliderInput("it0", "Initial number of cases",
             #                        min = 1, max = 40,
             #                        value = 1),
             #            
             #            sliderInput("r0", "Basic reproductive number",
             #                        min = 1.5, max = 4,
             #                        value = 2.3),
             #            
             #            sliderInput("re", "Effective reproductive number with control",
             #                        min = 0.5, max = 3,
             #                        value = 1.5)
             #            
             #            # sliderInput("num_beds", "Number of hospital beds",
             #            #             min = 0, max = 300000,
             #            #             value = 1000)
             #          ),
             #          mainPanel(
             #            plotOutput("plot_tuite_fisman"),
             #            width = 7
             #          )
             # ),
             
             tabPanel("Documentation",
                      fluidPage(
                        mainPanel(
                          h4(a(href='https://docs.google.com/spreadsheets/d/1pIGNv4EiXOjLXNvIoJUEGy6681Pf3LHbRQzzuFjAtSs/', "Click here for the metholodogy.",
                               target = '_blank')),
                          br(),
                          h4(a(href='https://docs.google.com/spreadsheets/d/1uut3Sw-DYleGneeZylRfr1qEQYqJYfRszJAp5eF8_Ho/', "Click here for the county-specific severity rates.",
                               target = '_blank')),
                          br(),
                          h4(a(href='https://docs.google.com/spreadsheets/d/1x9IjGEjLRO_8Tz7Y6Nf6rOeUsItZfMjoj83Qf5D9jo0/', "Click here for the age-specific severity rates.",
                               target = '_blank')),
                          br(),
                          h4("Definitions"),
                          p("Doubling time is defined by the amount of time it takes a population to double in size. In this case, assuming exponential 
                            growth in the number of COVID-19 cases, we are defining the doubling time as the number of days it takes for cases to double. "),
                          uiOutput("formula"),
                          HTML('For more details, see this <a href="https://www.nejm.org/doi/full/10.1056/NEJMoa2001316">analysis</a> of COVID-19 doubling time.'),
                          br(),
                          br(),
                          h4("References"),
                          a(href="https://data.census.gov/", "[1] 2014-2018 ACS 5-Year Estimates from US Census", target = '_blank'),
                          br(),
                          a(href="https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals", "[2] Data on Hospital beds from HIFLD", target = '_blank'),
                          br(),
                          a(href="https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf", target = '_blank', "[3] Estimates of the severity of COVID-19 disease by age group"),
                          br(),
                          a(href="https://www.nejm.org/doi/full/10.1056/NEJMoa2002032", target = '_blank', "[4] 11 day LOS for severe patients and 13 day LOS for critical patients (Table 1)"),
                          br(),
                          a(href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30566-3/fulltext", target = '_blank', "[5] 11 day LOS for all patients (Table 2)"),
                          br(),
                          a(href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", target = '_blank', "[6] Default values of cases by county"),
                          br(),
                          br(),
                          br(),
                          strong("Contact:"),
                          img(src = "email2.png", height = 17.5, width = 'auto'),
                          hr(),
                          strong("Created by:"), 
                          p("Johannes Opsahl Ferstad, Angela Gu, Raymond Ye Lee, Isha Thapa, Alejandro Martinez, Andy Shin, Joshua Salomon, Peter Glynn, Kevin Schulman, David Scheinker"),
                          strong("For their help, we thank:"),
                          p("Nigam Shah, Amber Levine, Grace Lee"),
                          br(),
                          img(src = "SURF.png", height = 60, width = 'auto'),
                          img(src = "CERC.png", height = 60, width = 'auto'),
                          img(src = "matrixds_logo.png", height = 60, width = 'auto')
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  output$state_selector_1 <- renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state1", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(sort(unique(df$State))),
                # calls unique values from the State column in the previously created table
                selected = "California") #default choice (not required)
  })
  
  output$county_selector_1 <- renderUI({#creates County select box object called in ui
    
    req(input$state1)
    data_available = df[df$State == input$state1, "County"]
    
    selectInput(inputId = "county1", #name of input
                label = "County:", #label displayed in ui
                choices = c('All', sort(unique(data_available))), #calls list of available counties
                selected = if (input$state1 == 'California') 'Santa Clara County' else sort(unique(data_available)[2]),
                multiple = TRUE)
  })

  
  get_county_df <- reactive({
    state <- input$state1
    state_df <- df %>% filter(State == state)
    counties <- input$county1
    if (!('All' %in% counties)) {
      return(state_df %>% filter(County %in% counties))
    } else {
      return (state_df)
    }
  })

  output$num_cases <- renderUI({
    req(input$state1)
    if (is.null(input$county1)) {
      numericInput("num_cases", "Total Confirmed Cases (as of today)", 1, min = 1)
    } else {
      num_cases <- sum((get_county_df() %>% group_by(County) %>% summarize(num_cases = max(Cases)) %>% filter(!is.na(num_cases)))$num_cases)
      if (is.na(num_cases)) {num_cases <- 1}
      num_cases <- max(num_cases, 1)
      numericInput("num_cases", "Total Confirmed Cases (as of today)", num_cases, min = 1)
    }
  })
    
  observeEvent(input$reset, {
    num_cases <- sum((get_county_df() %>% group_by(County) %>% summarize(num_cases = max(Cases)) %>% filter(!is.na(num_cases)))$num_cases)
    if (is.na(num_cases)) {num_cases <- 1}
    num_cases <- max(num_cases, 1)
    updateNumericInput(session, "num_cases", value = num_cases)
    updateSliderInput(session, "num_days", value = 30)
    updateNumericInput(session, "doubling_time", value = 6)
    updateNumericInput(session, "los_severe", value = 11)
    updateNumericInput(session, "los_critical", value = 13)
    updateSliderInput(session, "prop_bed_for_covid", value = 50)
  })
  
  
  observeEvent(input$clear, {
    updateNumericInput(session, "day_change_1", value = NA)
    #updateNumericInput(session, "day_change_2", value = NA)
    #updateNumericInput(session, "day_change_3", value = NA)
    updateNumericInput(session, "double_change_1", value = NA)
    #updateNumericInput(session, "double_change_2", value = NA)
    #updateNumericInput(session, "double_change_3", value = NA)
  })
  
  output$formula <- renderUI({
    withMathJax(sprintf('We define \\(N_{t+1} = N_{t} \\times 2^{\\frac{1}{DT}} \\), 
                        where \\(N_t \\) is the number of cases at time \\(t\\)
                        and \\(DT\\) is the doubling time.'))
  })

  observeEvent(input$clear, {
    updateNumericInput(session, "day_change_1", value = NA)
    #updateNumericInput(session, "day_change_2", value = NA)
    #updateNumericInput(session, "day_change_3", value = NA)
    updateNumericInput(session, "double_change_1", value = NA)
    #updateNumericInput(session, "double_change_2", value = NA)
    #updateNumericInput(session, "double_change_3", value = NA)
  })

  get_naive_estimations <- reactive({
    
    county_df <- get_county_df()
    num_cases <- input$num_cases
    doubling_time <- input$doubling_time
    
    combined_counties_severity_rates <- county_df %>% group_by(age_decade) %>% 
      summarise(
        combined_population_in_age_group = sum(population_in_age_group),
        wtd_case_fatality_rate = weighted.mean(case_fatality_rate, population_in_age_group),
        wtd_critical_case_rate = weighted.mean(critical_case_rate, population_in_age_group),
        wtd_severe_cases_rate = weighted.mean(severe_cases_rate, population_in_age_group)
      )
    
    naive_estimations <- combined_counties_severity_rates %>%
      mutate(relative_pop = combined_population_in_age_group/sum(combined_population_in_age_group)) %>%
      mutate(estimated_cases = num_cases*relative_pop) %>% 
      mutate(estimated_cases = num_cases*relative_pop) %>% 
      mutate(estimated_fatal_cases = estimated_cases*wtd_case_fatality_rate) %>% 
      mutate(estimated_critical_cases = estimated_cases*wtd_critical_case_rate) %>% 
      mutate(estimated_severe_cases = estimated_cases*wtd_severe_cases_rate)
    
    naive_estimations
  })
  
  output$text1 <- renderText({
    req(input$county1)
    
    if(is.na(input$doubling_time)) {
      return("<span style='color: red;'>Enter a Doubling Time on the left to generate a forecast</span>")
    }
    
    req(input$doubling_time)
    county_df <- get_county_df()
    
    text = "<ul>"
    
    # Population bullet
    if (!('All' %in% input$county1)) {
      for (county in input$county1) {
        under_60 = sum((county_df %>% filter(age_decade %in% c('0-9','10-19','20-29','30-39','40-49','50-59') & County == county))$population_in_age_group)
        over_60 = sum((county_df %>% filter(age_decade %in% c('60-69', '70-79', '80+') & County == county))$population_in_age_group)
        county_text = paste(c(county, "has", format(under_60, big.mark=","), "people aged 0-59 and",
                              format(over_60, big.mark=","), "people aged 60+. "), collapse = " ")
        text = paste(text, "<li>", county_text, "</li>", sep = '')
      }
      
      
    } else {
      under_60 = sum((county_df %>% filter(age_decade %in% c('0-9','10-19','20-29','30-39','40-49','50-59')))$population_in_age_group)
      over_60 = sum((county_df %>% filter(age_decade %in% c('60-69', '70-79', '80+')))$population_in_age_group)
      county_text = paste(c(input$state1, "has", format(under_60, big.mark=","), "people aged 0-59 and",
                            format(over_60, big.mark=","), "people aged 60+. "), collapse = " ")
      text = paste(text, "<li>", county_text, "</li>", sep = '')
    }
    
    # Hospital Beds bullet
    if (!('All' %in% input$county1)) {
      bed_total <- 0
      county_no_info <- c()
      county_with_info <- c()
      for (county in input$county1) {
        num_beds = (county_df %>% filter(County == county) %>% summarize(num_beds = max(num_beds)))[[1]]*input$prop_bed_for_covid/100
        if (is.na(num_beds)) {
          county_no_info <- c(county_no_info, county)
        } else {
          county_with_info <- c(county_with_info, county)
          bed_total <- bed_total + num_beds
        }
      }
      
      if (length(county_with_info) > 0) {
        text = paste(text, "<li>Assuming", paste0(toString(input$prop_bed_for_covid), "%"), "of beds available, ", collapse = " ")
      }
      
      for (county in county_with_info) {
        num_beds = (county_df %>% filter(County == county) %>% summarize(num_beds = max(num_beds)))[[1]]*input$prop_bed_for_covid/100
        bed_text = paste(county, 'has', round(num_beds), 'hospital beds for COVID-19 cases. ', collapse = " ")
        text = paste(text, bed_text, sep = '')
      }
      
      if (length(county_with_info) > 0) {
        text = paste(text, 'These include general (non-pediatric), government, and specialty hospitals (see documentation). You can modify the % of beds available to COVID patients in the inputs.')
      }
      
      if (length(county_no_info) > 0) {
        text = paste0(text, "\nWe did not find information on the number of beds in")
      }
      temp <- ""
      for (county in county_no_info) {
        temp = paste(temp, paste0(county, '/'), sep = '')
      }
      temp <- substr(temp, 1, nchar(temp)-1)

      if (length(county_no_info) > 0) {
        text = paste(text, temp, sep = ' ')
        text = paste(text, '. Add surrounding counties to see the combined results.', sep = '')
      }
      
      text = paste0(text, "</li>")
      
    } else {
      
      bed_total <- 0
      
      county_no_info <- c()
      county_with_info <- c()
      counties <- 
      for (county in unique(county_df$County)) {
        num_beds <- (county_df %>% filter(County == county) %>% summarize(num_beds = max(num_beds)))[[1]]*input$prop_bed_for_covid/100
        if (is.na(num_beds)) {
          county_no_info <- c(county_no_info, county)
        } else {
          county_with_info <- c(county_with_info, county)
          bed_total <- bed_total + num_beds
        }
      }
      
      if (length(county_with_info) > 0) {
        text = paste("<li>", "Assuming", paste0(toString(input$prop_bed_for_covid), "%"), "of beds available,", input$state1, "has", format(round(bed_total), big.mark=","),
                     "hospital beds for COVID-19 cases.", collapse = " ")
      }

      if (length(county_with_info) > 0) {
        text = paste(text, 'Includes general (non-pediatric), government, and specialty hospitals - see documentation. Modify COVID occupancy % below. ')
      }
      
      if (length(county_no_info) > 0) {
        text = paste0(text, "We did not find information on the number of beds in")
      }
      temp <- ""
      for (county in county_no_info) {
        temp = paste(temp, paste0(county, '/'), sep = '')
      }
      temp <- substr(temp, 1, nchar(temp)-1)
      if (length(county_no_info) > 0) {
        text = paste(text, paste0(temp, '.'), sep = ' ')
      }
      text = paste0(text, "</li>")
    }
    
    # No intervention bullet
    
    naive_estimations <- get_naive_estimations()
    sum_cases <- sum(naive_estimations['estimated_critical_cases']) + sum(naive_estimations['estimated_severe_cases']) 
    
    # calculate days to fill beds given no interventions
    hospitalizations_without_intervention = get_case_numbers()[['hospitalizations_without_intervention']]
    beds_remaining = bed_total - hospitalizations_without_intervention
    days_to_fill = min(which(beds_remaining<=0)) - 1
    
    if (bed_total > 0) {
      if(days_to_fill < Inf) {
        text <- paste(c(text, '<li>Assuming no changes to the doubling time, the number of people requiring hospitalization will exceed the number of available hospital beds in ', days_to_fill, " days. </li>"), collapse = "")
      } else {
        text <- paste(text, '<li>Assuming no changes to the doubling time, the number of people requiring hospitalization will not exceed the number of available hospital beds in the next ', input$num_days, " days. </li>", sep = "")
      }
    }
    
    # Intervention bullet
    
    dt_changes = get_dt_changes()
    if(length(dt_changes) > 0 & bed_total > 0) {
      cases_w_interventions <- get_case_numbers()
      intervention_hospitalizations = cases_w_interventions$critical + cases_w_interventions$severe
      beds_remaining = bed_total - intervention_hospitalizations
      first_day_without_beds = min(which(beds_remaining<=0)) - 1
      
      if(length(first_day_without_beds > 0) & first_day_without_beds < Inf) {
        text <- paste(text, '<li>If interventions lead to the input change in doubling time, then the number of people requiring hospitalization will exceed the number of available beds in ', first_day_without_beds, " days. </li>", sep = "")
      } else {
        text <- paste(text, '<li>If interventions lead to the input change in doubling time, then the number of people requiring hospitalization will not exceed the number of available beds in the next ', input$num_days, " days. </li>", sep = "")
      }
    }
    
    return(paste0(text, "</ul>"))
    
  })
  
  output$table1 <- renderTable({
    
    req(input$county1)
    
    county_df <- get_county_df()
    
    total_population = sum(county_df['population_in_age_group'])
    case_mortality_rate <- sum(county_df['case_fatality_rate']*county_df['population_in_age_group'])/total_population
    case_critical_rate <- sum(county_df['critical_case_rate']*county_df['population_in_age_group'])/total_population
    case_severe_rate <- sum(county_df['severe_cases_rate']*county_df['population_in_age_group'])/total_population
    critical_plus_severe <- case_critical_rate + case_severe_rate
    
    table <- data.table(" " = c('Population-specific case severity rate<br>(per 100 cases)'),
                        "Fatality<br>(Subset of Critical)" = c(round(case_mortality_rate*100, digits = 2)),
                        "Critical" = c(round(case_critical_rate*100, digits = 2)),
                        "Severe"= c(round(case_severe_rate*100, digits = 2)),
                        "Hospitalization<br>(Critical + Severe)"= c(round(critical_plus_severe*100, digits = 2)))
    
    table
    
  }, sanitize.text.function=identity, width = "100%")
  
  get_dt_changes <- reactive({
    n_days = input$num_days
    valid_pair <- function(dt, day) {
      if (is.na(dt) | is.na(day)) {return(FALSE)}
      if (day != as.integer(day)) {return(FALSE)}
      if (day >= n_days | day < 1) {return(FALSE)}
      return(TRUE)
    }
    
    dt_changes <- c() 
    if (valid_pair(input$double_change_1, input$day_change_1)) {dt_changes = c(dt_changes, c(input$double_change_1, input$day_change_1))}
    #if (valid_pair(input$double_change_2, input$day_change_2)) {dt_changes = c(dt_changes, c(input$double_change_2, input$day_change_2))}
    #if (valid_pair(input$double_change_3, input$day_change_3)) {dt_changes = c(dt_changes, c(input$double_change_3, input$day_change_3))}
    return(dt_changes)
  })
  
  # Function to get hospitalizations from cumulative cases, with projection backwards from current cases to prevent jump at day LOS
  get_hospitalizations = function(cumulative_cases, los, doubling_time) {
    
    # Project cases backwards LOS days
    cum_cases_w_backwards_projection = c(rep(cumulative_cases[1], los), cumulative_cases)
    for (i in los:1) {
      cum_cases_w_backwards_projection[i] = cum_cases_w_backwards_projection[i+1]/2^(1/doubling_time)
    }
    
    # Calculate hospitalizations from day 1 through projection period
    return(cum_cases_w_backwards_projection[(los+1):length(cum_cases_w_backwards_projection)] - cum_cases_w_backwards_projection[1:length(cumulative_cases)])
  }
  
  get_case_numbers <- reactive({
    req(input$county1)
    
    naive_estimations <- get_naive_estimations()
    
    n_days <- input$num_days
    cases <- rep(input$num_cases, n_days+1)
    fatal_cases <- rep(sum(naive_estimations['estimated_fatal_cases']), n_days+1)
    critical_cases <- rep(sum(naive_estimations['estimated_critical_cases']), n_days+1)
    severe_cases <- rep(sum(naive_estimations['estimated_severe_cases']), n_days+1)
    
    doubling_time <- input$doubling_time
    
    
    # cases without intervention
    critical_without_intervention = critical_cases
    severe_without_intervention = severe_cases
    for (i in 1:n_days) {
      critical_without_intervention[i+1] = critical_without_intervention[i]*2^(1/doubling_time)
      severe_without_intervention[i+1] = severe_without_intervention[i]*2^(1/doubling_time)
    }
    
    # number hospitalized at any one time (without intervention)
    #critical_without_intervention = critical_without_intervention - c(rep(0, input$los_critical), critical_without_intervention)[1:length(critical_without_intervention)]
    critical_without_intervention = get_hospitalizations(critical_without_intervention, input$los_critical, doubling_time)
    #severe_without_intervention = severe_without_intervention - c(rep(0, input$los_severe), severe_without_intervention)[1:length(severe_without_intervention)]
    severe_without_intervention = get_hospitalizations(severe_without_intervention, input$los_severe, doubling_time)
    
    # cases with intervention
    dt_changes = c()
    if (input$submit != 0) {
      dt_changes = get_dt_changes()
    }
    
    for (i in 1:n_days) {
      if (length(dt_changes) > 0) {
        days <- dt_changes[c(FALSE, TRUE)]
        if ((i-1) %in% days) {
          doubling_time <- dt_changes[2*min(which(days == i-1))-1]
        }
      }
      cases[i+1] = cases[i]*2^(1/doubling_time)
      fatal_cases[i+1] = fatal_cases[i]*2^(1/doubling_time)
      critical_cases[i+1] = critical_cases[i]*2^(1/doubling_time)
      severe_cases[i+1] = severe_cases[i]*2^(1/doubling_time)
    }
    
    # number hospitalized at any one time (with intervention)
    #critical_cases = critical_cases - c(rep(0, input$los_critical), critical_cases)[1:length(critical_cases)]
    critical_cases = get_hospitalizations(critical_cases, input$los_critical, doubling_time)
    #severe_cases = severe_cases - c(rep(0, input$los_severe), severe_cases)[1:length(severe_cases)]
    severe_cases = get_hospitalizations(severe_cases, input$los_severe, doubling_time)
    
    total_population <- sum(naive_estimations$combined_population_in_age_group)
    if ((severe_cases[n_days+1] + critical_cases[n_days + 1]) < 0.25*total_population) {
      return_cases <- list(
        "fatal" = fatal_cases, 
        "critical" = critical_cases, 
        "severe" = severe_cases,
        "hospitalizations_without_intervention" = critical_without_intervention + severe_without_intervention)
      return(return_cases)
    } else {
      stop(safeError(
        "Model is not valid for the input values relative to the population size. 
      Please reduce the initial number; reduce the days to model; or increase the doubling time."
      ))
    }
  })
  
  output$plot1 <- renderPlotly({
    req(input$county1)
    req(input$doubling_time)
    
    case_numbers <- get_case_numbers()
    fatal_cases <- case_numbers$fatal
    critical_cases <- case_numbers$critical
    severe_cases <- case_numbers$severe

    num_beds <- round(sum((get_county_df() %>% group_by(County) %>% summarize(num_beds = max(num_beds)) %>% filter(!is.na(num_beds)))$num_beds)*input$prop_bed_for_covid/100)
    n_days <- input$num_days
    day_list <- c(0:n_days)
    
    y_axis <- 'Cases'
    
    chart_data = melt(data.table(
      date = Sys.Date() + day_list,
      estimated_hospitalizations = round(critical_cases) + round(severe_cases),
      severe_cases = round(severe_cases),
      critical_cases = round(critical_cases)#,
      #fatal_cases = fatal_cases
    ), id.vars = c('date'))
    
    chart_data[chart_data$variable == 'estimated_hospitalizations', 'variable'] <-  'Hospitalizations (Severe + Critical Cases)'
    chart_data[chart_data$variable == 'severe_cases', 'variable'] <-  'Severe Cases'
    chart_data[chart_data$variable == 'critical_cases', 'variable'] <-  'Critical Cases'
    #chart_data[chart_data$variable == 'fatal_cases', 'variable'] <-  'Cumulative Fatal Cases'
    
    gp = ggplot(chart_data,
                aes(x=date, y=value, group=variable, text = sprintf("date:  %s \n cases: %i", date, value))) +
      geom_line(aes(linetype = variable, size = variable, color = variable)) +  guides(linetype=FALSE) + guides(size=FALSE) +
      scale_color_manual(values=c("black", "dodgerblue", "red")) +
      scale_linetype_manual(values=c("solid", "solid", "solid")) +
      scale_size_manual(values=c(0.75, 0.5, 0.5, 0.25)) +
      theme_minimal() +
      ylab("Number of cases") + xlab('Date')  +
      coord_cartesian(ylim=c(0, max(critical_cases + severe_cases))) +
      ggtitle("COVID-19 cases requiring hospitalization")
    
    if (input$submit != 0) {
      dt_changes = get_dt_changes()
      days <- dt_changes[c(FALSE, TRUE)]
      for (i in days) {
        gp = gp +
          geom_vline(xintercept = as.numeric(Sys.Date() + i), color = 'grey', linetype = 'dashed') +
          annotate("text", x = Sys.Date() + i, y = max(critical_cases + severe_cases), color = 'grey', 
                   label = "Intervention")
      }
    }
    
    if(!is.na(num_beds)) {
      gp = gp +
        geom_hline(yintercept = num_beds, linetype = "dashed", color = 'grey') + 
        annotate("text", x = Sys.Date() + 0.5*n_days, y = num_beds*1.05, label = "Number of Hospital Beds for COVID Patients", vjust=1, hjust=0, color = 'grey')
    }
    
    
    ggplotly(gp, tooltip = 'text') %>% 
      layout(
        legend = list(x = 0.02, y = 0.1, bgcolor = 'rgba(0,0,0,0)'),
        xaxis=list(fixedrange=TRUE),
        yaxis=list(fixedrange=TRUE)) %>% 
      config(displayModeBar = F)
    
  })
  
  output$plot2 <- renderPlotly({
    
    req(input$state1)
    selected_states <- input$state1
    state_df <- df %>% filter(State %in% selected_states)
    
    state_df <- state_df %>% group_by(State, FIPS) %>%
      summarize(
        hospitalization_rate = 
          100*sum(population_in_age_group*hospitalizations_per_case)/sum(population_in_age_group)) %>% 
      rename(state = State, fips = FIPS)
    
    geom_args <- list()
    geom_args[["color"]] <- "black"
    geom_args[["size"]] <- 0.2
    map_df <- map_with_data(state_df, values = "hospitalization_rate", include = selected_states) %>% mutate(group = county)
    geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, y = map_df$y,
                                           group = map_df$group, fill = map_df[, "hospitalization_rate"])
    polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)
    
    p <- ggplot(data = map_df, aes(text = paste(county, round(hospitalization_rate, 2), sep = ": "))) + polygon_layer + ggplot2::coord_equal() +
      scale_fill_viridis(option='inferno', direction = -1) +
      theme(legend.position = "right") + labs(title = "Expected Hospitalization Rate by County", fill = "Hospitalizations\nper 100\ncases") + theme_void() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "white"))
    
    ggplotly(p, tooltip = 'text')
  })
  
  output$plot_tuite_fisman <- renderPlot({
    end_date_sim = ymd('2020-03-31')
    
    # how many full generations?
    num_gens = floor(as.numeric(end_date_sim - input$start_date_outbreak)/input$serial_interval)
    
    # vectors, no control
    Nt_no_control = input$it0 * cumsum(input$r0^(0:num_gens))
    incidence_no_control = Nt_no_control - c(0, Nt_no_control[-length(Nt_no_control)])
    
    #how many generations before control? (including the current generation)
    num_gens_pre_control = ceiling(as.numeric(input$start_date_control - input$start_date_outbreak)/input$serial_interval)
    
    #how many full generations after control
    num_gens_post_control = num_gens - num_gens_pre_control # ceiling(as.numeric(end_date_sim - input$start_date_control)/input$serial_interval)
    
    
    # get Nt vector
    Nt_pre_control = input$it0 * cumsum(input$r0^(0:num_gens_pre_control))
    
    itc = Nt_pre_control[length(Nt_pre_control)] - Nt_pre_control[length(Nt_pre_control) - 1]
    Nt_post_control = Nt_pre_control[length(Nt_pre_control)] + itc * cumsum(input$re^(1:num_gens_post_control))
    
    # vectors, with controls
    Nt_with_control = c(Nt_pre_control, Nt_post_control)
    incidence_with_control = Nt_with_control - c(0, Nt_with_control[-length(Nt_with_control)])
    
    
    # plot
    plot(x = input$start_date_outbreak + 0:(length(Nt_no_control) - 1) * days(input$serial_interval),
         y = Nt_no_control,
         ylim = c(0, 3.2e5),
         xlim = c(ymd('2019-11-01'), ymd('2020-03-31')),
         xlab = 'Date',
         ylab = 'Cumulative cases',
         type = 'l',
         lwd = 2,
         xaxt = 'n',
         yaxt = 'n',
         col = 'red')
    
    lines(x = input$start_date_outbreak + 0:(length(Nt_with_control) - 1) * days(input$serial_interval),
          y = Nt_with_control,
          lwd = 2,
          col = 'orange')
    
    lines(x = input$start_date_outbreak + 0:(length(Nt_with_control) - 1) * days(input$serial_interval),
          y = incidence_no_control,
          lwd = 1,
          col = 'red',
          lty = 'dashed')
    
    lines(x = input$start_date_outbreak + 0:(length(Nt_with_control) - 1) * days(input$serial_interval),
          y = incidence_with_control,
          lwd = 1,
          col = 'orange',
          lty = 'dashed')
    
    options(scipen = 999)
    axis(2, at = seq(0, 300000, 100000))
    
    axis.Date(1, at = seq(ymd('2019-11-01'), ymd('2020-03-31'), by = '2 week'),
              format = '%m-%d-%Y')
    
    abline(v = ymd(input$start_date_control), col = 'black', lty = 'dashed')
    text(x = ymd(input$start_date_control), y = 2.75e5, 'Control start', pos = 2)
    
    # abline(h = input$num_beds, col = 'black', lty = 'dashed')
    # text(x = ymd(input$start_date_outbreak) + days(7), y = input$num_beds, 'Number of hospital beds', pos = 3)
    
    grid(nx = NA, ny = NULL)
    legend('topleft', c('No control (cumulative)', 'No control (new cases)', 'With control (cumulative)', 'With control (new cases)'), 
           col = c('red', 'red', 'orange', 'orange'),
           lty = c('solid', 'dashed', 'solid', 'dashed'),
           lwd = c(2, 1, 2, 1))
  })
}



shinyApp(ui, server)
