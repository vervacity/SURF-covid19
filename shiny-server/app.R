# shiny_project <- ""
# prod <- paste0("/srv/shiny-server/", shiny_project)
# dev <- paste0("~/shiny-server/", shiny_project)
# if (!dir.exists(prod) & !dir.exists(dev)) {
#   #message(" using getwd() for shiny_path")
#   shiny_path <- getwd()
# } else {
#   .libPaths(c(.libPaths(), "/srv/.R/library"))
#   options(java.parameters = "-Xmx8048m")
#   #if (dir.exists(prod)) message("prod"); shiny_path <- prod
#   #if (dir.exists(dev)) message("dev"); shiny_path <- dev
# }

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

df <- read.csv('county_age_severity_rates_v6.csv', stringsAsFactors = FALSE)
df$County <- gsub('city', 'City', df$County)
acute_beds_dt = fread('acute_byFIPS.csv')
icu_beds_dt = fread('icu_byFIPS.csv')
bed_dt = merge(acute_beds_dt, icu_beds_dt, by = "FIPS")
county_cases <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
county_cases <- county_cases[, c(1,2, ncol(county_cases))]
county_cases <- county_cases %>% rename_at(vars(colnames(county_cases)), ~ c("FIPS", 'County', 'Cases')) %>% 
  filter(FIPS != 0) %>% mutate(FIPS = as.numeric(FIPS)) %>% select(FIPS, Cases)
df <- left_join(df, bed_dt, by = 'FIPS')
df <- left_join(df, county_cases, by = 'FIPS')

ui <- shinyUI(
  list(
  HTML('<div style = "display: block; width: 100%; height: 60px; background: url(banner.png); background-repeat: no-repeat; background-size: auto; background-position: left center; background-size: contain; margin-left: 15px;"></div>'),

  navbarPage("Projecting Severe Cases of COVID-19",
             
             tabPanel("Calculator",
                      sidebarPanel(
                        
                        fluidRow(
                          column(12, 
                                 p("Select a state and county or group of counties."),
                                 uiOutput("state_selector_1"),
                                 checkboxInput("state_all_selector", "All Counties", value = FALSE),
                                 uiOutput("county_selector_1"),
                                 hr(),
                                 p('If available, input the number of cumulative COVID-19 hospitalizations.'),
                                 radioButtons("input_radio", inline=TRUE, label = "Input:", choices = list("Confirmed Cases" = 1, "Hospitalizations" = 2), selected = 1),
                                 uiOutput("num_cases"),
                                 hr(),
                                 HTML('Enter the <b>Doubling Time</b>, the number of days until the cumulative number of hospitalization/cases doubles.<br/><a href="https://www.nytimes.com/interactive/2020/03/21/upshot/coronavirus-deaths-by-country.html?action=click&module=Top%20Stories&pgtype=Homepage" target="_blank">(General range: 2-7 in the US)</a>'),
                                 #strong("Estimated Doubling Time for Cases Requiring Hospitalization (Days)"),
                                 # br(),
                                 # br(),
                                 numericInput("doubling_time", NULL, value = NA, min = 1, max = 20),
                                 uiOutput("case_scaler"),
                                 hr(),
                                 sliderInput("num_days", "Number of Days to Model Ahead", 20, min = 1, max = 60),
                                 hr(),
                                 h4("Simulation of Intervention"),
                                 p("To simulate the effects of interventions (e.g. social distancing), select up to three new doubling times and start times (days from today):"),
                                 # HTML('<div style = "display: block; width: 100%; height: 0.25em;"></div>'),
                                 # radioButtons("social_distancing_effect", label = NULL, 
                                 #              choices = list(
                                 #                "No reduction" = 0,
                                 #                "25%" = 25, 
                                 #                "33%" = 33,
                                 #                "50%" = 50)
                                 #              , selected = 0),
                                 fluidRow(
                                   column(6, tags$b("On Day")),
                                   column(6, tags$b("New DT"))
                                 ),
                                 fluidRow(
                                   column(6,
                                          numericInput("day_change_1", label=NULL, NA, min = 1),
                                          numericInput("day_change_2", label=NULL, NA, min = 1),
                                          numericInput("day_change_3", label=NULL, NA, min = 1)
                                   ),
                                   column(6,
                                          numericInput("double_change_1", label=NULL, NA, min = 1),
                                          numericInput("double_change_2", label=NULL, NA, min = 1),
                                          numericInput("double_change_3", label=NULL, NA, min = 1)
                                   ),
                                   column(12, style="display:center-align", 
                                          actionButton("load_dt_change_examples", "Fill Example Values"),
                                          actionButton("clear", "Clear")
                                   )
                                 ),
                                 hr(),
                                 p("If local data are available, modify length of stay and beds availability below."),
                                 #h4("User Inputs"),
                                 sliderInput("los_severe", "Length of Stay (Days) for Acute", 12, min = 1, max = 90),
                                 sliderInput("los_critical", "Length of Stay (Days) for ICU", 7, min = 1, max = 90),
                                 # sliderInput("days_to_hospitalization", "Days to Hospitalization", 9, min = 0, max = 30),
                                 sliderInput("prop_acute_beds_for_covid", "% of Acute Beds for COVID-19 Cases", 50, min = 0, max = 100),
                                 sliderInput("prop_icu_beds_for_covid", "% of ICU Beds for COVID-19 Cases", 50, min = 0, max = 100),
                                 hr(),
                                 actionButton("reset", "Reset all to default user inputs")
                          )
                        ),
                        width = 3
                      ),
                      mainPanel(
                        
                        tags$head(tags$style(".shiny-output-error{color: green;}")),
                        h4("This tool allows healthcare providers and policy makers to estimate ICU and Acute Care bed demand for COVID-19 patients. See the Documentation tab for methodology."),
                        hr(),
                        htmlOutput("text1"),
                        tags$head(tags$style("ul, li {margin-left: 0.5em; padding-left: 0;}")),
                        tags$head(tags$style("li {margin-top: 0.5em;}")),
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
                          
                          HTML('
                            <blockquote style="font-size: 1em;">
                            For each US county, the model accepts as an input the number of COVID-19 hospitalizations and the associated doubling time, if these are available. If these are not available, the model imports the latest number of confirmed cases from the USA facts online repository and accepts user-entered parameters of the ratio of total cases to confirmed cases (e.g. 51) and the COVID-19 population-level doubling time (e.g. 6 days). <br /><br />
                            The effects of interventions that mitigate the spread of infection (such as social distancing) are simulated with user-entered parameters of the changes in doubling time and the days of those changes. The model estimates county-specific hospitalization rates by combining age-distributions derived from the US census and age-group specific estimates of the case rates of severe symptoms, critical symptoms, and mortality (together morbidity) derived from the Imperial College COVID-19 Response Team.<br /><br />
                            The model estimates the number of people requiring hospitalization using the initial numbers, the doubling time, and the population-specific rates and then compares these to the numbers of relevant beds derived from data from the American Hospital Association. The default assumptions are that: people requiring hospitalization are hospitalized on the day they test positive (the assumptions will change when non-symptomatic people start being tested); those with severe and critical symptoms spend, respectively, 12 days in acute care and 7 days in intensive care; and 50% of each type of bed is available for COVID-19+ patients. 
                            </blockquote>'),
                          
                          h4(a(href='https://medrxiv.org', "Click here for the full metholodogy.",
                               target = '_blank')),
                          br(),
                          h4(a(href='https://docs.google.com/spreadsheets/d/1x9IjGEjLRO_8Tz7Y6Nf6rOeUsItZfMjoj83Qf5D9jo0/', "Click here for the county-age population numbers and severity rates we use as input.",
                               target = '_blank')),
                          br(),
                          
                          h4("Definitions"),
                          HTML("<b>Doubling time</b> is defined by the amount of time it takes a population to double in size. In this case, assuming exponential 
                            growth in the number of COVID-19 cases, we are defining the doubling time as the number of days it takes for cases to double."),
                          uiOutput("formula"),
                          HTML('For more details, see this <a href="https://www.nejm.org/doi/full/10.1056/NEJMoa2001316">analysis</a> of COVID-19 doubling time.'),
                          br(),br(),
                          HTML('<b>Acute/Severe Cases</b> are adult cases meeting any of the following criteria: <ol>
                               <li>Respiratory distress (≧30 breaths/ min);</li>
                               <li>Oxygen saturation≤93% at rest;</li>
                               <li>Arterial partial pressure of oxygen (PaO2)/fraction of inspired oxygen (FiO2)≦300mmHg (l mmHg=0.133kPa).</li></ol>
                               >50% of cases with chest imaging that showed obvious lesion progression within 24-48 hours are managed as severe cases.'),
                          br(),br(),
                          HTML('<b>ICU/Critical Cases</b> are cases meeting any of the following criteria: <ol>
                               <li>Respiratory failure and requiring mechanical ventilation;</li>
                               <li>Shock;</li>
                               <li>Other organ failure that requires ICU care.</li></ol>'),
                          br(),
                          br(),
                          h4("References"),
                          a(href="https://data.census.gov/", "[1] 2014-2018 ACS 5-Year Estimates from US Census", target = '_blank'),
                          br(),
                          a(href="https://www.ahadata.com/aha-annual-survey-database-asdb/", "[2] Data on Hospital beds from AHA Annual Survey Database", target = '_blank'),
                          br(),
                          a(href="https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf", target = '_blank', "[3] Estimates of the severity of COVID-19 disease by age group"),
                          br(),
                          a(href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30566-3/fulltext", target = '_blank', "[4] 7-day LOS for ICU cases and 12-day LOS for acute cases; Days from symptom onset to hospital admission; Definitions of Severe and Critical cases"),
                          br(),
                          # a(href="https://www.thelancet.com/journals/lanres/article/PIIS2213-2600(20)30079-5/fulltext", target = '_blank', "[5] 9.5 days from symptom onset to ICU admission"),
                          # br(),
                          a(href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", target = '_blank', "[5] Default values of cases by county"),
                          br(),
                          a(href="https://science.sciencemag.org/content/early/2020/03/13/science.abb3221.long", target = '_blank', "[6] Estimated 86% (95% CI: [82% - 90%]) of infections went undocumented within China prior to travel restrictions"),
                          br(),
                          br(),
                          br(),
                          strong("Contact:"),
                          img(src = "email2.png", height = 17.5, width = 'auto'),
                          br(),
                          a(href="https://forms.gle/oQ5uQAEpm13iCmQo8", target = '_blank', "Comments and questions are welcomed!"),
                          hr(),
                          strong("Created by:"), 
                          p("Johannes Opsahl Ferstad, Angela Gu, Raymond Ye Lee, Isha Thapa, Alejandro Martinez, Andy Shin, Joshua Salomon, Peter Glynn, Nigam Shah, Arnold Milstein, Kevin Schulman, David Scheinker"),
                          strong("For their help, we thank:"),
                          p("Amber Levine, Grace Lee, Teng Zhang, Jacqueline Jil Vallon, Francesca Briganti"),
                          p("We thank Amber Levine for her help testing and improving the model."),
                          br(),
                          img(src = "SURF.png", height = 60, width = 'auto'),
                          img(src = "CERC.png", height = 60, width = 'auto'),
                          img(src = "matrixds_logo.png", height = 60, width = 'auto'),
                        width = 10)
                      )
             ),
             
             tabPanel("About",

                fluidPage(
                  mainPanel(
                    h2("About"),
                    fluidRow(
                      column(width = 2, 
                             h4(a(href = "https://www.linkedin.com/in/johannes-ferstad-0a495833", "Johannes Ferstad", target = "_blank"), align = 'center'),
                             HTML('<center><img src="johannes.png" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/angela-gu", "Angela Gu", target = "_blank"), align = 'center'),
                             HTML('<center><img src="angela.png" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://www.linkedin.com/in/raymond-ye-lee-97882224", "Raymond Lee", target = "_blank"), align = 'center'),
                             HTML('<center><img src="raymond.png"style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/isha-thapa", "Isha Thapa", target = "_blank"), align = 'center'),
                             HTML('<center><img src="isha.png" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://www.linkedin.com/in/alejandromartinezm", "Alejandro Martinez", target = "_blank"), align = 'center'),
                             HTML('<center><img src="alejandro.jpg" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/andrew-shin", "Andy Shin", target = "_blank"), align = 'center'),
                             HTML('<center><img src="andy.jpg" style="width:150px;height:150px;"></center>'))
                    ),
                    br(),
                    fluidRow(
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/joshua-salomon", "Joshua Salomon", target = "_blank"), align = 'center'),
                             HTML('<center><img src="joshua.jpg" style="width:150px;height:150px;"></center>')),
                      column(width = 2,
                             h4(a(href = "https://web.stanford.edu/~glynn/", "Peter Glynn", target = "_blank"), align = 'center'),
                             HTML('<center><img src="peter.jpeg" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://shahlab.stanford.edu/", "Nigam Shah", target = "_blank"), align = 'center'),
                             HTML('<center><img src="nigam.jpeg" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/arnold-milstein", "Arnold Milstein", target = "_blank"), align = 'center'),
                             HTML('<center><img src="arnold.jpg" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://profiles.stanford.edu/kevin-schulman", "Kevin Schulman", target = "_blank"), align = 'center'),
                             HTML('<center><img src="kevin.png" style="width:150px;height:150px;"></center>')),
                      column(width = 2, 
                             h4(a(href = "https://surf.stanford.edu/people/", "David Scheinker", target = "_blank"), align = 'center'),
                             HTML('<center><img src="david.jpg" style="width:150px;height:150px;"></center>'))
                    ),
                  width = 11
                  )
                )
             )
  )
))

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
    if (!input$state_all_selector) {
      data_available = df[df$State == input$state1, "County"]
      
      selectInput(inputId = "county1", #name of input
                  label = "County:", #label displayed in ui
                  choices = sort(unique(data_available)), #calls list of available counties
                  selected = if (input$state1 == 'California') 'Santa Clara County' else sort(unique(data_available)[2]),
                  multiple = TRUE)
    }
  })

  
  get_county_df <- reactive({
    state <- input$state1
    state_df <- df %>% filter(State == state)
    counties <- input$county1
    if (!input$state_all_selector) {
      return(state_df %>% filter(County %in% counties))
    } else {
      return (state_df)
    }
  })

  output$num_cases <- renderUI({
    req(input$state1)
    
    if (is.null(input$county1) & input$input_radio == 1) {
      list(
        HTML('<b>Cumulative Confirmed Cases</b> (as of <a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/" target="_blank">today</a>)'),
        numericInput("num_cases", label=NULL, 1, min = 1))
      
    } else if (input$input_radio == 1) {
      num_cases <- sum((get_county_df() %>% group_by(County) %>% summarize(num_cases = max(Cases)) %>% filter(is.finite(num_cases)))$num_cases)
      if (!is.finite(num_cases)) {num_cases <- 0}
      num_cases <- max(num_cases, 0)
      list(
        HTML('<b>Cumulative Confirmed Cases</b> (as of <a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/" target="_blank">today</a>)'),
        numericInput("num_cases",  label=NULL, num_cases, min = 1))
      
    } else if (is.null(input$county1) & input$input_radio == 2) {
      numericInput("num_cases", "Cumulative Hospitalizations (as of today)", 1, min = 1)
      
    } else {
      numericInput("num_cases", "Cumulative Hospitalizations (as of today)", 0, min = 1)
    }
    
  })
  
  output$case_scaler <- renderUI({
    req(input$state1)
    if (input$input_radio == 1) {
      sliderInput("case_scaler", "Number of True Cases per Confirmed Case", 5, min = 1, max = 20)
    } 
  })
    
  observeEvent(input$reset, {
    num_cases <- sum((get_county_df() %>% group_by(County) %>% summarize(num_cases = max(Cases)) %>% filter(is.finite(num_cases)))$num_cases)
    if (!is.finite(num_cases)) {num_cases <- 0}
    num_cases <- max(num_cases, 0)

    if (input$input_radio == 1) {
      updateNumericInput(session, "doubling_time", value = 6) 
      updateNumericInput(session, "num_cases", value = num_cases) 
      updateSliderInput(session, "case_scaler", value = 5)
    } else {
      updateNumericInput(session, "num_cases", value = 0)
    }
    updateRadioButtons(session, "input_radio", selected = 1)
    updateSliderInput(session, "num_days", value = 20)
    updateNumericInput(session, "los_severe", value = 12)
    updateNumericInput(session, "los_critical", value = 7)
    # updateNumericInput(session, "days_to_hospitalization", value = 9)
    updateSliderInput(session, "prop_acute_beds_for_covid", value = 50)
    updateSliderInput(session, "prop_icu_beds_for_covid", value = 50)
  })
  
  
  observeEvent(input$clear, {
    updateNumericInput(session, "day_change_1", value = NA)
    updateNumericInput(session, "day_change_2", value = NA)
    updateNumericInput(session, "day_change_3", value = NA)
    updateNumericInput(session, "double_change_1", value = NA)
    updateNumericInput(session, "double_change_2", value = NA)
    updateNumericInput(session, "double_change_3", value = NA)
  })
  
  observeEvent(input$load_dt_change_examples, {
    updateNumericInput(session, "day_change_1", value = 1)
    updateNumericInput(session, "day_change_2", value = 10)
    updateNumericInput(session, "day_change_3", value = 23)
    updateNumericInput(session, "double_change_1", value = 9)
    updateNumericInput(session, "double_change_2", value = 12)
    updateNumericInput(session, "double_change_3", value = 14)
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
    
    hospitalizations_input_instead_of_cases <- (input$input_radio == 2)
    doubling_time <- input$doubling_time
    
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
      num_cases <- input$num_cases * input$case_scaler
    } else {
      validate(
        need(input$num_cases > 0, "To run the model enter a non-zero number of hospitalizations.")
      )
      # Infer total cases from demographics if hospitalizations are given
      num_cases <- input$num_cases / combined_counties_severity_rates$wtd_hosp_rate[1]
    }
    
    return(list(
      total_population = combined_counties_severity_rates$total_population[1],
      estimated_fatal_cases = num_cases * combined_counties_severity_rates$wtd_case_fatality_rate[1],
      estimated_critical_cases = num_cases * combined_counties_severity_rates$wtd_critical_case_rate[1],
      estimated_severe_cases = num_cases * combined_counties_severity_rates$wtd_severe_cases_rate[1]
    ))
  })
  
  output$text1 <- renderText({
    req(input$county1)
    
    if(!is.finite(input$doubling_time)) {
      return("<span style='color: red;'>Enter a Doubling Time on the left to generate a forecast</span>")
    }
    
    req(input$doubling_time)
    county_df <- get_county_df()
    
    text = "<ul>"
    
    # Population bullet
    if (!input$state_all_selector) {
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
    if (!input$state_all_selector) {
      acute_bed_total <- 0
      icu_bed_total <- 0
      county_no_info <- c()
      county_with_info <- c()
      
      # List counties with and without bed data, and add text about assumption if any county has bed data
      add_assumption = TRUE # Set to FALSE once added
      for (county in input$county1) {
        num_beds_df = (county_df %>% 
          filter(County == county) %>% 
          summarize(
            num_acute_beds = max(num_acute_beds, na.rm = T),
            num_icu_beds = max(num_icu_beds, na.rm = T))
          )
        num_acute_beds_available = num_beds_df$num_acute_beds[1]*input$prop_acute_beds_for_covid/100
        num_icu_beds_available = num_beds_df$num_icu_beds[1]*input$prop_icu_beds_for_covid/100
        num_total_beds_available = num_acute_beds_available + num_icu_beds_available
        
        if (!is.finite(num_total_beds_available)) {
          county_no_info <- c(county_no_info, county)
        } else {
          if(add_assumption) {
            text = paste(text, "<li>", "Assuming", paste0(toString(input$prop_acute_beds_for_covid), "%"), "of acute beds and", paste0(toString(input$prop_icu_beds_for_covid), "%"), "of ICU beds available to COVID-19 cases,", collapse = " ")
            add_assumption = FALSE
          }
          county_with_info <- c(county_with_info, county)
          acute_bed_total <- acute_bed_total + num_acute_beds_available
          icu_bed_total <- icu_bed_total + num_icu_beds_available
          bed_text = paste(county, 'has', round(num_acute_beds_available), 'acute beds and', round(num_icu_beds_available), 'ICU beds for COVID-19 cases. ', collapse = " ")
          text = paste(text, bed_text, sep = ' ')
        }
      }
      
      if (length(county_with_info) > 0) {
        text = paste(text, 'You can modify the % of beds available to COVID-19 patients in the inputs. See Documentation tab for data source.')
      }
      
      if (length(county_no_info) > 0) {
        text = paste(text, "We did not find information on the number of beds in")
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
      
      acute_bed_total <- 0
      icu_bed_total <- 0
      
      county_no_info <- c()
      county_with_info <- c()
      
      for (county in unique(county_df$County)) {
        num_beds_df = (county_df %>% 
                         filter(County == county) %>% 
                         summarize(
                           num_total_beds = max(num_acute_beds+num_icu_beds),
                           num_acute_beds = max(num_acute_beds),
                           num_icu_beds = max(num_icu_beds))
        )
        num_acute_beds_available = num_beds_df$num_acute_beds[1]*input$prop_acute_beds_for_covid/100
        num_icu_beds_available = num_beds_df$num_icu_beds[1]*input$prop_icu_beds_for_covid/100
        num_total_beds_available = num_acute_beds_available + num_icu_beds_available
        
        if (!is.finite(num_total_beds_available)) {
          county_no_info <- c(county_no_info, county)
        } else {
          county_with_info <- c(county_with_info, county)
          acute_bed_total <- acute_bed_total + num_acute_beds_available
          icu_bed_total <- icu_bed_total + num_icu_beds_available
        }
      }
      
      if (length(county_with_info) > 0) {
        text = paste(text, "<li>", "Assuming", paste0(toString(input$prop_acute_beds_for_covid), "%"), "of acute beds and", paste0(toString(input$prop_icu_beds_for_covid), "%"), "of ICU beds available to COVID-19 cases,", input$state1, "has", format(round(acute_bed_total), big.mark=","),
                     "acute beds and", format(round(icu_bed_total), big.mark=","), "ICU beds for COVID-19 cases.", collapse = " ")
      }

      if (length(county_with_info) > 0) {
        text = paste(text, 'You can modify the % of beds available to COVID-19 patients in the inputs. See Documentation tab for data source.')
      }
      
      if (length(county_no_info) > 0) {
        text = paste(text, "We did not find information on the number of beds in")
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
  
    critical_without_intervention = get_case_numbers()[['critical_without_intervention']]
    severe_without_intervention = get_case_numbers()[['severe_without_intervention']]
      
    icu_beds_remaining = icu_bed_total - critical_without_intervention
    acute_beds_remaining = acute_bed_total - severe_without_intervention
    icu_days_to_fill = min(which(icu_beds_remaining<=0)) - 1
    acute_days_to_fill = min(which(acute_beds_remaining<=0)) - 1

    if (icu_bed_total > 0) {
      if(icu_days_to_fill > 0 & icu_days_to_fill < Inf) {
        text <- paste(c(text, '<li>Assuming no changes to the doubling time, the number of people requiring ICU beds will exceed the number of available ICU beds in ', icu_days_to_fill, " days. </li>"), collapse = "")
      } else {
        text <- paste(text, '<li>Assuming no changes to the doubling time, the number of people requiring ICU beds will not exceed the number of available ICU beds in the next ', input$num_days, " days. </li>", sep = "")
      }
    }
    
    if (acute_bed_total > 0) {
      if(acute_days_to_fill > 0 & acute_days_to_fill < Inf) {
        text <- paste(c(text, '<li>Assuming no changes to the doubling time, the number of people requiring acute beds will exceed the number of available acute beds in ', acute_days_to_fill, " days. </li>"), collapse = "")
      } else {
        text <- paste(text, '<li>Assuming no changes to the doubling time, the number of people requiring acute beds will not exceed the number of available acute beds in the next ', input$num_days, " days. </li>", sep = "")
      }
    }
    
    # Intervention bullet
    
    dt_changes = get_dt_changes()
    if(length(dt_changes) > 0 & (acute_bed_total > 0 | icu_bed_total > 0)) {
      cases_w_interventions <- get_case_numbers()
      intervention_icu = cases_w_interventions$critical
      intervention_acute = cases_w_interventions$severe
      icu_beds_remaining = icu_bed_total - intervention_icu
      acute_beds_remaining = acute_bed_total - intervention_acute
      icu_days_to_fill = min(which(icu_beds_remaining<=0)) - 1
      acute_days_to_fill = min(which(acute_beds_remaining<=0)) - 1
      
      if (icu_bed_total > 0) {
        if(icu_days_to_fill > 0 & icu_days_to_fill < Inf) {
          text <- paste(c(text, '<li>If interventions lead to the input change in doubling time, then the number of people requiring ICU beds will exceed the number of available ICU beds in ', icu_days_to_fill, " days. </li>"), collapse = "")
        } else {
          text <- paste(text, '<li>If interventions lead to the input change in doubling time, then the number of people requiring ICU beds will not exceed the number of available ICU beds in the next ', input$num_days, " days. </li>", sep = "")
        }
      }
      
      if (acute_bed_total > 0) {
        if(acute_days_to_fill > 0 & acute_days_to_fill < Inf) {
          text <- paste(c(text, '<li>If interventions lead to the input change in doubling time, the number of people requiring acute beds will exceed the number of available acute beds in ', acute_days_to_fill, " days. </li>"), collapse = "")
        } else {
          text <- paste(text, '<li>If interventions lead to the input change in doubling time, the number of people requiring acute beds will not exceed the number of available acute beds in the next ', input$num_days, " days. </li>", sep = "")
        }
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
      if (!is.finite(dt) | !is.finite(day)) {return(FALSE)}
      if (day != as.integer(day)) {return(FALSE)}
      if (day >= n_days | day < 1) {return(FALSE)}
      return(TRUE)
    }
    
    day_change_1 = input$day_change_1
    double_change_1 = input$double_change_1
    day_change_2 = input$day_change_2
    double_change_2 = input$double_change_2
    day_change_3 = input$day_change_3
    double_change_3 = input$double_change_3
    
    
    # if(input$social_distancing_effect == 0) {
    #   double_change_1 = NA
    #   day_change_1 = NA
    #   double_change_2 = NA
    #   day_change_2 = NA
    #   double_change_3 = NA
    #   day_change_3 = NA
    #   
    # } else if (input$social_distancing_effect == 25) {
    #   
    #   # On Day 1, doubling time is 6
    #   day_change_1 = 1
    #   double_change_1 = 6
    #   
    #   # On Day 7, doubling time increases to 7
    #   day_change_2 = 7
    #   double_change_2 = 7
    # 
    #   # On Day 21, doubling time increases to 8
    #   day_change_3 = 21
    #   double_change_3 = 8
    #   
    # } else if (input$social_distancing_effect == 33) {
    #   
    #   # On Day 1, doubling time is 7
    #   day_change_1 = 1
    #   double_change_1 = 7
    #   
    #   # On Day 8, doubling time increases to 8
    #   day_change_2 = 8
    #   double_change_2 = 8
    #   
    #   # On Day 16, doubling time increases to 9
    #   day_change_3 = 16
    #   double_change_3 = 9
    #   
    # } else if (input$social_distancing_effect == 50) {
    #   
    #   # On Day 1, doubling time is 9
    #   day_change_1 = 1
    #   double_change_1 = 9
    #   
    #   # On Day 10, doubling time increases to 12
    #   day_change_2 = 10
    #   double_change_2 = 12
    #   
    #   # On Day 23, doubling time increases to 14
    #   day_change_3 = 23
    #   double_change_3 = 14
    # }
    
    dt_changes <- c() 
    if (valid_pair(double_change_1, day_change_1)) {dt_changes = c(dt_changes, c(double_change_1, day_change_1))}
    if (valid_pair(double_change_2, day_change_2)) {dt_changes = c(dt_changes, c(double_change_2, day_change_2))}
    if (valid_pair(double_change_3, day_change_3)) {dt_changes = c(dt_changes, c(double_change_3, day_change_3))}
    
    return(dt_changes)
  })
  
  # Function to get hospitalizations from cumulative cases, with projection backwards from cuendat cases to prevent jump at day LOS
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
      
      return(return_vec)
  }
  
  get_case_numbers <- reactive({
    req(input$county1)
    
    naive_estimations <- get_naive_estimations()
    
    n_days <- input$num_days
    # cases <- rep(input$num_cases * input$case_scaler, n_days+1)
    fatal_cases <- rep(naive_estimations$estimated_fatal_cases, n_days+1)
    critical_cases <- rep(naive_estimations$estimated_critical_cases, n_days+1)
    severe_cases <- rep(naive_estimations$estimated_severe_cases, n_days+1)
    
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
    
    # number hospitalized at any one time (with intervention)
    #critical_cases = critical_cases - c(rep(0, input$los_critical), critical_cases)[1:length(critical_cases)]
    critical_cases = get_hospitalizations(critical_cases, input$los_critical, doubling_time)
    #severe_cases = severe_cases - c(rep(0, input$los_severe), severe_cases)[1:length(severe_cases)]
    severe_cases = get_hospitalizations(severe_cases, input$los_severe, doubling_time)
    
    total_population <- naive_estimations$total_population
    validate(
      need(input$num_cases != 0, "To run the model enter a non-zero number of cases."),
      need((severe_cases[n_days+1] + critical_cases[n_days + 1]) < 0.25*total_population, 
           "Current data are insufficient to reliably model infection rates this high. The model will be updated as more data become available. To proceed, reduce the initial number; or reduce the days to model; or increase the doubling time.")
    )
  
    return_cases <- list(
      "fatal" = fatal_cases, 
      "critical" = critical_cases, 
      "severe" = severe_cases,
      "critical_without_intervention" = critical_without_intervention,
      "severe_without_intervention" = severe_without_intervention)
    return(return_cases)
  })
  
  output$plot1 <- renderPlotly({
    req(input$county1)
    req(input$doubling_time)
    
    case_numbers <- get_case_numbers()
    fatal_cases <- case_numbers$fatal
    critical_cases <- case_numbers$critical
    severe_cases <- case_numbers$severe

    num_beds_df = get_county_df() %>% 
      group_by(County) %>% 
      summarize(
       num_total_beds = max(num_acute_beds+num_icu_beds, na.rm = TRUE),
       num_acute_beds = max(num_acute_beds, na.rm = TRUE),
       num_icu_beds = max(num_icu_beds, na.rm = TRUE)) %>%
      filter(is.finite(num_total_beds)) %>% filter(is.finite(num_acute_beds)) %>% filter(is.finite(num_icu_beds)) %>% 
      ungroup() %>%
      summarize(
        num_acute_beds = sum(num_acute_beds, na.rm = TRUE),
        num_icu_beds = sum(num_icu_beds, na.rm = TRUE))

    num_acute_beds_available = num_beds_df$num_acute_beds[1]*input$prop_acute_beds_for_covid/100
    num_icu_beds_available = num_beds_df$num_icu_beds[1]*input$prop_icu_beds_for_covid/100
    num_total_beds_available = num_acute_beds_available + num_icu_beds_available
    
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
    
    chart_data[chart_data$variable == 'estimated_hospitalizations', 'variable'] <-  'Hospitalizations (Acute + ICU Cases)'
    chart_data[chart_data$variable == 'severe_cases', 'variable'] <-  'Acute Cases'
    chart_data[chart_data$variable == 'critical_cases', 'variable'] <-  'ICU Cases'
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
      scale_x_date(name="Date", labels = date_format("%b %d",tz = "EST")) +
      ggtitle("Daily number of people hospitalized for COVID-19 (not cumulative)")

    if (is.finite(input$day_change_1) & input$day_change_1 > 0 & is.finite(input$double_change_1) & input$double_change_1 > 0) {
      dt_changes = get_dt_changes()
      days <- dt_changes[c(FALSE, TRUE)]
      for (i in days) {
        gp = gp +
          geom_vline(xintercept = as.numeric(Sys.Date() + i), color = 'grey', linetype = 'dashed') +
          annotate("text", x = Sys.Date() + i, y = max(critical_cases + severe_cases), color = 'grey',
                   label = "Intervention")
      }
    }
    
    if(is.finite(num_total_beds_available)) {
      gp = gp +
        geom_hline(yintercept = num_total_beds_available, linetype = "dashed", color = 'grey') + 
        annotate("text", x = Sys.Date() + 0.5*n_days, y = num_total_beds_available*1.05, label = "Total Beds for COVID Patients", vjust=1, hjust=0, color = 'grey')
    }
    
    if(is.finite(num_acute_beds_available)) {
      gp = gp +
        geom_hline(yintercept = num_acute_beds_available, linetype = "dashed", color = 'grey') + 
        annotate("text", x = Sys.Date() + 0.5*n_days, y = num_acute_beds_available*1.05, label = "Acute Beds for COVID Patients", vjust=1, hjust=0, color = 'grey')
    }
    
    if(is.finite(num_icu_beds_available)) {
      gp = gp +
        geom_hline(yintercept = num_icu_beds_available, linetype = "dashed", color = 'grey') + 
        annotate("text", x = Sys.Date() + 0.5*n_days, y = num_icu_beds_available*1.05, label = "ICU Beds for COVID Patients", vjust=1, hjust=0, color = 'grey')
    }
    
    ggplotly(gp, tooltip = 'text') %>% 
      layout(
        legend = list(x = 0.02, y = 0.9, bgcolor = 'rgba(0,0,0,0)'),
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
