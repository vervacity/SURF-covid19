#
# This is a Shiny web application on MatrixDS. 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##########################################################################################
# This points the Shiny server tool to any libraries installed with RStudio 
# that means that any library you install on your RStudio instance in this project,
# will be available to the shiny server
##########################################################################################

#.libPaths( c( .libPaths(), "/srv/.R/library") )

##########################################################################################
# Here you can call all the required libraries for your code to run
##########################################################################################

library(shiny)

##########################################################################################
# For deploying tools on MatrixDS, we created this production variable
# when set to true, your shiny app will run on the shiny server tool upon clicking open
# when set to false, your shiny app will run when you hit the "Run App" button on RStudio
##########################################################################################

production <- TRUE

##########################################################################################
# The shiny server tool uses a different absolute path than RStudio.
# this if statement denotes the correct path for the 2 values of the production variable
##########################################################################################

#if(production == FALSE) {
  #if you using the RStudio tool
#  shiny_path <- "/Users/dscheinker/Documents/COVID19/Population models/shiny-server/"
#  home_path <- "/Users/dscheinker/Documents/COVID19/Population models"
#} else {
  #if you are using the shiny tool
#  shiny_path <- "/srv/shiny-server/"
#  home_path <- "/srv/"
#}

shiny_path <- "~/Dropbox/covid_pop_model/Population models/shiny-server/"
home_path <- "~/Dropbox/covid_pop_model/Population models/"

##########################################################################################
# To call a file/artifact in your MatrixDS project use the following line of code
# this example uses the function read.csv
#  my_csv <- read.csv(paste0(home_path,"file_name.csv"))
##########################################################################################
##########################################################################################
# To call a file/artifact in your MatrixDS project use the following line of code
# this example uses the function read.csv
#  my_csv <- read.csv(paste0(home_path,"file_name.csv"))
##########################################################################################
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)
library(usmap)

df <- read.csv('merged_pop_ccs_fatality_rate_v4.csv', stringsAsFactors = FALSE)
df$County <- gsub('city', 'City', df$County)
bed_df <- read.csv('Hospitals.csv', stringsAsFactors = FALSE) %>%
  filter(BEDS != -999, STATE != 'PR') %>%
  select(COUNTYFIPS, COUNTY, BEDS) %>% rename(FIPS = COUNTYFIPS, beds = BEDS)
bed_df <- bed_df %>% group_by(FIPS) %>%
  summarize(num_beds = sum(beds)) %>% mutate(FIPS = as.numeric(FIPS))
df <- left_join(df, bed_df, by = 'FIPS')

ui <- shinyUI(
  
  navbarPage("30-Day Predictions for Severe Cases of COVID",
             
             tabPanel("County-Level (from today)",
                      sidebarPanel(
                        fluidRow(
                          column(6, 
                                 uiOutput("state_selector_1"),
                                 uiOutput("county_selector_1"),
                                 numericInput("num_cases", "Current Cases (Today)", 114, min = 1),
                                 checkboxInput("only_serious", "Number of hospitalizations", FALSE),
                                 numericInput("num_days", "Number of Days", 30, min = 1)
                          ),
                          column(6,
                                 numericInput("doubling_time", "Doubling Time (Days)", 6, min = 1, max = 20),
                                 hr(),
                                 p("Doubling times (DT) can be changed at up to three time points to reflect interventions."),
                                 fluidRow(
                                   column(6, tags$b("On Day")),
                                   column(6, tags$b("New DT"))
                                 ),
                                 fluidRow(
                                   column(6,
                                          numericInput("day_change_1", "", NA, min = 1),
                                          numericInput("day_change_2", "", NA, min = 1),
                                          numericInput("day_change_3", "", NA, min = 1),
                                   ),
                                   column(6,
                                          numericInput("double_change_1", "", NA, min = 1),
                                          numericInput("double_change_2", "", NA, min = 1),
                                          numericInput("double_change_3", "", NA, min = 1),
                                   ),
                                   column(12, style="display:center-align",
                                          actionButton("submit", "Submit DT changes"),
                                          actionButton("clear", "Clear DT changes")
                                   )
                                 )
                          )
                        ),
                        width = 5
                      ),
                      mainPanel(
                        textOutput("text1"),
                        plotOutput("plot1",
                                   hover = hoverOpts(id = "plot_hover")),
                        width = 7
                      )
             ),
             
             tabPanel("Tuite and Fisman (2020)",
                      sidebarPanel(
                        sliderInput("serial_interval", "Serial interval (days)",
                                    min = 5, max = 10,
                                    value = 7),
                        
                        sliderInput("start_date_outbreak", "Outbreak start date",
                                    min = ymd('2019-11-01'), max = ymd('2019-12-31'),
                                    value = ymd('2019-12-01')),
                        
                        sliderInput("start_date_control", "Control start date",
                                    min = ymd('2020-01-01'), max = ymd('2020-01-31'),
                                    value = ymd('2020-01-15')),
                        
                        sliderInput("it0", "Initial number of cases",
                                    min = 1, max = 40,
                                    value = 1),
                        
                        sliderInput("r0", "Basic reproductive number",
                                    min = 1.5, max = 4,
                                    value = 2.3),
                        
                        sliderInput("re", "Effective reproductive number with control",
                                    min = 0.5, max = 3,
                                    value = 1.5),
                        
                        sliderInput("num_beds", "Number of hospital beds",
                                    min = 0, max = 300000,
                                    value = 1000),
                        
                        actionButton("reset", "Reset to default values")
                      ),
                      mainPanel(
                        plotOutput("plot_tuite_fisman"),
                        width = 7
                      )
             ),
             
             tabPanel("County-Level (Population)",
                      sidebarPanel(
                        uiOutput("state_selector_2"),
                        uiOutput("county_selector_2"),
                        numericInput("pct_pop", "% of Population Infected", NA, min = 1, max = 100)
                      ),
                      mainPanel(
                        textOutput("text2"),
                        tableOutput("table1")
                      )
             ),
             
             tabPanel("National",
                      sidebarPanel(
                        uiOutput("state_selector_3"),
                      ),
                      mainPanel(
                        plotlyOutput("plot2")
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
    
    data_available = df[df$State == input$state1, "County"]
    
    selectInput(inputId = "county1", #name of input
                label = "County:", #label displayed in ui
                choices = sort(unique(data_available)), #calls list of available counties
                selected = sort(unique(data_available)[1]),
                multiple = TRUE)
  })
  
  output$state_selector_2 <- renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state1", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(sort(unique(df$State))),
                # calls unique values from the State column in the previously created table
                selected = input$state1)
  })
  
  output$county_selector_2 <- renderUI({#creates County select box object called in ui
    
    data_available = df[df$State == input$state1, "County"]
    
    selectInput(inputId = "county1", #name of input
                label = "County:", #label displayed in ui
                choices = sort(unique(data_available)), #calls list of available counties
                selected = input$county1,
                multiple = TRUE)
  })
  
  output$state_selector_3 <- renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state2", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(sort(unique(df$State))),
                # calls unique values from the State column in the previously created table
                selected = "California",
                multiple = TRUE) #default choice (not required)
  })
  
  critical_to_fatal_proportion <- 2087/1023
  severe_to_fatal_proportion <- 6168/1023
  
  get_county_df <- reactive({
    state <- input$state1
    state_df <- df %>% filter(State == state)
    counties <- input$county1
    county_df <- state_df %>%
      filter(County %in% counties) %>% 
      select(County, age_decade, comorbidity, case_fatality_rate, pop_within_comorbidity, num_beds)
    county_df
  })
  
  get_naive_estimations <- reactive({
    
    county_df <- get_county_df()
    num_cases <- input$num_cases
    doubling_time <- input$doubling_time
    
    naive_estimations <- county_df %>% group_by(County) %>% 
      mutate(relative_pop = pop_within_comorbidity/sum(pop_within_comorbidity)) %>% 
      mutate(estimated_cases = num_cases*relative_pop) %>% 
      mutate(estimated_fatal_cases = estimated_cases*case_fatality_rate) %>% 
      mutate(estimated_critical_cases = critical_to_fatal_proportion*estimated_fatal_cases) %>% 
      mutate(estimated_severe_cases = severe_to_fatal_proportion*estimated_fatal_cases)
    
    if (input$only_serious) {
      multiply_constant = num_cases/sum(naive_estimations$estimated_severe_cases + naive_estimations$estimated_critical_cases)
      naive_estimations <- naive_estimations %>%
        mutate_at(vars(estimated_cases, estimated_fatal_cases,
                       estimated_critical_cases, estimated_severe_cases),.funs = funs(. * multiply_constant))
    }
    naive_estimations
  })
  
  output$text1 <- renderText({
    req(input$county1)
    county_df <- get_county_df()
    text <- ""
    bed_total <- 0
    has_nan <- FALSE
    for (county in input$county1) {
      num_beds = (df %>% filter(County == county) %>% summarize(num_beds = max(num_beds)))[[1]]
      if (is.na(num_beds)) {
        bed_text = paste("We did not find information on the number of beds in", county, sep = " ")
        text = paste(text, bed_text, sep = '')
        text = paste(text, '. \n', sep = '')
        has_nan <- TRUE
      } else {
        bed_text = paste(c(county, 'has', num_beds, 'hospital beds. \n'), collapse = " ")
        text = paste(text, bed_text, sep = '')
        bed_total <- bed_total + num_beds
      }
    }
    
    naive_estimations <- get_naive_estimations()
    sum_cases <- sum(naive_estimations['estimated_critical_cases']) + sum(naive_estimations['estimated_severe_cases']) 
    days_to_fill <- ceiling(input$doubling_time*log(bed_total/sum_cases, base = 2))
    
    if (has_nan) {
      if (length(input$county1) > 1) {
        text <- paste(c(text, "Assuming a total of", bed_total, "hospital beds with no interventions, these will be filled within",
                        days_to_fill, "days. \n"), collapse = " ")
      }
    } else {
      text <- paste(c(text, 'Assuming no interventions, these will be filled within', days_to_fill, "days. \n"), collapse = " ")
    }
    text
    
  })
  
  observeEvent(input$clear, {
    updateNumericInput(session, "day_change_1", value = NA)
    updateNumericInput(session, "day_change_2", value = NA)
    updateNumericInput(session, "day_change_3", value = NA)
    updateNumericInput(session, "double_change_1", value = NA)
    updateNumericInput(session, "double_change_2", value = NA)
    updateNumericInput(session, "double_change_3", value = NA)
  })
  
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
    if (valid_pair(input$double_change_2, input$day_change_2)) {dt_changes = c(dt_changes, c(input$double_change_2, input$day_change_2))}
    if (valid_pair(input$double_change_3, input$day_change_3)) {dt_changes = c(dt_changes, c(input$double_change_3, input$day_change_3))}
    return(dt_changes)
  })
  
  get_case_numbers <- reactive({
    req(input$county1)
    
    naive_estimations <- get_naive_estimations()
    n_days <- input$num_days
    cases <- rep(input$num_cases, n_days+1)
    fatal_cases <- rep(sum(naive_estimations['estimated_fatal_cases']), n_days+1)
    critical_cases <- rep(sum(naive_estimations['estimated_critical_cases']), n_days+1)
    severe_cases <- rep(sum(naive_estimations['estimated_severe_cases']), n_days+1)
    
    doubling_time <- input$doubling_time
    
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
    
    return_cases <- list("fatal" = fatal_cases, "critical" = critical_cases, "severe" = severe_cases)
    return(return_cases)
    
  })
  
  output$plot1 <- renderPlot({
    req(input$county1)
    
    case_numbers <- get_case_numbers()
    fatal_cases <- case_numbers$fatal
    critical_cases <- case_numbers$critical
    severe_cases <- case_numbers$severe
    
    num_beds <- sum((get_naive_estimations() %>% group_by(County) %>% summarize(num_beds = max(num_beds)))$num_beds)
    n_days <- input$num_days
    day_list <- c(0:n_days)
    
    y_axis <- 'Cases'
    if (input$only_serious) {
      y_axis <- 'Hospital Patients'
    }
    plot(Sys.Date() + day_list, critical_cases + severe_cases, type = 'l', col = 'black', lwd = 2,
         main = paste(paste(input$county1, collapse = ' & \n'), '(Projected)'),
         xlab = 'Date', ylab = y_axis, ylim = c(0, max(critical_cases + severe_cases)))
    lines(Sys.Date() + day_list, severe_cases, type = 'l', col = 'blue', lwd = 1.5)
    lines(Sys.Date() + day_list, critical_cases, type = 'l', col = 'red', lwd=1.5)
    lines(Sys.Date() + day_list, fatal_cases, type = 'l', col = 'purple', lty = 'dashed', lwd=1)
    abline(h = num_beds, col = "grey", lty="dashed")
    text(Sys.Date() + 1/2*n_days, num_beds, "number of \n hospital beds", pos = 3)
    points(Sys.Date(), input$num_cases, pch = 4)
    text(Sys.Date(), input$num_cases, labels = input$num_cases, pos = 4)
    
    if (input$submit != 0) {
      dt_changes = get_dt_changes()
      days <- dt_changes[c(FALSE, TRUE)]
      for (i in days) {
        abline(v = Sys.Date() + i, col = "grey", lty = "dashed")
      }
    }
    
    legend("topleft", c("estimated need \n (critical + severe)", "severe", 'critical', 'fatal (subset of \n critical)'), fill=c('black', 'blue', 'red', 'purple'))
    
    
  })
  
  output$text2 <- renderText({
    req(input$county1)
    county_df <- get_county_df()
    
    text = ""
    for (county in input$county1) {
      under_65 = sum((county_df %>% filter(comorbidity == 'Any', County == county))$pop_within_comorbidity)
      over_65 = sum((county_df %>% filter(comorbidity != 'Any', County == county))$pop_within_comorbidity)
      county_text = paste(c(county, "has", format(under_65, big.mark=","), "people aged 0-64 and",
                            format(over_65, big.mark=","), "people aged 65+. \n"), collapse = " ")
      text = paste(text, county_text, sep = '')
    }
    
    text = paste(text, paste(c("This is a total of ",
                               format(sum(county_df$pop_within_comorbidity), big.mark=","),
                               "people."), collapse = " "), sep = '')
    text
  })
  
  output$table1 <- renderTable({
    
    req(input$county1)
    
    county_df <- get_county_df()
    
    total_population = sum(county_df['pop_within_comorbidity'])
    case_mortality_rate <- sum(county_df['case_fatality_rate']*county_df['pop_within_comorbidity'])/total_population
    case_critical_rate <- case_mortality_rate*critical_to_fatal_proportion
    case_severe_rate <- case_mortality_rate*severe_to_fatal_proportion
    critical_plus_severe <- case_critical_rate + case_severe_rate
    
    table <- data.table(" " = c('Population-specific symptom severity rate', 'Percent of population with COVID-19',
                                '10%', '25%', '50%'),
                        "Case mortality (subset of critical)" = c(round(case_mortality_rate*100, digits = 2), "",
                                                                  toString(round(case_mortality_rate*total_population*0.1)),
                                                                  round(case_mortality_rate*total_population*0.25),
                                                                  round(case_mortality_rate*total_population*0.5)),
                        "Critical symptoms" = c(round(case_critical_rate*100, digits = 2), "",
                                                toString(round(case_critical_rate*total_population*0.1, digits = 0)),
                                                round(case_critical_rate*total_population*0.25),
                                                round(case_critical_rate*total_population*0.5)),
                        "Severe symptoms"= c(round(case_severe_rate*100, digits = 2), "",
                                             toString(round(case_severe_rate*total_population*0.1)),
                                             round(case_severe_rate*total_population*0.25),
                                             round(case_severe_rate*total_population*0.5)),
                        "Estimated need"= c(round(critical_plus_severe*100, digits = 2), "",
                                            toString(round(critical_plus_severe*total_population*0.1)),
                                            round(critical_plus_severe*total_population*0.25),
                                            round(critical_plus_severe*total_population*0.5)))
    
    if (!is.na(input$pct_pop)) {
      pct <- min(input$pct_pop, 100)
      table <- rbind(table, list(paste0(toString(pct), '%'), round(case_mortality_rate*total_population*pct/100),
                                 round(case_critical_rate*total_population*pct/100),
                                 round(case_severe_rate*total_population*pct/100),
                                 round(critical_plus_severe*total_population*pct/100)))
    }
    
    table
    
  })
  
  output$plot2 <- renderPlotly({
    
    req(input$state2)
    selected_states <- input$state2
    state_df <- df %>% filter(State %in% selected_states)
    
    state_df <- state_df %>% group_by(State, FIPS) %>%
      summarize(hospitalization_rate = 100*sum(case_fatality_rate*pop_within_comorbidity*
                                                 (critical_to_fatal_proportion + severe_to_fatal_proportion))/sum(pop_within_comorbidity)) %>% 
      rename(state = State, fips = FIPS)
    
    geom_args <- list()
    geom_args[["color"]] <- "black"
    geom_args[["size"]] <- 0.4
    map_df <- map_with_data(state_df, values = "hospitalization_rate", include = selected_states) %>% mutate(group = county)
    geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, y = map_df$y,
                                           group = map_df$group, fill = map_df[, "hospitalization_rate"])
    polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)
    
    p <- ggplot(data = map_df, aes(text = paste(county, round(hospitalization_rate, 2), sep = ": "))) + polygon_layer + ggplot2::coord_equal() +
      scale_fill_viridis(option='inferno', direction = -1) +
      theme(legend.position = "right") + labs(title = "Expected Hospitalization Rate by County", fill = "Expected \n Hospitalization \n Rate") + theme_void()
    
    ggplotly(p, tooltip = 'text')
  })
  
  
  output$plot_tuite_fisman <- renderPlot({
    end_date_sim = ymd('2020-03-31')
    
    # how many full generations?
    num_gens = floor(as.numeric(end_date_sim - input$start_date_outbreak)/input$serial_interval)
    
    # vector, no control
    Nt_no_control = input$it0 * cumsum(input$r0^(1:num_gens))

    
    #how many generations before control? (including the current generation)
    num_gens_pre_control = ceiling(as.numeric(input$start_date_control - input$start_date_outbreak)/input$serial_interval)
    
    #how many full generations after control
    num_gens_post_control = floor(as.numeric(end_date_sim - input$start_date_control)/input$serial_interval)
    
    # get Nt vector
    Nt_pre_control = input$it0 * cumsum(input$r0^(1:num_gens_pre_control))
    
    itc = Nt_pre_control[length(Nt_pre_control)]
    Nt_post_control = itc * cumsum(input$re^(1:num_gens_post_control))
    
    Nt_with_control = c(Nt_pre_control, Nt_post_control)
    
    
    # plot
    plot(x = input$start_date_outbreak + 1:length(Nt_no_control) * days(input$serial_interval),
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
    
    lines(x = input$start_date_outbreak + 1:length(Nt_with_control) * days(input$serial_interval),
          y = Nt_with_control,
          lwd = 2,
          col = 'orange')
    
    options(scipen = 999)
    axis(2, at = seq(0, 300000, 100000))
    
    axis.Date(1, at = seq(ymd('2019-11-01'), ymd('2020-03-31'), by = "2 week"),
              format = "%m-%d-%Y")
    
    abline(v = ymd(input$start_date_control), col = 'black', lty = 'dashed')
    text(x = ymd(input$start_date_control), y = 2.75e5, "Control start", pos = 2)
    
    abline(h = input$num_beds, col = 'black', lty = 'dashed')
    text(x = ymd(input$start_date_outbreak), y = input$num_beds, "Number of hospital beds", pos = 3)
    
    grid(nx = NA, ny = NULL)
    legend("topleft", c("No control", "With control"), fill = c('red', 'orange'))
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, 'updateSliderInput', value = 7)
    updateSliderInput(session, 'start_date_outbreak', value = ymd('2019-12-01'))
    updateSliderInput(session, 'start_date_control', value = ymd('2020-01-15'))
    updateSliderInput(session, 'it0', value = 1)
    updateSliderInput(session, 'r0', value = 2.3)
    updateSliderInput(session, 're', value = 1.5)
    updateSliderInput(session, 'num_beds', value = 1000)
  })
}

shinyApp(ui, server)


