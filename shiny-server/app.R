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
  select(COUNTYFIPS, COUNTY, BEDS) %>% rename(FIPS = COUNTYFIPS, beds = BEDS)
bed_df <- bed_df %>% group_by(FIPS) %>%
  summarize(num_beds = sum(beds)) %>% mutate(FIPS = as.numeric(FIPS))
df <- left_join(df, bed_df, by = 'FIPS')

ui <- shinyUI(
  
  navbarPage("Projecting Severe Cases of COVID-19",
             
             tabPanel("Calculator",
                      sidebarPanel(
                        fluidRow(
                          column(12, 
                                 uiOutput("state_selector_1"),
                                 uiOutput("county_selector_1"),
                                 hr(),
                                 h4("Baseline"),
                                 numericInput("num_cases", "Current Cases (Today)", 138, min = 1),
                                 numericInput("num_days", "Number of Days to Project Forward", 50, min = 1),
                                 numericInput("doubling_time", "Doubling Time (Days)", 6, min = 1, max = 20),
                                 hr(),
                                 h4("Interventions"),
                                 p("Doubling times (DT) can be changed at up to three time points to reflect interventions."),
                                 fluidRow(
                                   column(6, tags$b("On Day")),
                                   column(6, tags$b("New DT"))
                                 ),
                                 fluidRow(
                                   column(6,
                                          numericInput("day_change_1", "", NA, min = 1),
                                          numericInput("day_change_2", "", NA, min = 1),
                                          numericInput("day_change_3", "", NA, min = 1)
                                   ),
                                   column(6,
                                          numericInput("double_change_1", "", NA, min = 1),
                                          numericInput("double_change_2", "", NA, min = 1),
                                          numericInput("double_change_3", "", NA, min = 1)
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
                        textOutput("text2"),
                        textOutput("text1"),
                        tags$head(tags$style("#text1, #text2 {color: black; font-size: 20px; white-space: pre-wrap;}")),
                        br(),
                        tableOutput("table1"),
                        hr(),
                        plotlyOutput("plot1"),
                        hr(),
                        plotlyOutput("plot2"),
                        width = 9
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
                label = "County/ies:", #label displayed in ui
                choices = sort(unique(data_available)), #calls list of available counties
                selected = "Santa Clara County",
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
  
  get_county_df <- reactive({
    state <- input$state1
    state_df <- df %>% filter(State == state)
    counties <- input$county1
    county_df <- state_df %>%
      filter(County %in% counties)
    county_df
  })
  
  get_naive_estimations <- reactive({
    
    county_df <- get_county_df()
    num_cases <- input$num_cases
    doubling_time <- input$doubling_time
    
    naive_estimations <- county_df %>% group_by(County) %>% 
      mutate(relative_pop = population_in_age_group/sum(population_in_age_group)) %>% 
      mutate(estimated_cases = num_cases*relative_pop) %>% 
      mutate(estimated_fatal_cases = estimated_cases*case_fatality_rate) %>% 
      mutate(estimated_critical_cases = estimated_cases*critical_case_rate) %>% 
      mutate(estimated_severe_cases = estimated_cases*severe_cases_rate)
    
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
      text <- paste(c(text, 'Assuming no interventions, these will be filled within ', days_to_fill, " days. \n"), collapse = "")
    }
    
    dt_changes = get_dt_changes()
    if(length(dt_changes) > 0 & bed_total > 0) {
      message("ninja")
      cases_w_interventions <- get_case_numbers()
      intervention_hospitalizations = cases_w_interventions$critical + cases_w_interventions$severe
      beds_remaining = bed_total - intervention_hospitalizations
      first_day_without_beds = min(which(beds_remaining<=0)) - 1
      print(first_day_without_beds)
      if(length(first_day_without_beds > 0) & first_day_without_beds < Inf) {
        text <- paste(text, 'With the interventions, these will be filled within ', first_day_without_beds, " days. \n", sep = "")
      } else {
        text <- paste(text, 'With the interventions, these will not be filled within ', input$num_days, " days. \n", sep = "")
      }
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
  
  output$plot1 <- renderPlotly({
    req(input$county1)
    
    case_numbers <- get_case_numbers()
    fatal_cases <- case_numbers$fatal
    critical_cases <- case_numbers$critical
    severe_cases <- case_numbers$severe
    
    num_beds <- sum((get_naive_estimations() %>% group_by(County) %>% summarize(num_beds = max(num_beds)))$num_beds)
    n_days <- input$num_days
    day_list <- c(0:n_days)
    
    y_axis <- 'Cases'
    
    chart_data = melt(data.table(
      date = Sys.Date() + day_list,
      estimated_hospitalizations = critical_cases + severe_cases,
      severe_cases = severe_cases,
      critical_cases = critical_cases,
      fatal_cases = fatal_cases
    ), id.vars = c('date'))
    chart_data[, value := round(value)]
    
    gp = ggplot(chart_data,
                aes(x=date, y=value, color=variable)) +
      geom_line(size = 1.2) +
      theme_minimal() +
      ylab("") +
      coord_cartesian(ylim=c(0, max(critical_cases + severe_cases)))
    
    if(!is.na(num_beds)) {
      gp = gp +
        geom_hline(yintercept = num_beds, linetype = "dashed") + 
        annotate("text", x = Sys.Date() + 0.5*n_days, y = num_beds*1.05, label = "Number of Hospital Beds", vjust=0, hjust=0) +
        coord_cartesian(ylim=c(0, pmax(num_beds*1.05, max(critical_cases + severe_cases))))
    }
    
    ggplotly(gp) %>% 
      layout(
        legend = list(x = 0.02, y = 0.1, bgcolor = 'rgba(0,0,0,0)'),
        xaxis=list(fixedrange=TRUE),
        yaxis=list(fixedrange=TRUE)) %>% 
      config(displayModeBar = F)
    
  })
  
  output$text2 <- renderText({
    req(input$county1)
    county_df <- get_county_df()
    
    text = ""
    for (county in input$county1) {
      under_60 = sum((county_df %>% filter(age_decade %in% c('0-9','10-19','20-29','30-39','40-49','50-59') & County == county))$population_in_age_group)
      over_60 = sum((county_df %>% filter(age_decade %in% c('60-69', '70-79', '80+') & County == county))$population_in_age_group)
      county_text = paste(c(county, "has", format(under_60, big.mark=","), "people aged 0-59 and",
                            format(over_60, big.mark=","), "people aged 60+. \n"), collapse = " ")
      text = paste(text, county_text, sep = '')
    }
    
    text
  })
  
  output$table1 <- renderTable({
    
    req(input$county1)
    
    county_df <- get_county_df()
    
    total_population = sum(county_df['population_in_age_group'])
    case_mortality_rate <- sum(county_df['case_fatality_rate']*county_df['population_in_age_group'])/total_population
    case_critical_rate <- sum(county_df['critical_case_rate']*county_df['population_in_age_group'])/total_population
    case_severe_rate <- sum(county_df['severe_cases_rate']*county_df['population_in_age_group'])/total_population
    critical_plus_severe <- case_critical_rate + case_severe_rate
    
    table <- data.table(" " = c('Population-specific severity rate<br>(per 100 cases)'),
                        "Mortality/Ventilator<br>(Subset of Critical)" = c(round(case_mortality_rate*100, digits = 2)),
                        "Critical" = c(round(case_critical_rate*100, digits = 2)),
                        "Severe"= c(round(case_severe_rate*100, digits = 2)),
                        "Hospitalization<br>(Critical + Severe)"= c(round(critical_plus_severe*100, digits = 2)))
    
    table
    
  }, sanitize.text.function=identity, width = "100%")
  
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
  
}

shinyApp(ui, server)


