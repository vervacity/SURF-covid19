rm(list = ls())
setwd('~/github/SURF-covid19/etl/data/')

library(dplyr)
library(magrittr)
library(reshape2)
library(viridis)
library(usmap)
library(ggplot2)
library(data.table)
library(plotly)


df <- read.csv('county_age_severity_rates_v6.csv', stringsAsFactors = FALSE)
df$County <- gsub('city', 'City', df$County)

selected_states <- unique(df$State)
state_df <- df %>% filter(State %in% selected_states)
state_df <- state_df %>% group_by(State, FIPS) %>%
    summarize(
        hospitalization_rate =
            100*sum(population_in_age_group*hospitalizations_per_case)/sum(population_in_age_group)) %>%
    rename(state = State, fips = FIPS)

# p <- plot_usmap(data=as.data.table(state_df)[,.(fips, hospitalization_rate)], 
#            values = "hospitalization_rate", 
#            include = selected_states,
#            size = 0.2,
#            text = 'paste(group, round(value, 2), sep = ": ")') +
#   scale_fill_viridis(option='inferno', direction = -1) +
#   labs(title = "Expected number of hospitalizations per 100 symptomatic cases\n(based on the age distribution of the county population)", fill = "Hospitalizations\nper 100\ncases") +
#   theme_void() +
#   theme(legend.position = "right") + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "white"))
# 


map_df <- map_with_data(as.data.table(state_df)[,.(fips, hospitalization_rate)], 
                        values = "hospitalization_rate", include = selected_states)

geom_args <- list()
geom_args[["mapping"]] <- ggplot2::aes(x = map_df$x, 
                                       y = map_df$y,
                                       group = map_df$group, 
                                       fill = map_df$hospitalization_rate,
                                       text = paste(map_df$county, round(map_df$hospitalization_rate, 2), sep = ": "))
geom_args[["size"]] <- 0.2

polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)

p <- ggplot2::ggplot(data = map_df) + polygon_layer + 
  ggplot2::coord_equal() + usmap:::theme_map()  +
  scale_fill_viridis(option='inferno', direction = -1) +
  labs(title = "Expected number of hospitalizations per 100 symptomatic cases\n(based on the age distribution of the county population)", fill = "Hospitalizations\nper 100\ncases") +
  theme_void() +
  theme(legend.position = "right") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "white"))

#ggplotly(p, tooltip = 'text')

ggsave('usmap.svg', p,
       width = 10,
       height = 6)
