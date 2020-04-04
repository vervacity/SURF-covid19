rm(list = ls())

library(data.table)


cond_rates = fread('../data/imperial_rates.csv')

cond_rates = cond_rates[,.(
    age_decade = age_group,
    hospitalizations_per_case = hospitalization_given_symptomatic,
    severe_cases_rate = (1-critical_care_given_hospitalization)*hospitalization_given_symptomatic,
    critical_case_rate = critical_care_given_hospitalization*hospitalization_given_symptomatic,
    case_fatality_rate = fatality_given_symptomatic
)]

age_buckets_by_province = fread('county_age_populations_canada.csv')

merged_pop_fat_rate = merge(
    age_buckets_by_province[, .(province, pruid, age_decade, population_in_age_group = pop)],
    cond_rates,
    by = 'age_decade')

county_case_severity_rates = merged_pop_fat_rate[,.(
    population = sum(population_in_age_group),
    hospitalizations_per_case = weighted.mean(hospitalizations_per_case, population_in_age_group),
    severe_cases_rate = weighted.mean(severe_cases_rate, population_in_age_group),
    critical_case_rate = weighted.mean(critical_case_rate, population_in_age_group),
    case_fatality_rate = weighted.mean(case_fatality_rate, population_in_age_group)
), by = c('province', 'pruid')]

fwrite(county_case_severity_rates, 'province_case_severity_rates.csv')
fwrite(merged_pop_fat_rate, 'province_age_severity_rates.csv')
