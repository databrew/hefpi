
### This is a script to change the indicator short names needed to change the labels per Sven's request:
# Make the following changes to the labels of the indicators in the dropdowns/chart titles/axis titles
# Full vaccination (%)” >> “Full vaccination, 15-23 months (%)
# Zero vaccination (%) >> Zero vaccinations, 15-23 months (%)
# Measles vaccination (%) >> Measles vaccination, 15-23 months (%)
# Blood pressure measured (%) >> Blood pressure measured, adults (%)
# Blood sugar measured (%) >> Blood sugar measured, adults (%)"	


library(dplyr)
library(tidyverse)


## hefpi_sub_df.rda
load('data/hefpi_sub_df.rda')
hefpi_sub_df <- hefpi_sub_df %>%
  mutate(indicator_short_name = ifelse(indicator_short_name == 'Full vaccination (%)', 
                                       'Full vaccination, 15-23 months (%)', 
                                ifelse(indicator_short_name == 'Zero vaccination (%)',
                                       'Zero vaccinations, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Measles vaccination (%)',
                                       'Measles vaccination, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Blood pressure measured (%)',
                                       'Blood pressure measuredm adults (%)',
                                ifelse(indicator_short_name == 'Blood sugar measured (%)',
                                       'Blood sugar measured, adults (%)', indicator_short_name)))))
         ) 
save(hefpi_sub_df, file = 'data/hefpi_sub_df.rda')

## hefpi_df.rda
load('data/hefpi_df.rda')
hefpi_df <- hefpi_df %>%
  mutate(indicator_short_name = ifelse(indicator_short_name == 'Full vaccination (%)', 
                                       'Full vaccination, 15-23 months (%)', 
                                ifelse(indicator_short_name == 'Zero vaccination (%)',
                                       'Zero vaccinations, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Measles vaccination (%)',
                                       'Measles vaccination, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Blood pressure measured (%)',
                                       'Blood pressure measuredm adults (%)',
                                ifelse(indicator_short_name == 'Blood sugar measured (%)',
                                       'Blood sugar measured, adults (%)', indicator_short_name)))))
         ) 
save(hefpi_df, file = 'data/hefpi_df.rda')

## indicators.rda
load('data/indicators.rda')
indicators <- indicators %>%
  mutate(indicator_short_name = ifelse(indicator_short_name == 'Full vaccination (%)', 
                                       'Full vaccination, 15-23 months (%)', 
                                ifelse(indicator_short_name == 'Zero vaccination (%)',
                                       'Zero vaccinations, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Measles vaccination (%)',
                                       'Measles vaccination, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Blood pressure measured (%)',
                                       'Blood pressure measuredm adults (%)',
                                ifelse(indicator_short_name == 'Blood sugar measured (%)',
                                       'Blood sugar measured, adults (%)', indicator_short_name)))))
         ) 
save(indicators, file = 'data/indicators.rda')

## percentage_inds.rda
load('data/percentage_inds.rda')
percentage_inds <- percentage_inds %>%
  mutate(indicator_short_name = ifelse(indicator_short_name == 'Full vaccination (%)', 
                                       'Full vaccination, 15-23 months (%)', 
                                ifelse(indicator_short_name == 'Zero vaccination (%)',
                                       'Zero vaccinations, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Measles vaccination (%)',
                                       'Measles vaccination, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Blood pressure measured (%)',
                                       'Blood pressure measuredm adults (%)',
                                ifelse(indicator_short_name == 'Blood sugar measured (%)',
                                       'Blood sugar measured, adults (%)', indicator_short_name)))))
         ) 
save(percentage_inds, file = 'data/percentage_inds.rda')



## dots_indicator_default.rda
load('data/dots_indicator_default.rda')
levels(dots_indicator_default[[1]][["indicator_short_name"]])[levels(dots_indicator_default[[1]][["indicator_short_name"]]) == 'Full vaccination (%)'] <- 'Full vaccination, 15-23 months (%)'
levels(dots_indicator_default[[1]][["indicator_short_name"]])[levels(dots_indicator_default[[1]][["indicator_short_name"]]) == 'Measles vaccination (%)'] <- 'Measles vaccination, 15-23 months (%)'
levels(dots_indicator_default[[1]][["indicator_short_name"]])[levels(dots_indicator_default[[1]][["indicator_short_name"]]) == 'Zero vaccination (%)'] <- 'Zero vaccinations, 15-23 months (%)'
levels(dots_indicator_default[[1]][["indicator_short_name"]])[levels(dots_indicator_default[[1]][["indicator_short_name"]]) == 'Blood pressure measured (%)'] <- 'Blood pressure measured, adults (%)'
levels(dots_indicator_default[[1]][["indicator_short_name"]])[levels(dots_indicator_default[[1]][["indicator_short_name"]]) == 'Blood sugar measured (%)'] <- 'Blood sugar measured, adults (%)'
save(dots_indicator_default, file = 'data/dots_indicator_default.rda')

## dat_country_default.rda
load('data/dat_country_default.rda')
levels(dat_country_default[[1]][["indicator_short_name"]])[levels(dat_country_default[[1]][["indicator_short_name"]]) == 'Full vaccination (%)'] <- 'Full vaccination, 15-23 months (%)'
levels(dat_country_default[[1]][["indicator_short_name"]])[levels(dat_country_default[[1]][["indicator_short_name"]]) == 'Measles vaccination (%)'] <- 'Measles vaccination, 15-23 months (%)'
levels(dat_country_default[[1]][["indicator_short_name"]])[levels(dat_country_default[[1]][["indicator_short_name"]]) == 'Zero vaccination (%)'] <- 'Zero vaccinations, 15-23 months (%)'
levels(dat_country_default[[1]][["indicator_short_name"]])[levels(dat_country_default[[1]][["indicator_short_name"]]) == 'Blood pressure measured (%)'] <- 'Blood pressure measured, adults (%)'
levels(dat_country_default[[1]][["indicator_short_name"]])[levels(dat_country_default[[1]][["indicator_short_name"]]) == 'Blood sugar measured (%)'] <- 'Blood sugar measured, adults (%)'
save(dat_country_default, file = 'data/dat_country_default.rda')

## df.rda
load('data/df.rda')
df <- df %>%
  mutate(indicator_short_name = ifelse(indicator_short_name == 'Full vaccination (%)', 
                                       'Full vaccination, 15-23 months (%)', 
                                ifelse(indicator_short_name == 'Zero vaccination (%)',
                                       'Zero vaccinations, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Measles vaccination (%)',
                                       'Measles vaccination, 15-23 months (%)',
                                ifelse(indicator_short_name == 'Blood pressure measured (%)',
                                       'Blood pressure measured, adults (%)',
                                ifelse(indicator_short_name == 'Blood sugar measured (%)',
                                       'Blood sugar measured, adults (%)', indicator_short_name)))))
         ) 
save(df, file = 'data/df.rda')
