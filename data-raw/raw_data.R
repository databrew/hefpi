## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

library(sp)
library(rgdal)
world <- readOGR('from_other/world_small/', 'TM_WORLD_BORDERS_SIMPL-0.3')
usethis::use_data(world, overwrite = T)


# Read in the indicators hierarchy data from WB
indicators <- readxl::read_excel('from_wb/Indicator_description.xlsx')
names(indicators) <- tolower(gsub(' ', '_', names(indicators)))
usethis::use_data(indicators, overwrite = T)

temp <- haven::read_dta('~/Desktop/hefpi_full_database.dta')

# Read in the full database
df <- haven::read_dta('from_wb/hefpi_full_database.dta')
df$bin <- 
  ifelse(df$level1 == 'fp', 'Financial Protection',
         ifelse(df$level2 == 'h_cov', 'Healthcare Coverage',
                ifelse(df$level2 == 'h_out', 'Health Outcomes', NA)))

year_list <- sort(unique(df$year))
usethis::use_data(year_list, overwrite = T)

# Get nicer indicator names in full database
df <- df %>%
  left_join(indicators %>%
            dplyr::select(variable_name:unit_of_measure),
            by = c('indic' = 'variable_name')) %>% 
  select(-c(indicator_short_name, indicator_name, indicator_description, unit_of_measure))
usethis::use_data(df, overwrite = T)

# Get indicators in a form usable for the selectInput function
indicators_list <- 
  df %>%
  dplyr::distinct(bin, indic) %>%
  filter(!is.na(bin)) %>%
  arrange(bin, indic) %>%
  left_join(indicators %>% dplyr::select(variable_name, indicator_short_name, indicator_description, unit_of_measure),
            by = c('indic' = 'variable_name'))
indicator_groups <- unique(indicators_list$bin)
out_list <- list()
for(i in 1:length(indicator_groups)){
  this_group <- indicator_groups[i]
  these_elements <- indicators_list %>% filter(bin == this_group)
  these_elements <- as.list(sort(unique(these_elements$indicator_short_name)))
  out_list[[this_group]] <- these_elements
}  
indicators_list <- out_list
usethis::use_data(indicators_list, overwrite = T)


# # Read in the raw data from the website
library(tidyverse)
country <- read_csv('from_website/HEFPICountry.csv')
dat <- read_csv('from_website/HEFPIData.csv')
series <- read_csv('from_website/HEFPISeries.csv')

# recode columna names
names(country) <- tolower(gsub(' ', '_', names(country)))
names(dat) <- tolower(gsub(' ', '_', names(dat)))
names(series) <- tolower(gsub(' ', '_', names(series)))

# get region code for region
country$region_code <- ifelse(country$region == 'Latin America & Caribbean', 'LCN',
                              ifelse(country$region == 'South Asia', 'SAS',
                                     ifelse(country$region == 'Sub-Saharan Africa', 'SSF',
                                            ifelse(country$region == 'Europe & Central Asia', 'ECS',
                                                   ifelse(country$region == 'Middle East & North Africa', 'MEA', 
                                                          ifelse(country$region == 'East Asia & Pacific', 'EAS', 'NAC'))))))

# join country and dat to get region and country data together
df_series <- inner_join(dat,country, by = c('country_name' = 'short_name'))

# save data: NOTE: explore these later - Not sure these are actually needed. The database data seems to be a function of a combination of this data
usethis::use_data(country, overwrite = T)
usethis::use_data(dat, overwrite = T)
usethis::use_data(series, overwrite = T)
usethis::use_data(df_series, overwrite = T)


# create lists
# get region code because that is how we subset database data (df, above).
# May need to preprocess data differently to make it more efficient
region_list <- as.data.frame(unique(cbind(region = country$region, region_code = country$region_code)))
country_list <- unique(country$short_name)
yn_list <- c('Yes', 'No')

# save data
usethis::use_data(region_list, overwrite = T)
usethis::use_data(country_list, overwrite = T)
usethis::use_data(yn_list, overwrite = T)

