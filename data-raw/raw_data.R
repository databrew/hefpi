## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

# Read in the indicators hierarchy data from WB
indicators <- readxl::read_excel('from_wb/Indicator_description.xlsx')
names(indicators) <- tolower(gsub(' ', '_', names(indicators)))
usethis::use_data(indicators, overwrite = T)



# Read in the full database
df <- haven::read_dta('from_wb/hefpi_full_database.dta')
df$bin <- 
  ifelse(df$level1 == 'fp', 'Financial Protection',
         ifelse(df$level2 == 'h_cov', 'Healthcare Coverage',
                ifelse(df$level2 == 'h_out', 'Health Outcomes', NA)))

# Get nicer indicator names in full database
df <- df %>%
  left_join(right %>%
              dplyr::select(variable_name:unit_of_measure),
            by = c('indic' = 'variable_name'))
usethis::use_data(df, overwrite = T)

# Get indicators in a form usable for the selectInput function
indicators_list <- 
  df %>%
  dplyr::distinct(bin, indic) %>%
  filter(!is.na(bin)) %>%
  arrange(bin, indic) %>%
  left_join(indicators %>% dplyr::select(variable_name, indicator_name, indicator_description, unit_of_measure),
            by = c('indic' = 'variable_name'))
indicator_groups <- unique(indicators_list$bin)
out_list <- list()
for(i in 1:length(indicator_groups)){
  this_group <- indicator_groups[i]
  these_elements <- indicators_list %>% filter(bin == this_group)
  these_elements <- as.list(these_elements$indicator_name)
  out_list[[this_group]] <- these_elements
}  
indicators_list <- out_list
usethis::use_data(indicators_list, overwrite = T)


# # Read in the raw data from the website
library(tidyverse)
country <- read_csv('from_website/HEFPICountry.csv')
dat <- read_csv('from_website/HEFPIData.csv')
series <- read_csv('from_website/HEFPISeries.csv')

