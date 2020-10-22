## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

library(sp)
library(rgdal)

# read in default map parameters
sn_map_params <- read.csv('from_other/default_map_parameters.csv')
usethis::use_data(sn_map_params, overwrite = TRUE)
world <- readOGR('from_other/world_small/', 'TM_WORLD_BORDERS_SIMPL-0.3')
usethis::use_data(world, overwrite = T)

# # Read in gaul codes (downloaded from https://blog.gdeltproject.org/global-second-order-administrative-divisions-now-available-from-gaul/)
# gaul <- read.delim('from_other/gaul/GNS-GAUL-ADM2-CROSSWALK.TXT')

# Read in the Gaul shp files for MICS subnational and the DHS shape files for DHS subnational and combine them
# ---------------------------
# Gaul shapefile (downloaded from https://worldmap.harvard.edu/data/geonode:g2008_1)
# save spatial data fro dhs 
gaul <- readOGR('from_other/gaul/g2008_1/')
usethis::use_data(gaul, overwrite = T)

# read in dhs subnational data
afghan <- readOGR('from_dhs/afghanistan/shps/')
maldives <- readOGR('from_dhs/maldives/shps/')
tajik <- readOGR('from_dhs/tajikistan/shps/')
mali <- readOGR('from_dhs/mali/shps/')
nigeria <- readOGR('from_dhs/nigeria/shps/')

dhs_shp <- rbind(afghan, maldives, tajik, mali, nigeria)
rm(afghan, maldives, tajik, mali, nigeria)

# subset dhs_shp by needed columns
dhs_shp@data <- dhs_shp@data %>% select(CNTRYNAMEE, REGCODE, DHSREGEN)
gaul@data <- gaul@data %>% select( ADM0_NAME, ADM1_CODE, ADM1_NAME)

# rename columns so we can bind them
names(dhs_shp) <- c('country_name', 'reg_code', 'reg_name')
names(gaul) <- c('country_name', 'reg_code', 'reg_name')

# combine data
sub_national_shp <- rbind(dhs_shp, gaul)
usethis::use_data(sub_national_shp, overwrite = T)


# Read in the indicators hierarchy data from WB
indicators <- readxl::read_excel('from_wb/indicator_description.xlsx')
names(indicators)[1:2] <- c('level 1', 'level 2')
names(indicators) <- tolower(gsub(' ', '_', names(indicators)))

usethis::use_data(indicators, overwrite = T)



################################################################

# Read in the full database
df <- haven::read_dta('from_wb/hefpi_full_database.dta')
df$bin <- 
  ifelse(df$level1 == 'fp', 'Financial Protection',
         ifelse(df$level2 == 'h_cov', 'Healthcare Coverage',
                ifelse(df$level2 == 'h_out', 'Health Outcomes', NA)))

# get regioncode code for regioncode
df$region_name <- ifelse(df$regioncode == 'LCN', 'Latin America & Caribbean', 
                              ifelse(df$regioncode =='SAS', 'South Asia', 
                                     ifelse(df$regioncode == 'SSF','Sub-Saharan Africa', 
                                            ifelse(df$regioncode =='ECS', 'Europe & Central Asia', 
                                                   ifelse(df$regioncode == 'MEA','Middle East & North Africa',  
                                                          ifelse(df$regioncode == 'EAS','East Asia & Pacific',  'NAC'))))))

year_list <- sort(unique(df$year))
usethis::use_data(year_list, overwrite = T)

# Get nicer indicator names in full database
df <- left_join(df, indicators, by = c('indic' = 'variable_name')) 

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

# read in WB subnational dhs data 
# Attaching an example file for DHS – Rp is the subnational region’s mean, Rc is its code (based on variable v024 in micro-data) and Rl is its label. LMK if you have any questions.
sub_national_dhs <- haven::read_dta('from_wb/NewDHS_2020.dta')

# HERE SHOULD COMBINE WITH OTHER SUBNATIONAL AND JUST CALL IT GAUL CODE
# Restructure
x <- sub_national_dhs %>% dplyr::select(year, indic, survey, country, iso3c, iso2c, Rp1:Rl34)
names(x) <- gsub('Rp', 'value_', names(x))
names(x) <- gsub('Rc', 'key_', names(x))
out_list <- list()
for(i in 1:34){
  sub_data <- x[,c('year', 'indic','survey', 'country', 'iso3c', 'iso2c', paste0('value_', i), paste0('key_', i))]
  names(sub_data)[7:8] <- c('value', 'gaul_code')
  sub_data <- sub_data %>% dplyr::filter(!is.na(gaul_code))
  out_list[[i]] <- sub_data
}
out <- dplyr::bind_rows(out_list)
sub_national_dhs <- out

temp <- country %>% select(short_name, region, region_code)

# join with country
sub_national_dhs <- left_join(sub_national_dhs, temp, by = c('country'='short_name'))

# get list of indicators from sub_national that are not present in indicators, and remove them temporarily from subnation until sven gives us all the descriptions
sub_national_dhs <- inner_join(sub_national_dhs, indicators, by = c('indic'='variable_name'))

usethis::use_data(sub_national_dhs, overwrite = T)



# As for the subnational points – the dataset indeed has one row per country. But for each country, the Rp* and Rc* variables give the indicator value (Rp) and GAUL code (Admin1) for each of the country’s regions. For instance, for Equatorial Guinea 2000 and indicater c_ITN, Rp1 is the indicator value for the region with the code 1198 (Rc1) and takes on .1153984. The indicator value for the second region (code 1199) is .0848593. And so on. Makes sense?
# ----------------------------------------
# Read in sub-national data (sent June 10 2020)
sub_national <- haven::read_dta('from_wb/GAULdata.dta')
# Restructure
x <- sub_national %>% dplyr::select(year, indic, survey, country, iso3c, iso2c, Rp1:Rc37)
names(x) <- gsub('Rp', 'value_', names(x))
names(x) <- gsub('Rc', 'key_', names(x))
out_list <- list()
for(i in 1:37){
  sub_data <- x[,c('year', 'indic','survey', 'country', 'iso3c', 'iso2c', paste0('value_', i), paste0('key_', i))]
  names(sub_data)[7:8] <- c('value', 'gaul_code')
  sub_data <- sub_data %>% dplyr::filter(!is.na(gaul_code))
  out_list[[i]] <- sub_data
}
out <- dplyr::bind_rows(out_list)
sub_national <- out

temp <- country %>% select(short_name, region, region_code)

# join with country
sub_national <- left_join(sub_national, temp, by = c('country'='short_name'))

# get list of indicators from sub_national that are not present in indicators, and remove them temporarily from subnation until sven gives us all the descriptions
sub_national <- inner_join(sub_national, indicators, by = c('indic'='variable_name'))

usethis::use_data(sub_national, overwrite = T)

##########################################################
# combine sub_national and sub_national dhs
sub_national_data <- rbind(sub_national, sub_national_dhs)
usethis::use_data(sub_national_data, overwrite = T)


# save data: NOTE: explore these later - Not sure these are actually needed. The database data seems to be a function of a combination of this data
# usethis::use_data(country, overwrite = T)
# usethis::use_data(dat, overwrite = T)
# usethis::use_data(series, overwrite = T)
# usethis::use_data(df_series, overwrite = T)


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

t11 <- c(1,2,3,4,5)
usethis::use_data(t11, overwrite = TRUE)



#### ---------------------------------------------### create defualt data objects

# trends national mean

# trends subnational mean

# trends ci 

# trends quin

# dots country

# dots indicator

# dat country

# dat indicator






