## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

# Read in the indicators hierarchy data from WB
indicators <- readxl::read_excel('from_wb/Indicator_description.xlsx')
names(indicators) <- tolower(gsub(' ', '_', names(indicators)))
usethis::use_data(indicators, overwrite = T)

# Read in the full database
db <- haven::read_dta('from_wb/hefpi_full_database.dta')
usethis::use_data(db, overwrite = T)
