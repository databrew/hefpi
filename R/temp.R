# library(rdhs)
# library(stringr)
# # SvenDHS1!
# # Attaching an example file for DHS – Rp is the subnational region’s mean, Rc is its code (based on variable v024 in micro-data) and Rl is its label. LMK if you have any questions.
# subnational_dhs <- haven::read_dta('from_wb/NewDHS_2020.dta')
# # make R11 uppercase
# subnational_dhs$Rl1 <- str_replace_all(subnational_dhs$Rl1,"[^[:graph:]]", " ")
# subnational_dhs$Rl1 <- toupper(subnational_dhs$Rl1)
# 
# load('spt_dat.rda')
# 
# # join by adm1_name
# 
# 
# # get subnational data from dhs
# # set up your credentials
# set_rdhs_config(email = "sneelsen@worldbank.org",
#                 project = "Health Equity and Financial Protection Indicators - HEFPI")
# 
# 
# d <- dhs_data(countryIds = c('AF'),
#               indicatorIds = "FE_FRTR_W_A15",
#               surveyYearStart = 2015,
#               breakdown = "subnational")
# 
# # get our related spatial data frame object
# sp <- rdhs::dhs_geometry(surveyId = d$SurveyId[1])
# # # get a list of all available surveys
# # survey_list<- dhs_surveys()
# # 
# # # Set download criteris
# survs <- dhs_surveys(countryIds = c('AF', 'MV', 'ML', 'NG', 'TJ'),
#                      surveyType = c('DHS'),
#                      surveyYearStart = 2015)
# 
# # download Egypt 2014 DHS survey data
# datasets <- dhs_datasets(surveyIds = survs$SurveyId,fileFormat = 'flat')
# datasets <- datasets[datasets$FileType=='Geospatial Covariates',]
# 
# # download datasets
# downloads <- rdhs::get_datasets(datasets)
# 
# # "Afghanistan"
# #none
# # "Maldives"
# #none
# 
# # "Mali"
# mali <- readRDS('/home/benbrew88/.cache/rdhs/datasets/MLGC7BFL.rds')
# View(mali@data)
# # "Nigeria"
# nigeria <- readRDS('/home/benbrew88/.cache/rdhs/datasets/NGGE7BFL.rds')
# 
# # "Tajikistan"
# tajikistan <- readRDS('/home/benbrew88/.cache/rdhs/datasets/TJGE71FL.rds')
# 
# # combine data
# spt_dat <- rbind(mali, nigeria, tajikistan)
# 
# saveRDS(spt_dat, file = 'from_dhs/dhs_subnational_spatial.rda')
