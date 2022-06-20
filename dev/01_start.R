# Building a Prod-Ready, Robust Shiny Application.

# 1 - On init
golem::fill_desc(
  pkg_name = "hefpi", 
  pkg_title = "Health Equity and Financial Protection Indicators", 
  pkg_description = "A Shiny app / R package for the visualization of the World Bank's Health Equity and Financial Protection Indicators", 
  author_first_name = "Ben", 
  author_last_name = "Brew", 
  author_email = "ben@databrew.cc", 
  repo_url = "https://github.com/databrew/hefpi")     

## Use this desc to set {golem} options
golem::set_golem_options()

## 1.2 - Set common Files 
## 
## If you want to use the MIT licence, README, code of conduct, lifecycle badge, and news

#usethis::use_mit_license( name = "Databrew LLC" )  # You can set another licence here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
# usethis::use_news_md( open = FALSE )
usethis::use_git()

## 1.3 - Add a data-raw folder
usethis::use_data_raw( name = "raw_data", open = FALSE )

## 1.4 - Init Tests
# Template for tests
golem::use_recommended_tests()

## 1.5 : Use Recommended Package
golem::use_recommended_deps()

## 1.6 Add various tools

# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon('misc/logo_clear.ico') 

# Add helper functions 
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! 
# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

