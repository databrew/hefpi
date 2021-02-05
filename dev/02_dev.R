# 2. All along your project

## 2.1 Add modules
golem::add_module( name = "example_module" ) # https://shiny.rstudio.com/articles/modules.html

## 2.2 Add dependencies # To call each time you need a new package
usethis::use_package("dplyr") 
usethis::use_package("tidyr") 
usethis::use_package("ggplot2") 
usethis::use_package("leaflet") 
usethis::use_package("RColorBrewer") 
usethis::use_package("sp") 
usethis::use_package("shinyjs")
usethis::use_package("shinydashboard")
usethis::use_package("ggimage") 
usethis::use_package("hexSticker") 
usethis::use_package("extrafont") 
usethis::use_package("shinyWidgets")


## 2.3 Add tests
usethis::use_test( "app" )

## 2.4 Add a browser button
golem::browser_button()

## 2.5 Add external files
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation
## 3.1 Vignette
usethis::use_vignette("hefpi")
devtools::build_vignettes()

## 3.2 Code coverage
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
