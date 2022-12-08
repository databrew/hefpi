#' @rawNamespace import(shiny, except = runExample)
#' @import shinydashboard
#' @rawNamespace import(shinyWidgets, except = alert)
#' @rawNamespace import(shinyjs, except = show)
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import tidyr
#' @import htmltools
#' @import RColorBrewer
#' @import sp
#' @import leaflet
#' @import waiter
#' @import plotly
#' @importFrom shiny NS tagList 


app_server <- function(input, output,session) {
  shinyOptions(cache = cachem::cache_disk("./bind-cache"))
  suppressWarnings({
  
  # w <- waiter::Waiter$new(html = spin_loader(), color = "#FFF")
  # webshot::install_phantomjs()
  # Capture URL parameters
  # shinyURL.server()

  # MOST RECENT VALUE MAPS
  shiny::callModule(mod_recent_mean_server, 'recent_mean_leaf')
  shiny::callModule(mod_recent_radar_server, 'recent_radar')
  
  shiny::callModule(mod_recent_con_server, 'recent_con_leaf')
  shiny::callModule(mod_recent_mean_sub_server, 'recent_mean_sub_leaf')
  # 
  # # TRENDS TAB
  shiny::callModule(mod_trends_mean_server, 'trends_mean')
  shiny::callModule(mod_rural_server, 'rural')
  # 
  shiny::callModule(mod_trends_quin_server, 'trends_quin')
  shiny::callModule(mod_trends_con_server, 'trends_con')
  # 
  # # QUINTILES TAB
  shiny::callModule(mod_dots_country_server, 'dots_country')
  shiny::callModule(mod_dots_ind_server, 'dots_ind')
  # 
  # # DATA AVAILABILITY TAB
  shiny::callModule(mod_dat_country_server, 'dat_country')
  shiny::callModule(mod_dat_ind_server, 'dat_ind')
  
  ## Social
  callModule(mod_social_server, "social_module_1")
  
  # data availability alternate tab
  callModule(mod_dat_ind_alt_server, 'dat_ind_alt1')
  
  })
  
  
}
