#' @import shiny
#' @import shinyURL
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @import ggplot2
#' @import tidyr
#' @import htmltools
#' @import RColorBrewer
#' @import sp
#' @import leaflet
#' @import plotly
#' @importFrom shiny NS tagList 
app_server <- function(input, output,session) {
  
  # Capture URL parameters
  shinyURL.server()

  # MOST RECENT VALUE MAPS
  callModule(mod_recent_mean_server, 'recent_mean_leaf')
  callModule(mod_recent_con_server, 'recent_con_leaf')
  callModule(mod_recent_mean_sub_server, 'recent_mean_sub_leaf')
  # callModule(mod_recent_con_sub_server, 'recent_con_sub_leaf1')
  
  # TRENDS TAB
  callModule(mod_trends_mean_server, 'trends_mean')
  callModule(mod_trends_mean_sub_server, 'trends_sub_mean')
  
  callModule(mod_trends_quin_server, 'trends_quin')
  callModule(mod_trends_con_server, 'trends_con')
  
  # QUINTILES TAB
  callModule(mod_dots_country_server, 'dots_country')
  callModule(mod_dots_ind_server, 'dots_ind')
  
  # DATA AVAILABILITY TAB
  callModule(mod_dat_country_server, 'dat_country')
  callModule(mod_dat_ind_server, 'dat_ind')
  
  ## Social
  # callModule(mod_social_server, "social_module_1")
  
  # data availability alternate tab
  # callModule(mod_dat_country_alt_server, 'dat_country_alt1')
  # callModule(mod_dat_ind_alt_server, 'dat_ind_alt1')
  
  
  
  
  

  output$plot1 <- renderPlot({
    barplot(1:10, col = grey(seq(0, 1, length = 10)),
            main = 'This is a plot',
            sub = "You can't modify it from html/css, it's an image file")
  })
  
  library(leaflet)
  output$l1 <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(data = tibble(lng = rnorm(10, sd = 40),
                               lat = rnorm(10, sd = 40)))
  })
  
  observeEvent(input$action, {
    showModal(
      modalDialog(title = 'This is a modal',
                  size = 'l',
                  easyClose = TRUE,
                  fade = TRUE,
                  fluidPage(
                    fluidRow(
                      column(6, h3('Here is some text'),
                             p('And some sub-text')),
                      column(6, h3('Here is some more text'),
                             helpText('And some helper text'))
                    )
                  ))
    )
  })
  
}
