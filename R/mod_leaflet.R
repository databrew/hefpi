# Module leaflet UI
  
#' @title   mod_leaflet_ui and mod_leaflet_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_leaflet
#'
#' @keywords internal
#' @export 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_leaflet_ui <- function(id){
  
  ns <- NS(id)
  # tagList(

  
  fluidPage(
    column(8,
           leafletOutput(
             ns('leafy')
           )),
    column(4,
           selectInput('indicator', 'Indicator',
                       choices = indicators_list),
           sliderInput('date_range',
                          'Date range',
                          min = 1982,
                          max = 2017,
                       value = c(1982, 2017),
                       step = 1,
                       sep = ''))
  )
  
}
    
# Module Server
#' @rdname mod_leaflet
#' @export
#' @import leaflet
#' @keywords internal
    
mod_leaflet_server <- function(input, output, session,
                               plot_year = 2015){
  
  # Get the data to be plotted
  pd <- hefpi::df %>%
    filter(year == plot_year)
  
  
  
  output$leafy <- renderLeaflet({
    leaflet() %>% addProviderTiles('Stamen.Toner')
  })
}
    
## To be copied in the UI
mod_leaflet_ui("leaf1")

## To be copied in the server
# callModule(mod_leaflet_server, 'leaf1')

