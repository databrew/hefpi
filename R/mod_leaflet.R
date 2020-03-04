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
  leafletOutput(
    ns('leafy')
  )
}
    
# Module Server
    
#' @rdname mod_leaflet
#' @export
#' @import leaflet
#' @keywords internal
    
mod_leaflet_server <- function(input, output, session){
  output$leafy <- renderLeaflet({
    leaflet() %>% addProviderTiles('Stamen.Toner')
  })
}
    
## To be copied in the UI
# mod_leaflet_ui("example_module_ui_1")
    
## To be copied in the server
# callModule(mod_example_module_server, "example_module_ui_1")
 
