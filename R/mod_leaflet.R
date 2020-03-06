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
#' @import RColorBrewer
#' @import htmltools
#' @param plot_years Years to plot
#' @keywords internal
    
mod_leaflet_server <- function(input, output, session,
                               plot_years = 1982:2015,
                               indicator = "Proportion of population spending more than 10% of household consumption or income on out-of-pocket health care expenditure (%)"){
  
  # Get the variable
  variable <- indicators %>%
    filter(indicator_name == indicator) %>%
    .$variable_name
  
  # Get the data to be plotted
  pd <- hefpi::df %>%
    filter(year >= min(plot_years),
           year <= max(plot_years)) %>%
    filter(indic == variable) %>%
    group_by(ISO3 = iso3c) %>%
    summarise(value = mean(pop))
  
  shp <- world
  shp@data <- shp@data %>% left_join(pd)
  
  # Make color palette
  mypalette <- colorBin( palette="YlOrBr", domain=shp@data$pop, na.color="transparent")
  
  # Make tooltip
  mytext <- paste(
    "Country: ", as.character(shp@data$NAME),"<br/>", 
    "Value: ", round(shp@data$value, digits = 3), "<br/>", 
    sep="") %>%
    lapply(htmltools::HTML)
  
  output$leafy <- renderLeaflet({
    leaflet(shp) %>% 
      addProviderTiles('Stamen.Toner') %>%
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(value), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~value, opacity=0.9, title = "", position = "bottomleft" )
  })
}
    
## To be copied in the UI
# mod_leaflet_ui("leaf1")

## To be copied in the server
# callModule(mod_leaflet_server, 'leaf1')

