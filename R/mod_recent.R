# Module recent value UI
  
#' @title   mod_recent_value 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_leaflet_mean_ui
#' @title mod_leaflet_mean_ui
#' @keywords internal
#' @export 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_leaflet_mean_ui <- function(id){
  ns <- NS(id)
  # tagList(

  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('leafy')
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''))
    ),
    fluidRow(
      column(8,
             plotlyOutput(
               ns('ploty')
             ))
    )
    
  )
  
}
    
# Module Server
#' @rdname mod_leaflet_mean_server
#' @title mod_leaflet_mean_server
#' @export
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import htmltools
#' @keywords internal
    
mod_leaflet_mean_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent({
    input$date_range
    input$indicator
    1
  }, {
    # Capture the plot_years
    plot_years <- input$date_range
    if(is.null(plot_years)){
      plot_years <- c(1982, 2017)
    }
    
    # Capture the indicator
    indicator <- input$indicator
    if(is.null(indicator)){
      indicator <- 'BMI, adults'
    }
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # Get the data to be plotted
    pd<- hefpi::df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == variable) %>%
      group_by(ISO3 = iso3c) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(pop),
                year = year,
                data_source = referenceid_list) 
    
    shp <- world
    shp@data <- shp@data %>% left_join(pd)
    
    # Make color palette
    mypalette <- colorBin( palette="YlOrRd", domain=shp@data$pop, na.color="transparent")
    
    # Make tooltip
    mytext <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    output$leafy <- renderLeaflet({
      leaflet(shp) %>% 
        addProviderTiles('Stamen.Toner') %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~mypalette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>%
        addLegend( pal=mypalette, values=~value, opacity=0.9, title = "", position = "bottomleft" )
    })
    
    output$ploty <- renderPlotly({
      
      # get data from shp and remove NA
      temp <- shp@data
      temp <- temp %>% filter(!is.na(value))
      
      # order countries by value
      temp$NAME <- factor(temp$NAME, levels = unique(temp$NAME)[order(temp$value, decreasing = TRUE)])
      
      # get text for plotly 
      mytext <- paste(
        "Country: ", as.character(temp$NAME),"\n", 
        "Value: ", round(temp$value, digits = 3), "\n",
        "Year: ", as.character(temp$year),"\n",
        "Data source :", as.character(temp$data_source), "\n",
        sep="") %>%
        lapply(htmltools::HTML)
      # plotly plot
     p <- plot_ly(temp, x = ~NAME, y = ~value, type = 'bar', text = mytext, hoverinfo = 'text', color = 'red') %>%
        layout(xaxis= list(title = 'Country', showticklabels = FALSE),
               xaxis= list(title = 'Value'))
     return(p)
    })
    
    
  })
}
    
# -------------------------------------------------------------------------------------


#' @rdname mod_leaflet_con_ui
#' @title mod_leaflet_con_ui
#' @keywords internal
#' @export 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_leaflet_con_ui <- function(id){
  ns <- NS(id)
  # tagList(
  
  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('leafy'), 
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''))
    ),
    fluidRow(
      column(8,
             plotlyOutput(
               ns('ploty'), width = '1000px'
             ))
    )
    
  )
  
}
# Module Server
#' @rdname mod_leaflet_con_server
#' @title mod_leaflet_mean_server
#' @export
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import htmltools
#' @keywords internal

mod_leaflet_con_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent({
    input$date_range
    input$indicator
    1
  }, {
    # Capture the plot_years
    plot_years <- input$date_range
    if(is.null(plot_years)){
      plot_years <- c(1982, 2017)
    }
    
    # Capture the indicator
    indicator <- input$indicator
    if(is.null(indicator)){
      indicator <- 'Catastrophic health spending, 10%'
    }
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # Get the data to be plotted
    pd<- hefpi::df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == variable) %>%
      group_by(ISO3 = iso3c) %>%
      filter(year == max(year)) %>%
      filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(CI),
                year = year,
                data_source = referenceid_list) 
    
    shp <- world
    shp@data <- shp@data %>% left_join(pd)
    
    # Make color palette
    mypalette <- colorBin( palette="YlOrRd", domain=shp@data$pop, na.color="transparent")
    
    # Make tooltip
    mytext <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    output$leafy <- renderLeaflet({
      leaflet(shp) %>% 
        addProviderTiles('Stamen.Toner') %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~mypalette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>%
        addLegend( pal=mypalette, values=~value, opacity=0.9, title = "", position = "bottomleft" )
    })
    
    output$ploty <- renderPlotly({
     
      # get data from shp
      temp <- shp@data
      temp <- temp %>% filter(!is.na(value))
      
      # text for plot
      mytext <- paste(
        "Country: ", as.character(temp$NAME),"\n", 
        "Value: ", round(temp$value, digits = 3), "\n",
        "Year: ", as.character(temp$year),"\n",
        "Data source :", as.character(temp$data_source), "\n",
        sep="") %>%
        lapply(htmltools::HTML)
      temp$NAME <- as.character(temp$NAME)
      # plotly plot
      p <- plot_ly(temp, x = ~NAME, y = ~value, type = 'bar',text = mytext, hoverinfo = 'text', color = 'red') %>%
        layout(xaxis= list(title = 'Country', showticklabels = FALSE),
               xaxis= list(title = 'Value'))
      
      return(p)
    })
    
    
  })
}



## To be copied in the UI
# mod_leaflet_mean_ui("leaf1")
# mod_leaflet_con_ui("con1")


## To be copied in the server
# callModule(mod_leaflet_mean_server, 'leaf1')
# callModule(mod_leaflet_con_server, 'con1')

