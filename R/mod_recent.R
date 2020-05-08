# Module recent value UI
  
#' @title   mod_recent_value 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_recent_mean_ui
#' @title mod_leaflet_mean_ui
#' @keywords internal
#' @export 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_recent_mean_ui <- function(id){
  ns <- NS(id)
  # tagList(

  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('recent_mean_leaf')
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''))
    ),
    br(), br(),
    fluidRow(
      column(8,
             plotlyOutput(
               ns('recent_mean_plot')
             ))
    )
    
  )
  
}
    
# Module Server
#' @rdname mod_recent_mean_server
#' @export
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import htmltools
#' @keywords internal
    
mod_recent_mean_server <- function(input, output, session){
  
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
    
    output$recent_mean_leaf <- renderLeaflet({
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
    
    output$recent_mean_plot <- renderPlotly({
      
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
      plot_title = paste0('Most recent value - population mean - ', indicator)
      # plotly plot
     p <- plot_ly(temp, x = ~NAME, y = ~value, type = 'bar', text = mytext, hoverinfo = 'text', color = 'red') %>%
        layout(title = plot_title,
              xaxis= list(title = 'Country', showticklabels = FALSE),
               yaxis= list(title = 'Value'))
     return(p)
    })
    
    
  })
}
    
# -------------------------------------------------------------------------------------


#' @rdname mod_recent_con_ui
#' @keywords internal
#' @export 
#' @import leaflet
#' @importFrom shiny NS tagList 
mod_recent_con_ui <- function(id){
  ns <- NS(id)
  # tagList(
  
  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('recent_con_leaf'), 
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''))
    ),
    br(), br(),
    
    fluidRow(
      column(8,
             plotlyOutput(
               ns('recent_con_plot'), width = '1000px'
             ))
    )
    
  )
  
}
# Module Server
#' @rdname mod_recent_con_server
#' @export
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import htmltools
#' @keywords internal

mod_recent_con_server <- function(input, output, session){
  
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
    
    output$recent_con_leaf <- renderLeaflet({
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
    
    output$recent_con_plot <- renderPlotly({
     
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
      plot_title = paste0('Most recent value - concentration index - ', indicator)
      
      # plotly plot
      p <- plot_ly(temp, x = ~NAME, y = ~value, type = 'bar',text = mytext, hoverinfo = 'text', color = 'red') %>%
        layout(title = plot_title,
               xaxis= list(title = 'Country', showticklabels = FALSE),
               yaxis= list(title = 'Value'))
      
      return(p)
    })
    
    
  })
}



## To be copied in the UI
# mod_recent_mean_ui("leaf1")
# mod_recent_con_ui("con1")


## To be copied in the server
# callModule(mod_recent_mean_server, 'leaf1')
# callModule(mod_recent_con_server, 'con1')

