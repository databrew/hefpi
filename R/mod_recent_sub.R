# Module recent value sub UI

#' @title   mod_recent_sub_value 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_recent_sub_mean_ui
#' @title mod_leaflet_mean_ui
#' @keywords internal
#' @export 
#' @import leaflet
#' @import shinyalert
#' @importFrom shiny NS tagList 
mod_recent_mean_sub_ui <- function(id){
  ns <- NS(id)
  # tagList(
  
  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('recent_mean_leaf'),
             )),
      column(4,
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region[1])),
             selectInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(sub_national$indic)),
                         selected = 'Inpatient care use, adults'),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
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
#' @rdname mod_recent_mean_sub_server
#' @export
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import rmapshaper
#' @import htmltools
#' @keywords internal

mod_recent_mean_sub_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Most recent value - Population mean", 
               text = "charts display a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
 
  
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
      indicator <- 'c_anc_ur'
    }
    
    # Capture the indicator
    region <- input$region
    if(is.null(region)){
      region <- 'Latin America & Caribbean'
    }
    # as.character(region_list$region[1])
    # # Get the variable
    # variable <- indicators %>%
    #   filter(indicator_short_name == indicator) %>%
    #   .$variable_name
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # Get the data to be plotted
    pd<- hefpi::sub_national %>%
      filter(region_code == region_code) %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == indicator) %>%
      group_by(ISO3 = iso3c, gaul_code) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      summarise(value = first(value),
                year = year) 
    
    shp <- gaul
    # simplify polygons
    # gaul <- hefpi::gaul
    # gaul <- geojson_json(gaul)
    # gaul <- geojson_sp(gaul)
    # gaul <- ms_simplify(gaul, keep = 0.05)
    # 
    shp@data <- shp@data %>% left_join(pd, by=c('ADM1_CODE'='gaul_code'))
    
    
    
    ## POPULATION MEAN
    
    # Make color palette
    pop_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), domain=shp@data$value, na.color="transparent")
    
    # Make tooltip
    pop_text <- paste(
      "Country: ", as.character(shp@data$ADM1_NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    
    # --------------- population mean map ------------------- #
    output$recent_mean_leaf <- renderLeaflet({
      leaflet(shp) %>% 
        addProviderTiles('Stamen.Toner') %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~pop_palette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = pop_text,
          highlightOptions = highlightOptions(
            weight = 1,
            fillOpacity = 0,
            color = "black",
            opacity = 1.0,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>%
        addLegend( pal=pop_palette, values=~value, opacity=0.9, title = "", position = "bottomleft" )
    })
    
    # --------------- population mean barchart ------------------- #
    output$recent_mean_plot <- renderPlotly({
      
      # get data from shp and remove NA
      temp <- shp@data
      temp <- temp %>% filter(!is.na(value))
      temp$ADM1_NAME <- as.character(temp$ADM1_NAME)
      
      if(all(is.na(temp$value))){
        empty_plot <- function(title = NULL){
          p <- plotly_empty(type = "scatter", mode = "markers") %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = title,
                yref = "paper",
                y = 0.5
              )
            )
          return(p)
        } 
        p <- empty_plot("No data available for the selected inputs")
      } else {
        # order countries by value
        temp$ADM1_NAME <- factor(temp$ADM1_NAME, levels = unique(temp$ADM1_NAME)[order(temp$value, decreasing = TRUE)])
        # get text for plotly 
        # Make tooltip
        pop_text <- paste(
          "Country: ", as.character(shp@data$ADM1_NAME),"<br/>", 
          "Value: ", round(shp@data$value, digits = 3), "<br/>",
          "Year: ", as.character(shp@data$year),"<br/>",
          sep="") %>%
          lapply(htmltools::HTML)
        plot_title = paste0('Most recent value - population mean - ', indicator)
        y_axis_text = indicator
        temp <- highlight_key(temp, key=~ADM1_NAME)
        # plotly plot
        p <- plot_ly(temp, x = ~as.character(ADM1_NAME), y = ~value, type = 'bar', text = pop_text, hoverinfo = 'text', 
                     marker = list(color='#469CD8')) %>%
          layout(title = '',
                 xaxis= list(title = '', showticklabels = TRUE),
                 yaxis= list(title = y_axis_text, showticklabels = TRUE)) %>% 
          toWebGL() %>%
          highlight(on='plotly_hover',
                    color = 'blue',
                    opacityDim = 0.6)
        
      }
      
      return(p)
    })
    
  })
  
}



## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')
