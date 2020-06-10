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
#' @import shinyalert
#' @importFrom shiny NS tagList 
mod_recent_mean_ui <- function(id){
  ns <- NS(id)
  # tagList(

  fluidPage(
    fluidRow(
      column(2,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults')),
      column(2,
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''))
    ),
    fluidRow(
      column(6,
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info_mean"), "Population Mean"),
             plotlyOutput(
               ns('recent_mean_plot'), 
             )),
      column(6,
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info_con"), "Concentration Index"),
             plotlyOutput(
               ns('recent_con_plot'),
             ))
      ),
    br(), 
    fluidRow(
      column(6,
             leafletOutput(
               ns('recent_mean_leaf'),
             )),
      column(6,
             leafletOutput(
               ns('recent_con_leaf'),
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
  observeEvent(input$plot_info_mean, {
    # Show a modal when the button is pressed
    shinyalert(title = "Most recent value - Population mean", 
               text = "charts display a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info_con, {
    # Show a modal when the button is pressed
    shinyalert(title = "Most recent value - Concentration Index", 
               text = "charts display a world map in which countries are color-coded according to the most recent value of an indicator’s concentration index. The concentration index is bounded between -1 and 1. Negative values indicate disproportionate concentration of a variable among the poor, and positive values disproportionate concentration among the rich. For instance, if the variable is “bad” such as infant mortality, a negative value means infant mortality is higher among the poor. The map is complemented by a bar chart that ranks countries by the concentration index. By default, the map and bar chart use an indicator’s latest available concentration index, but users can choose the time period from which this latest concentration index value is chosen.", 
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
      summarise(pop_value = first(pop),
                ci_value = first(CI),
                year = year,
                data_source = referenceid_list) 
    
    shp <- world
    shp@data <- shp@data %>% left_join(pd)
    
    ## POPULATION MEAN
    
    # Make color palette
    pop_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), domain=shp@data$pop_value, na.color="transparent")
    
    # Make tooltip
    pop_text <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$pop_value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # --------------- population mean map ------------------- #
    output$recent_mean_leaf <- renderLeaflet({
      leaflet(shp) %>% 
        addProviderTiles('Stamen.Toner') %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~pop_palette(pop_value), 
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
        addLegend( pal=pop_palette, values=~pop_value, opacity=0.9, title = "", position = "bottomleft" )
    })
    
    # --------------- population mean barchart ------------------- #
    output$recent_mean_plot <- renderPlotly({
      
      # get data from shp and remove NA
      temp <- shp@data
      temp <- temp %>% filter(!is.na(pop_value))
      
      if(all(is.na(temp$pop_value))){
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
        temp$NAME <- factor(temp$NAME, levels = unique(temp$NAME)[order(temp$pop_value, decreasing = TRUE)])
        # get text for plotly 
        pop_bar_text <- paste(
          "Country: ", as.character(temp$NAME),"\n", 
          "Value: ", round(temp$pop_value, digits = 3), "\n",
          "Year: ", as.character(temp$year),"\n",
          "Data source :", as.character(temp$data_source), "\n",
          sep="") %>%
          lapply(htmltools::HTML)
        plot_title = paste0('Most recent value - population mean - ', indicator)
        temp <- highlight_key(temp, key=~NAME)
        # plotly plot
        p <- plot_ly(temp, x = ~NAME, y = ~pop_value, type = 'bar', text = pop_bar_text, hoverinfo = 'text', 
                     marker = list(color='#469CD8')) %>%
          layout(title = plot_title,
                 xaxis= list(title = '', showticklabels = TRUE),
                 yaxis= list(title = 'Value', showticklabels = TRUE)) %>% 
          toWebGL() %>%
          highlight(on='plotly_hover',
                    color = 'blue',
                    opacityDim = 0.6)
        
      }
      
      return(p)
    })
    
    ## CONCENTRATION INDEX
    
    # Make color palette
    ci_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), domain=shp@data$ci_value, na.color="transparent")
    
    # Make tooltip
    ci_text <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$ci_value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # --------------- concentration index map ------------------- #
    output$recent_con_leaf <- renderLeaflet({
      leaflet(shp) %>% 
        addProviderTiles('Stamen.Toner') %>%
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~ci_palette(ci_value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = ci_text,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>%
        addLegend( pal=ci_palette, values=~ci_value, opacity=0.9, title = "", position = "bottomleft" )
    })
    
   
    
    # --------------- concentration index barchart ------------------- #
    output$recent_con_plot <- renderPlotly({
      
      # get data from shp
      temp <- shp@data
      temp <- temp %>% filter(!is.na(ci_value))
      
      if(all(is.na(temp$ci_value))){
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
       
        temp$NAME <- as.character(temp$NAME)
        ci_bar_text <- paste(
          "Country: ", as.character(temp$NAME),"\n", 
          "Value: ", round(temp$ci_value, digits = 3), "\n",
          "Year: ", as.character(temp$year),"\n",
          "Data source :", as.character(temp$data_source), "\n",
          sep="") %>%
          lapply(htmltools::HTML)
        plot_title = paste0('Most recent value - concentration index - ', indicator)
        temp <- highlight_key(temp, key=~NAME)
        
        # plotly plot
        p <- plot_ly(temp, x = ~NAME, y = ~ci_value, type = 'bar',text = ci_bar_text, hoverinfo = 'text',
                     marker = list(color='#469CD8')) %>%
          layout(title = plot_title,
                 xaxis= list(title = '', showticklabels = TRUE),
                 yaxis= list(title = 'Value', showticklabels = TRUE)) %>%
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
# mod_recent_mean_ui("leaf1")
# mod_recent_con_ui("con1")


## To be copied in the server
# callModule(mod_recent_mean_server, 'leaf1')
# callModule(mod_recent_con_server, 'con1')

