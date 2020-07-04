# Module recent value sub UI

#' @title   mod_recent_sub_value 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_recent_mean_sub_ui
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
               ns('recent_mean_sub_leaf'), height = '650px'),
             ),
      column(4,
             pickerInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region[1]),
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(sub_national$indicator_short_name)),
                         selected = sort(unique(sub_national$indicator_short_name))[1],
                         options = list(`style` = "btn-primary")),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
    )#,
  #   br(), br(),
  #   
  #   fluidRow(
  #     column(8,
  #            plotlyOutput(
  #              ns('recent_mean_sub_plot')
  #            ))
  #   )
  #   
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
  
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Recent value- Population mean", 
               text = "charts display a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  get_pop_map <- reactive({
    # to be used for testing 
    indicator <- sort(unique(sub_national$indicator_short_name))[1]
    region = 'Latin America & Caribbean'
    plot_years <- c(1982, 2017)
    
    # get list to store map data
    pop_map_list <- list()
    
    # get input 
    plot_years <- input$date_range
    indicator <- input$indicator
    region  <- input$region
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # Get the data to be plotted
    temp <- hefpi::sub_national[sub_national$region_code == region_code,]
    pd <- temp %>% filter(year >= min(plot_years),
                          year <= max(plot_years)) %>%
      filter(indicator_short_name == indicator) %>%
      group_by(ISO3 = iso3c, country,gaul_code) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      summarise(value = first(value),
                year = year) 
    
    
    # get shape files
    shp <- hefpi::gaul
    
    # joine with data
    shp@data <- shp@data %>% dplyr::left_join(pd, by=c('ADM1_CODE'='gaul_code'))
    
    # remove polygons associated with NA - keeps only that region
    na_rows <- which(!is.na(shp@data$value))
    shp <- shp[na_rows,]
    shp@data$ADM1_NAME <- as.character(shp@data$ADM1_NAME)
    
    # Define centroid
    centroid <- coordinates(shp)
    centroid <- data.frame(centroid)
    names(centroid) <- c('x', 'y')
    centroid <- centroid %>%
      summarise(x = mean(x, na.rm = TRUE),
                y = mean(y, na.rm = TRUE))
    
    # Make color palette
    mypalette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="transparent")
    
    # Make tooltip
    mytext <- paste(
      "Indicator: ", as.character(indicator),"<br/>", 
      "Economy: ", as.character(shp@data$ADM1_NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # create map
    pop_map <- leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
      addProviderTiles('OpenStreetMap.DE', options=providerTileOptions(noWrap = TRUE)) %>%
      addPolygons( 
        color = 'black',
        fillColor = ~mypalette(value), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        weight=1,
        label = mytext,
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
      # setView(lat=0, lng=0 , zoom=1.7) %>%
      setView(lat=centroid$y, lng=centroid$x , zoom=4) %>%
      addLegend( pal=mypalette, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
    
    # store palette, text, map object, and data
    pop_map_list[[1]] <- mypalette
    pop_map_list[[2]] <- mytext
    pop_map_list[[3]] <- pop_map
    pop_map_list[[4]] <- shp
    return(pop_map_list)
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_mean_sub_leaf <- renderLeaflet({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      mytext <- pop_map[[1]]
      mypalette <- pop_map[[2]]
      this_map <- pop_map[[3]]
      this_map 
    }
    
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # get map
      pop_map <- get_pop_map()
      this_map <- pop_map[[4]]
      if(is.null(this_map)){
        NULL
      } else {
        temp <- this_map@data
        temp <- temp %>% filter(!is.na(value))
        
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(
    filename = paste0( Sys.Date()
                       , "_population_mean_map"
                       , ".png"
    )
    
    , content = function(file) {
      pop_map <- get_pop_map()
      if(is.null(pop_map)){
        NULL
      } else {
        # get map
        this_map <- pop_map[[3]]
        
        mapview::mapshot( x = this_map,
                          file = file,
                          cliprect = "viewport",
                          selfcontained = FALSE)
      }
      
    })
  
  # STOP HERE
# 
#   output$recent_mean_sub_plot <- renderPlotly({
#     pop_map <- get_pop_map()
#     plot_years <- input$date_range
#     indicator <- input$indicator
#     if(is.null(pop_map)){
#       NULL
#     } else {
#       shp <- pop_map[[4]]
#     }
# 
#     # get data from shp and remove NA
#     temp <- shp@data
#     # temp <- temp %>% filter(!is.na(value))
#     temp$ADM1_NAME <- as.character(temp$ADM1_NAME)
# 
#     if(all(is.na(temp$value))){
#       empty_plot <- function(title = NULL){
#         p <- plotly_empty(type = "scatter", mode = "markers") %>%
#           config(
#             displayModeBar = FALSE
#           ) %>%
#           layout(
#             title = list(
#               text = title,
#               yref = "paper",
#               y = 0.5
#             )
#           )
#         return(p)
#       }
#       p <- empty_plot("No data available for the selected inputs")
#     } else {
#       # order countries by value
#       temp$ADM1_NAME <- factor(temp$ADM1_NAME, levels = unique(temp$ADM1_NAME)[order(temp$value, decreasing = TRUE)])
#       # get text for plotly
#       # Make tooltip
#       mytext <- paste(
#         "Country: ", as.character(temp$ADM1_NAME),"<br/>",
#         "Value: ", round(temp$value, digits = 3), "<br/>",
#         "Year: ", as.character(temp$year),"<br/>",
#         sep="") %>%
#         lapply(htmltools::HTML)
#       plot_title = paste0('Population mean - ', indicator)
#       y_axis_text = 'Population mean'
#       temp <- highlight_key(temp, key=~ADM1_NAME)
#       # plotly plot
#       # plotly plot
#       print(ggplotly(ggplot(temp, aes(ADM1_NAME, value, text = mytext)) +
#                        geom_bar(stat = 'identity', aes(fill = value)) +
#                        scale_fill_distiller(palette = "Greens", direction = 1) +
#                        labs(x='Country',
#                             y = y_axis_text,
#                             title = plot_title) +
#                        hefpi::theme_gdocs() +
#                        theme(panel.grid.major.x = element_blank(),
#                              axis.text.x = element_blank(),
#                              axis.ticks = element_blank()),
#                      tooltip = 'text')   %>%
#               highlight(on='plotly_hover',
#                         color = 'darkgreen',
#                         opacityDim = 0.6))
# 
#     }
# 
#     return(p)
#   })
#   
}


## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')
