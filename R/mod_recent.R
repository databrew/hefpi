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
#' @import webshot
#' @import shinyalert
#' @importFrom shiny NS tagList 
mod_recent_mean_ui <- function(id){
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('recent_mean_leaf'), height = '650px'),
             ),
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
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image'),
             downloadButton(ns("dl_data"), label = 'Download data'),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info"))
               )
             )
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
#' @title mod_recent_mean_server
#' @keywords internal
#' @export 
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import sp
#' @import webshot
#' @import htmltools
#' @keywords internal

mod_recent_mean_server <- function(input, output, session){
  
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
    # create list to store results from reactive object
    pop_map_list <- list()
    
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # Get the variable from indicator input
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # Get the data, subsetted by inputs
    pd <- hefpi::df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == variable) %>%
      group_by(ISO3 = iso3c) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(pop),
                year = year,
                data_source = referenceid_list) 
    
    # get world map shape files
    shp <- world
    
    # join with data
    shp@data <- shp@data %>% left_join(pd)
    
    # Make color palette
    map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="white")
    
    # Make tooltip
    map_text <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # create map
    pop_map <- leaflet(shp, 
                       options = leafletOptions(minZoom = 1, 
                                                maxZoom = 10)) %>% 
      addProviderTiles('OpenStreetMap.DE', 
                       options=providerTileOptions(noWrap = TRUE)) %>%
      addPolygons( 
        color = 'black',
        fillColor = ~map_palette(value), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        weight=1,
        label = map_text,
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
      setView(lat=0, lng=0 , zoom=1.7) %>%
      addLegend( pal=map_palette, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
    # store palette, text, map object, and data in list
    pop_map_list[[1]] <- map_palette
    pop_map_list[[2]] <- map_text
    pop_map_list[[3]] <- pop_map
    pop_map_list[[4]] <- shp
    return(pop_map_list)
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_mean_leaf <- renderLeaflet({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
     map_text <- pop_map[[1]]
     map_palette <- pop_map[[2]]
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
  output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_population_mean_map", ".png"),
                                    content = function(file) {
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
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_mean_plot <- renderPlotly({
    # get reactive list
    pop_map <- get_pop_map()
    
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # while map (generate from reactive object) is null, plot is null
    if(is.null(pop_map)){
      NULL
    } else {
      shp <- pop_map[[4]]
    }
    # get data from shp and remove NA
    temp <- shp@data
    
    # if data is null or all values are NA, generate empty plot with message
    if(all(is.na(temp$value)) | is.null(temp)){
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
      temp$NAME <- factor(temp$NAME, levels = unique(temp$NAME)[order(temp$value, decreasing = TRUE)])
      # get text for plot
      plot_text <- paste(
        "Country: ", as.character(temp$NAME),"\n", 
        "Value: ", round(temp$value, digits = 3), "\n",
        "Year: ", as.character(temp$year),"\n",
        "Data source :", as.character(temp$data_source), "\n",
        sep="") %>%
        lapply(htmltools::HTML)
      
      # create title and y axis label
      y_axis_text = paste0('Population mean')
      plot_title = paste0('Population mean - ', indicator)
      
      # add highlight functionality, so hovering highlights bar.
      temp <- highlight_key(temp, key=~NAME)
      
      # plotly plot
      print(ggplotly(ggplot(temp, aes(NAME, value, text = plot_text)) +
                       geom_bar(stat = 'identity', aes(fill = value)) +
                       scale_fill_distiller(palette = "Greens", direction = 1) +
                       labs(x='Country',
                            y = y_axis_text,
                            title = plot_title) +
                       hefpi::theme_gdocs() +
                       theme(panel.grid.major.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks = element_blank()),
                     tooltip = 'text')   %>%
              highlight(on='plotly_hover',
                        color = 'darkgreen',
                        opacityDim = 0.6))
    }
  })
}

# -------------------------------------------------------------------------------------
#' @rdname mod_recent_con_ui
#' @export 
#' @import webshot
#' @import shinyalert
#' @import leaflet
#' @importFrom shiny NS tagList 

mod_recent_con_ui <- function(id){
  ns <- NS(id)
  # tagList(
  
  fluidPage(
    fluidRow(
      column(8,
             leafletOutput(
               ns('recent_con_leaf'), height = '650px'),
             ),
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
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image'),
             downloadButton(ns("dl_data"), label = 'Download data'),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info"))
               )
             )
    ),
    br(), br(),
    
    fluidRow(
      column(8,
             plotlyOutput(
               ns('recent_con_plot')
             ))
    )
    
  )
  
}

# Module Server
#' @rdname mod_recent_con_server
#' @import leaflet
#' @import RColorBrewer
#' @import plotly
#' @import htmltools
#' @keywords internal

mod_recent_con_server <- function(input, output, session){
  

    # Observe changes to inputs in order to generate changes to the map
    observeEvent(input$plot_info, {
      # Show a modal when the button is pressed
      shinyalert(title = "Recent value- Concentration Index", 
                 text = "charts display a world map in which countries are color-coded according to the most recent value of an indicator’s concentration index. The concentration index is bounded between -1 and 1. Negative values indicate disproportionate concentration of a variable among the poor, and positive values disproportionate concentration among the rich. For instance, if the variable is “bad” such as infant mortality, a negative value means infant mortality is higher among the poor. The map is complemented by a bar chart that ranks countries by the concentration index. By default, the map and bar chart use an indicator’s latest available concentration index, but users can choose the time period from which this latest concentration index value is chosen.", 
                 type = "info", 
                 closeOnClickOutside = TRUE, 
                 showCancelButton = FALSE, 
                 showConfirmButton = FALSE)
    })
  
  get_con_map <- reactive({
    indicator <- 'BMI, adults'
    plot_years <- c(1982, 2017)
    
    con_map_list <- list()
    plot_years <- input$date_range
    indicator <- input$indicator

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
      summarise(value = first(CI),
                year = year,
                data_source = referenceid_list) 
    
    shp <- world
    # save(shp, file = 'shp.rda')
    shp@data <- shp@data %>% left_join(pd)
    
    # Make color palette
    plot_palette <- colorNumeric(palette = brewer.pal(11, "BrBG"), domain=shp@data$value, na.color="white")
    
    # Make tooltip
    plot_text <- paste(
      "Country: ", as.character(shp@data$NAME),"<br/>", 
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      "Year: ", as.character(shp@data$year),"<br/>",
      "Data source :", as.character(shp@data$data_source), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # get map
    con_map <- leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
      addProviderTiles('OpenStreetMap.DE', options=providerTileOptions(noWrap = TRUE)) %>%
      setView( lat=10, lng=0 , zoom=1.5) %>%
      addPolygons( 
        color = 'black',
        fillColor = ~plot_palette(value), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        weight=1,
        label = plot_text,
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
      ) %>% setView(lat=0, lng=0 , zoom=1.7) %>%
      addLegend( pal=plot_palette, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
    con_map_list[[1]] <- plot_palette
    con_map_list[[2]] <- plot_text
    con_map_list[[3]] <- con_map
    con_map_list[[4]] <- shp
    return(con_map_list)
    
  })
  
  
  
  output$recent_con_leaf <- renderLeaflet({
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      plot_text <- con_map[[1]]
      plot_palette <- con_map[[2]]
      this_map <- con_map[[3]]
      this_map
    }
    
  })
  
  output$dl_data <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # get map
      con_map <- get_con_map()
      this_map <- con_map[[4]]
      if(is.null(this_map)){
        NULL
      } else {
        temp <- this_map@data
        temp <- temp %>% filter(!is.na(value))
        
        write.csv(temp, file)
      }
      
    }
  )
  
  
  output$dl_plot <- downloadHandler(
    filename = paste0( Sys.Date()
                       , "_ci_map"
                       , ".png"
    )   
    
    , content = function(file) {
      con_map <- get_con_map()
      if(is.null(con_map)){
        NULL
      } else {
        # get map
        this_map <- con_map[[3]]
        
        mapview::mapshot( x = this_map,
                          file = file,
                          cliprect = "viewport",
                          selfcontained = FALSE)
      }
      
    })
  
  output$recent_con_plot <- renderPlotly({
    con_map <- get_con_map()
    plot_years <- input$date_range
    indicator <- input$indicator
    if(is.null(con_map)){
      NULL
    } else {
      shp <- con_map[[4]]
    }
    # get data from shp and remove NA
    temp <- shp@data
    # temp <- temp %>% filter(!is.na(value))
    
    if(all(is.na(temp$value)) | is.null(temp)){
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
      # temp$NAME <- factor(temp$NAME, levels = unique(temp$NAME)[order(temp$value, decreasing = TRUE)])
      # get text for plotly 
      con_bar_text <- paste(
        "Country: ", as.character(temp$NAME),"\n", 
        "Value: ", round(temp$value, digits = 3), "\n",
        "Year: ", as.character(temp$year),"\n",
        "Data source :", as.character(temp$data_source), "\n",
        sep="") %>%
        lapply(htmltools::HTML)
      plot_limit <- max(abs(temp$value), na.rm = TRUE) * c(-1, 1)
      
      y_axis_text = paste0('Population mean')
      
      plot_title = paste0('Population mean - ', indicator)
      temp <- highlight_key(temp, key=~NAME)
      # plotly plot
      print(ggplotly(ggplot(temp, aes(NAME, value, text = con_bar_text)) +
                       geom_bar(stat = 'identity', aes(fill = value)) +
                       scale_fill_distiller(palette = "BrBG", limit = plot_limit) +
                       labs(x='Country',
                            y = y_axis_text,
                            title = plot_title) +
                       hefpi::theme_gdocs() +
                       theme(panel.grid.major.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks = element_blank()),
                     tooltip = 'text')   %>%
              highlight(on='plotly_hover',
                        color = 'darkgreen',
                        opacityDim = 0.6))
      
    }
    
  })

}


## To be copied in the UI
# mod_recent_mean_ui("leaf1")
# mod_recent_con_ui("con1")

## To be copied in the server
# callModule(mod_recent_mean_server, 'leaf1')
# callModule(mod_recent_con_server, 'con1')