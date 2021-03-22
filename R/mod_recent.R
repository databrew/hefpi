# Module recent value
#' @title mod_recent.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal

#' @keywords internal
#' @export 
# UI FOR MOST RECENT VALUE MAP
mod_recent_mean_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(8,
             uiOutput(ns('map_title_ui')),
             leafletOutput(
               ns('recent_mean_leaf'), height = 700 ),
      ),
      column(4,
             useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             br(), br(),
             p('Indicator'),
             selectInput(ns('indicator'), 
                         label = NULL,
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary')
      )
    ),
    br(), br(),
    fluidRow(
      column(8,
             plotlyOutput(
               ns('recent_mean_plot'), height = 550
             )),
      column(4,
             p('Choose country to highlight'),
             selectInput(ns('country'), 
                         label = NULL,
                         choices = country_list, selected = 'Brazil'))

             ))
}

# SERVER FOR MOST RECENT VALUE MAP
mod_recent_mean_server <- function(input, output, session){
  
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Most recent value - National mean", 
               text = "This chart displays a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
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
    ind_info <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      select(variable_name, good_or_bad, unit_of_measure)
    variable_name = ind_info$variable_name
    good_or_bad = ind_info$good_or_bad
    unit_of_measure = ind_info$unit_of_measure
    
    # Get the data, subsetted by inputs
    pd <- hefpi::df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == variable_name) %>%
      group_by(ISO3 = iso3c) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(pop),
                indic = indic,
                year = year,
                region_name = region_name,
                survey_list = survey_list,
                data_source = referenceid_list,
                indicator_short_name = indicator_short_name,
                indicator_description = indicator_description,
                unit_of_measure = unit_of_measure) 
    
    # get indicator short name joined to data
    if(nrow(pd)==0 | all(is.na(pd$value))){
      pop_map_list <- NA
    } else {
      # get world map shape files and join to data
      shp <- world
      shp <- rbind(shp, wb_bound)
      
      shp@data <- shp@data %>% left_join(pd)
      shp@data$value[shp@data$NAME == 'Cyprus No Mans Land'] <- 0.2
      shp@data$value[shp@data$NAME == 'Sahrawi Arab Democratic Republic'] <- 0.2
      
      # adjust value and color palette based indicator info
      if(unit_of_measure == '%'){
        shp@data$value <- shp@data$value*100
      } 
      if(good_or_bad == 'Good'){
        map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="#CECECE")
      } else {
        map_palette <- colorNumeric(palette = brewer.pal(9, "Reds"), domain=shp@data$value, na.color="#CECECE")
      }
      # text for map
      map_text <- paste(
        "Indicator: ",  indicator,"<br>",
        "Economy: ", as.character(shp@data$NAME),"<br/>", 
        'Value: ', paste0(round(shp@data$value, digits = 2), ' (',unit_of_measure,')'),  "<br/>",
        "Year: ", as.character(shp@data$year),"<br/>",
        "Data source :", as.character(shp@data$data_source), "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
      pop_map <- leaflet(shp, 
                         options = leafletOptions(minZoom = 1, 
                                                  maxZoom = 10)) %>% 
        addProviderTiles('Esri.WorldShadedRelief') %>%
        addPolygons( 
          # options = pathOptions(pane = "mover"),
          color = 'black',
          fillColor = ~map_palette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = map_text,
          highlightOptions = highlightOptions(
            weight = 1,
            fillColor = 'white',
            fillOpacity = 1,
            color = "white",
            opacity = 1.0,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          labelOptions = labelOptions( 
            noHide = FALSE,
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>% 
        setView(lat=0, lng=0 , zoom=1.7) %>%
        addLegend( pal=map_palette,title = unit_of_measure, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
      # store palette, text, map object, and data in list
      pop_map_list<- list(pop_map, shp, unit_of_measure, good_or_bad, year_title)
    }
    return(pop_map_list)
  })
  
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- renderUI({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        
        h4('')
      } else {
        indicator_name <- input$indicator
        year_title <- pop_map[[5]]
        # HTML(paste(h4(paste0('Most recent value - National mean - ', indicator_name)), '\n',
        #            h5(year_title)))
        fluidPage(
          fluidRow(
            h4(paste0('Most recent value - National mean - ', indicator_name)),
            h5(paste0(year_title))
            
          )
        )
        
      }
    }
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_mean_leaf <- renderLeaflet({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        this_map <- leaflet(options = leafletOptions(minZoom = 1, 
                                                     maxZoom = 10)) %>% 
          addProviderTiles('CartoDB.VoyagerNoLabels') %>%
          setView(lat=0, lng=0 , zoom=1.7) 
        this_map
      } else {
        this_map <- pop_map[[1]]
        
      }
    }
  })
  
  # ---- GET COUNTRY LABELS ---- #
  observeEvent(input$recent_mean_leaf_zoom, {
    the_zoom <- input$recent_mean_leaf_zoom
    print('THE ZOOM LEVEL IS :')
    message('----', the_zoom)
    if(the_zoom <= 4 & the_zoom >= 1){ # BEN, change this to 2 if you want to suppress the continent labels
      leafletProxy('recent_mean_leaf') %>%
        addMapPane("country_labels", zIndex = 410) %>%
        addProviderTiles('CartoDB.PositronOnlyLabels',
                         options = pathOptions(pane = "country_labels"),
                         layerId = 'country_labs')
    } else {
      leafletProxy('recent_mean_leaf') %>%
        removeTiles(layerId = 'country_labs')
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_map <- get_pop_map()
      if(is.null(pop_map)){
        NULL
      } else {
        if(is.na(pop_map)){
          temp <- data_frame()
          write.csv(temp, file)
        } else {
          # get the map data from the second element of the list
          temp <- pop_map[[2]]
          temp <- temp@data
          temp <- temp %>% filter(!is.na(value))
          names(temp) <- tolower(names(temp))
          temp$parameter <- 'Mean'
          temp$level <- 'National'
          temp <- temp %>% select(region_name, name, iso3, year, data_source, survey_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          write.csv(temp, file)
        }
        
      }
    }
  )
  
  # ---- CAPTURE USER ZOOM LEVEL FOR DOWNLOAD ---- #
  user_zoom <- reactive({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      this_map <- pop_map[[1]]
      this_map %>% setView(lng = input$recent_mean_leaf_center$lng,
                           lat = input$recent_mean_leaf_center$lat,
                           zoom = input$recent_mean_leaf_zoom)
    }
  }) 
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("most_recent_value_mean_", Sys.Date(), ".png"),
                                    content = function(file) {
                                      pop_map <- user_zoom()
                                      if(is.null(pop_map)){
                                        NULL
                                      } else {
                                        if(is.na(pop_map)){
                                          
                                          this_map <- leaflet(options = leafletOptions(minZoom = 1, 
                                                                                       maxZoom = 10)) %>% 
                                            addProviderTiles('OpenStreetMap.DE') %>%
                                            setView(lat=0, lng=0 , zoom=1.7) 
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        } else {
                                          this_map <- pop_map
                                          this_map <- this_map %>%
                                            addMapPane("country_labels", zIndex = 410) %>%
                                            addProviderTiles('CartoDB.PositronOnlyLabels',
                                                             options = pathOptions(pane = "country_labels"),
                                                             layerId = 'country_labs')
                                      
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })
  
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_mean_plot <- renderPlotly({
    # get reactive list
    pop_map <- get_pop_map()
    
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    country_name <- input$country

    # while map (generate from reactive object) is null, plot is null
    if(is.null(pop_map)){
      NULL
    } else {
      # create null plot if data is empty
      if(is.na(pop_map)){
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
        # get data 
        shp <- pop_map[[2]]
        unit_of_measure <- pop_map[[3]]
        good_or_bad = pop_map[[4]]
        temp <- shp@data
        temp <- temp %>% filter(!is.na(value))
        
        # get plot
        if(good_or_bad == 'Good'){
          bar_palette = 'Greens'
        } else {
          bar_palette = 'Reds'
        }
        # relevel factor for chart
        temp$NAME <- factor(temp$NAME, levels = unique(temp$NAME)[order(temp$value, decreasing = TRUE)])
        
        # get plot objects
        plot_text <- paste(
          "Indicator: ",  indicator,' (',unit_of_measure,')',"<br>",
          "Economy: ", as.character(temp$NAME),"<br>", 
          'Value: ', round(temp$value, digits = 2),' (',unit_of_measure,')',"<br>",
          "Year: ", as.character(temp$year),"<br>",
          "Data source :", as.character(temp$data_source), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        y_axis_text = paste0(indicator, ' (', unit_of_measure,')')

        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        temp$value_col[temp$NAME == country_name] <- NA
        # add higlight functionality to plot
        temp <- highlight_key(temp, key=~NAME)
        p <- ggplotly(
          ggplotly(ggplot(temp, aes(NAME, value, text = plot_text)) +
                     geom_bar(stat = 'identity', aes(fill = value_col)) +

                     scale_fill_distiller(palette = bar_palette, direction = 1) +
                     labs(x='Country',
                          y = y_axis_text) +
                     hefpi::theme_hefpi(grid_major_x=NA,
                                        x_axis_angle = 90,
                                        x_axis_line = NA,
                                        legend_position = 'none') +
                     theme(axis.text.x = element_blank(),
                           axis.ticks.x = element_blank()),
                   tooltip = 'text'))   
        p <- p %>% 
          config(displayModeBar = F) %>%
          highlight(on='plotly_hover',
                    persistent = FALSE,
                    color = 'white',
                    opacityDim = 0.6) %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
      }
    }
  })
}

# -------------------------------------------------------------------------------------

# UI FOR MOST RECENT VALUE (CONCENTRATION INDEX) MAP
mod_recent_con_ui <- function(id){
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(8,
             uiOutput(ns('map_title_ui')),
             leafletOutput(
               ns('recent_con_leaf'), height = 700),
      ),
      column(4,
             p('Indicator'),
             selectInput(ns('indicator'),
                         label = NULL,
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
             br(),br(),
             fluidPage(
               fluidRow(
                 useShinyalert(),
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             )
      )
    ),
    br(), br(),
    fluidRow(
      column(8,
             plotlyOutput(
               ns('recent_con_plot'), height = 550

             )),
      column(4,
             p('Choose country to highlight'),
             selectInput(ns('country'), 
                         label = NULL, 
                         choices = country_list, 
                         selected = 'Brazil'))

             ))
}

# SERVER FOR MOST RECENT VALUE (CONCENTRATION INDEX) MAP
mod_recent_con_server <- function(input, output, session){
  
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Concentration index – Most recent value", 
               text = 'This chart displays a world map in which countries are color-coded according to the most recent value of an indicator’s concentration index. The concentration index is based on a measure of household wealth and bounded between -1 and 1. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. Negative values of the concentration index indicate disproportionate concentration of an indicator among the poor, and positive values disproportionate concentration among the rich. For instance, a negative value for infant mortality in a country means infant mortality is higher among the poor there. The map is complemented by a bar chart that ranks countries by the concentration index. By default, the map and bar chart use an indicator’s latest available concentration index, but users can choose the time period from which this latest concentration index value is chosen.', 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  

  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  map_data <- reactiveValues(map = NULL)
  get_con_map <- reactive({
    con_map_list <- list()
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # generate data
    ind_info <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      select(variable_name, unit_of_measure)
    variable_name <- ind_info$variable_name
    # hard code CI as unit of measure
    unit_of_measure <- 'CI'
    
    # get subset of data based on inputs
    pd<- hefpi::df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(indic == variable_name) %>%
      group_by(ISO3 = iso3c) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      filter(referenceid_list == first(referenceid_list))%>%
      summarise(value = first(CI),
                indic = indic,
                year = year,
                region_name = region_name,
                survey_list = survey_list,
                data_source = referenceid_list) %>%
      inner_join(indicators, by = c('indic'='variable_name'))
    
    # fill result list with NA if data is null
    if(nrow(pd)==0 | all(is.na(pd$value))){
      con_map_list <- NA
    } else {
      shp <- world
      shp@data <- shp@data %>% left_join(pd)
      
      # generate map
      map_palette <- colorNumeric(palette = brewer.pal(11, "BrBG"), domain=shp@data$value, na.color="#CECECE")
      map_text <- paste(
        "Indicator: ",  indicator,"<br>",
        "Economy: ", as.character(shp@data$NAME),"<br/>", 
        'Value: ', paste0(round(shp@data$value, digits = 2)),  "<br/>",
        "Year: ", as.character(shp@data$year),"<br/>",
        "Data source :", as.character(shp@data$data_source), "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      year_title = paste0('From ',plot_years[1], ' to ', plot_years[2])
      con_map <- leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
        addProviderTiles('Esri.WorldShadedRelief') %>%
        addPolygons(
          color = 'black',
          fillColor = ~map_palette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = map_text,
          highlightOptions = highlightOptions(
            weight = 1,
            fillColor = 'white',
            fillOpacity = 1,
            color = "white",
            opacity = 1.0,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          labelOptions = labelOptions(
            noHide = FALSE,
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>% setView(lat=0, lng=0 , zoom=1.7) %>%
        addLegend(pal=map_palette, title = 'CI', values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" ) 
      con_map_list<- list(con_map, shp, unit_of_measure,year_title)
      map_data$map <- con_map
    }
    return(con_map_list)
  })
  
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- renderUI({
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      if(is.na(con_map)){
        fluidPage(
          fluidRow(
            h4('')
          )
        )
      } else {
        indicator_name <- input$indicator
        year_title <- con_map[[4]]
        fluidPage(
          fluidRow(
            h4(paste0('Most recent value - Concentration index - ', indicator_name)),
            h5(paste0(year_title))
          )
        )
      }
    }
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_con_leaf <- renderLeaflet({
    # Plotly hover capture
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      if(is.na(con_map)){
        this_map <- leaflet(options = leafletOptions(minZoom = 1,
                                                     maxZoom = 10)) %>%
          addProviderTiles('CartoDB.VoyagerNoLabels') %>%
          setView(lat=0, lng=0 , zoom=1.7)
        this_map
      } else {
      this_map <- map_data$map #con_map[[1]]
      this_map 
      
      }
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_ci_", Sys.Date(), ".csv")
    },
    content = function(file) {
      con_map <- get_con_map()
      if(is.null(con_map)){
        NULL
      } else {
        map_dat <- con_map[[2]]
        if(is.na(map_dat)){
          temp <- data_frame()
          write.csv(temp, file)
        } else {
          temp <- map_dat@data
          temp <- temp %>% filter(!is.na(value))
          names(temp) <- tolower(names(temp))
          # subset by  
          temp$parameter <- 'CI'
          temp$level <- 'National'
          temp <- temp %>% select(region_name, name, iso3, year, data_source, survey_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          write.csv(temp, file)
        }
      }
    }
  )
  
  # ---- CAPTURE USER ZOOM LEVEL FOR DOWNLOAD ---- #
  user_zoom_ci <- reactive({
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      this_map <- con_map[[1]]
      this_map %>% setView(lng = input$recent_con_leaf_center$lng,
                           lat = input$recent_con_leaf_center$lat,
                           zoom = input$recent_con_leaf_zoom)
    }
  }) 
  
  # ---- GET COUNTRY LABELS ---- #
  observeEvent(input$recent_con_leaf_zoom, {
    the_zoom <- input$recent_con_leaf_zoom
    print('THE CON ZOOM LEVEL IS :')
    message('----', the_zoom)
    if(the_zoom <= 4 & the_zoom >= 1){ # BEN, change this to 2 if you want to suppress the continent labels
      leafletProxy('recent_con_leaf') %>%
        addMapPane("country_labels_con", zIndex = 410) %>%
        addProviderTiles('CartoDB.PositronOnlyLabels',
                         options = pathOptions(pane = "country_labels_con"),
                         layerId = 'country_labs_con')
    } else {
      leafletProxy('recent_con_leaf') %>%
        removeTiles(layerId = 'country_labs_con')
    }
  })
  
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("most_recent_value_ci_", Sys.Date(), ".png"),
                                    content = function(file) {
                                      con_map <- user_zoom_ci()
                                      if(is.null(con_map)){
                                        NULL
                                      } else {
                                        if(is.na(con_map)){
                                          this_map <- leaflet(options = leafletOptions(minZoom = 1,
                                                                                       maxZoom = 10)) %>%
                                            addProviderTiles('OpenStreetMap.DE') %>%
                                            setView(lat=0, lng=0 , zoom=1.7)
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        } else {
                                          this_map <- con_map
                                          this_map <- this_map %>%
                                            addMapPane("country_labels", zIndex = 410) %>%
                                            addProviderTiles('CartoDB.PositronOnlyLabels',
                                                             options = pathOptions(pane = "country_labels"),
                                                             layerId = 'country_labs')
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_con_plot <- renderPlotly({
    con_map <- get_con_map()
    plot_years <- input$date_range
    indicator <- input$indicator
    country_name <- input$country
    if(grepl('voire', country_name)){
      country_name <- "Cote d'Ivoire"
    }

    if(is.null(con_map)){
      NULL
    } else {
      # create empty plot if data is null
      if(is.na(con_map)){
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
        shp <- con_map[[2]]
        unit_of_measure <- con_map[[3]]
        temp <- shp@data
        temp <- temp %>% filter(!is.na(value))
        
        # generate map
        plot_text <- paste(
          "Indicator: ", indicator,"<br>",
          "Economy: ", as.character(temp$NAME),"<br>", 
          'Value: ', paste0(round(temp$value, digits = 2)),  "<br>",
          "Year: ", as.character(temp$year),"<br>",
          "Data source :", as.character(temp$data_source), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        ordered_names <- temp$NAME[order(temp$value, decreasing = TRUE)]
        temp$NAME <- factor(temp$NAME, levels = ordered_names)
        y_axis_text = paste0('CI - ', indicator)
        plot_title = 'Concentration index - Most recent value'
        plot_limit <- max(abs(temp$value), na.rm = TRUE) * c(-1, 1)
        # create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        temp$value_col[temp$NAME == country_name] <- NA
        
        temp <- highlight_key(temp, key=~NAME)
        p <- ggplotly(ggplot(temp, aes(NAME, value, text = plot_text)) +
                        geom_bar(stat = 'identity', aes(fill = value_col)) +

                        scale_fill_distiller(palette = "BrBG", limit = plot_limit) +
                        labs(x='Country',
                             y = y_axis_text) +
                        hefpi::theme_hefpi(grid_major_x=NA,
                                           x_axis_angle = 90,
                                           x_axis_line = NA,
                                           legend_position = 'none') +
                        theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank()),
                      tooltip = 'text')
        p <- p %>% 
          config(displayModeBar = F) %>%
          highlight(on='plotly_hover',
                    persistent = FALSE,
                    color = 'white',
                    opacityDim = 0.6) %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
    }
  })
}
