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
  # let leaflet know that selections should persist
  options(persistent = TRUE)
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(9,
             uiOutput(ns('map_title_ui')),
             leafletOutput(
               ns('recent_mean_leaf')),
             ),
      column(3,
             useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             br(), br(),
             pickerInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults',
                         options = list(`style` = "btn-primary")),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    ),
    br(), br(),
    fluidRow(
      column(9,
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
    shinyalert(title = "Most recent value - National mean", 
               text = "This chart displays a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
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
      shp@data <- shp@data %>% left_join(pd)
      
      # adjust value and color palette based indicator info
      if(unit_of_measure == '%'){
        shp@data$value <- shp@data$value*100
      } 
      if(good_or_bad == 'Good'){
        map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="#CECECE")
      } else {
        map_palette <- colorNumeric(palette = brewer.pal(9, "Reds"), domain=shp@data$value, na.color="#CECECE")
      }
      
      # Create map
      map_text <- paste(
        "Indicator: ",  indicator,"<br>",
        "Economy: ", as.character(shp@data$NAME),"<br/>", 
        'Value: ', paste0(round(shp@data$value, digits = 2), ' (',unit_of_measure,')'),  "<br/>",
        "Year: ", as.character(shp@data$year),"<br/>",
        "Data source :", as.character(shp@data$data_source), "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
      # carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
      pop_map <- leaflet(shp, 
                         options = leafletOptions(minZoom = 1, 
                                                  maxZoom = 10)) %>% 
        addProviderTiles('Esri.WorldShadedRelief') %>%
        # addTiles(carto) %>%
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
        ) %>% 
        setView(lat=0, lng=0 , zoom=1.7) %>%
        addLegend( pal=map_palette,title = unit_of_measure, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
      # store palette, text, map object, and data in list
      pop_map_list<- list(pop_map, shp, unit_of_measure, good_or_bad, year_title, indicator, plot_years)
    }
    chart_data$plot_data <- pop_map_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
 
  
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- renderUI({
    # get reactive list
    pop_map <- chart_data$plot_data
    if(length(pop_map)==1){
      load('maps_national_mean.RData')
      pop_map <- pop_map_list
      
    }
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        
        h4('')
      } else {
        indicator_name <- pop_map[[6]]
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
    # get reactive list
    pop_map <- chart_data$plot_data
    if(length(pop_map)==1){
      load('maps_national_mean.RData')
      pop_map <- pop_map_list
      
    }
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
       this_map
     }
    }
  })
  
  the_country <- reactiveVal(value = NULL)
  the_shp <- reactiveValues(shp = NULL)
  
  observeEvent(input$recent_mean_leaf_shape_mouseover, {
    # print('Shape mouseover')
    shp_mouse <- input$recent_mean_leaf_shape_mouseover
    lng <- shp_mouse$lng
    lat <- shp_mouse$lat
    the_point <- tibble(lng = lng, lat = lat)
    coordinates(the_point) <- ~lng+lat    
    proj4string(the_point) <- proj4string(world)
    # Get the country
    poly <- over(world, the_point)
    poly_number <- which(!is.na(poly))
    country_row <- world[poly_number,]
    print(country_row@data)
    the_shp$shp <- country_row
    country_name <- country_row@data$NAME
    the_country(country_name)
    message('Mouse-overed country is ', country_name)
    
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      # get reactive list
      pop_map <- chart_data$plot_data
      if(length(pop_map)==1){
        load('maps_national_mean.RData')
        pop_map <- pop_map_list
        
      }
      if(is.null(pop_map)){
        NULL
      } else {
        if(is.na(pop_map)){
          temp <- data_frame()
          write.csv(temp, file)
        } else {
          temp <- pop_map[[2]]
          temp <- temp@data
          temp <- temp %>% filter(!is.na(value))
          names(temp) <- tolower(names(temp))
          # subset by  
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
    # get reactive list
    pop_map <- chart_data$plot_data
    if(length(pop_map)==1){
      load('maps_national_mean.RData')
      pop_map <- pop_map_list
      
    }
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
    # get reactive list
    pop_map <- chart_data$plot_data
    if(length(pop_map)==1){
      load('maps_national_mean.RData')
      pop_map <- pop_map_list
      
    }
    
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
        # get inputs
        plot_years <- pop_map[[7]]
        indicator <- pop_map[[6]]
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
        temp <- highlight_key(temp, key=~NAME)
        p <- ggplotly(
          ggplotly(ggplot(temp, aes(NAME, value, text = plot_text)) +
                     geom_bar(stat = 'identity', aes(fill = value)) +
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
      column(9,
             uiOutput(ns('map_title_ui')),
             leafletOutput(
               ns('recent_con_leaf')),
             ),
      column(3,
             useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             br(), br(),
             pickerInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults',
                         options = list(`style` = "btn-primary")),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    ),
    br(), br(),
    fluidRow(
      column(9,
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
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    con_map_list <- list()
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # generate data
    ind_info <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      select(variable_name, unit_of_measure)
    variable_name <- ind_info$variable_name
    unit_of_measure <- 'CI'
    
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
      # carto <- "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
      con_map <- leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
        addProviderTiles('Esri.WorldShadedRelief') %>%
        # addTiles(carto) %>%
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
      con_map_list<- list(con_map, shp, unit_of_measure,year_title, indicator, plot_years)
    }
    chart_data$plot_data <- con_map_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)

  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- renderUI({
    # get reactive list
    con_map <- chart_data$plot_data
    if(length(con_map)==1){
      load('maps_national_ci.RData')
      con_map <- con_map_list
    }
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
        indicator_name <- con_map[[5]]
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
    mouse_event <- event_data("plotly_click", source = "subset")
    print(mouse_event)
    con_map <- chart_data$plot_data
    if(length(con_map)==1){
      load('maps_national_ci.RData')
      con_map <- con_map_list
    }
    if(is.null(con_map)){
      NULL
    } else {
      # if(is.na(con_map)){
      #   this_map <- leaflet(options = leafletOptions(minZoom = 1, 
      #                                                maxZoom = 10)) %>% 
      #     addProviderTiles('CartoDB.VoyagerNoLabels') %>%
      #     setView(lat=0, lng=0 , zoom=1.7) 
      #   this_map
      # } else {
        this_map <- con_map[[1]]
        this_map
      # }
    }
  })
  
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_ci_", Sys.Date(), ".csv")
    },
    content = function(file) {
      con_map <- chart_data$plot_data
      if(length(con_map)==1){
        load('maps_national_ci.RData')
        con_map <- con_map_list
      }
      if(is.null(con_map)){
        NULL
      } else {
        map_dat <- con_map[[2]]
        
        if(nrow(map_dat@data)==0){
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
    con_map <- chart_data$plot_data
    if(length(con_map)==1){
      load('maps_national_ci.RData')
      con_map <- con_map_list
    }
    if(is.null(con_map)){
      NULL
    } else {
      this_map <- con_map[[1]]
      this_map %>% setView(lng = input$recent_con_leaf_center$lng,
                           lat = input$recent_con_leaf_center$lat,
                           zoom = input$recent_con_leaf_zoom)
    }
  }) 
  
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  # DO THE SAME HERE
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
                                          this_map <- user_zoom_ci()
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_con_plot <- renderPlotly({
    con_map <- chart_data$plot_data
    if(length(con_map)==1){
      load('maps_national_ci.RData')
      con_map <- con_map_list
    }
    plot_years <- con_map[[6]]
    indicator <- con_map[[5]]

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
        plot_title = 'Most recent value - concentration index'
        plot_limit <- max(abs(temp$value), na.rm = TRUE) * c(-1, 1)
        temp <- highlight_key(temp, key=~NAME)
        p <- ggplotly(ggplot(temp, aes(NAME, value, text = plot_text)) +
                        geom_bar(stat = 'identity', aes(fill = value)) +
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


## To be copied in the UI
# mod_recent_mean_ui("leaf1")
# mod_recent_con_ui("con1")

## To be copied in the server
# callModule(mod_recent_mean_server, 'leaf1')
# callModule(mod_recent_con_server, 'con1')