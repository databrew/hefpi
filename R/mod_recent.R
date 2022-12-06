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
  ns <- shiny::NS(id)
  # tagList(
  shiny::fluidPage(
    shiny::fluidRow(
      
      shiny::column(8,
                    shiny::uiOutput(ns('map_title_ui')),
                    leaflet::leafletOutput(
               ns('recent_mean_leaf'), height = 700 ),
      ),
      shiny::column(4,
             #useShinyalert(),
             shiny::actionButton(ns("plot_info"), label = "Plot Info"),
             # actionButton(ns("plot_info"), label = "Plot Info"),
             # actionButton(ns('share_chart'), 'Share chart'),
             # br(), br(),
             p('Indicator'),
             shiny::selectInput(ns('indicator'), 
                         label = NULL,
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults (%)'),
             p('Date range'),
             shiny::sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary')
             # ,
             # br(), br(),
             # shiny::actionButton(ns("plot_info"), label = "Plot Info")
             # ,
             # actionButton(ns('share_chart'), 'Share chart')
      )
    ),
    br(), br(),
    shiny::fluidRow(
      shiny::column(8,
             plotly::plotlyOutput(
               ns('recent_mean_plot'), height = 550
             )),
      shiny::column(4,
             p('Choose country to highlight'),
             shiny::selectInput(ns('country'), 
                         label = NULL,
                         choices = country_list, selected = 'Brazil'))

             ))
}

# SERVER FOR MOST RECENT VALUE MAP
mod_recent_mean_server <- function(input, output, session){
  
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  shiny::observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(title = "Most recent value - National mean", 
               text = "This chart displays a world map in which countries are color-coded according to the most recent value of an indicator’s population level mean. To give users a better idea of a country’s relative positioning, the map is complemented by a bar chart that ranks countries by indicator value. By default, the map and bar chart use the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  get_pop_map <- shiny::reactive({
    
    # create list to store results from reactive object
    pop_map_list <- list()
    
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # save(plot_years, indicator, file = 'temp_inputs.rda')
    # Get the variable from indicator input
    ind_info <- indicators %>%
      dplyr::filter(indicator_short_name == indicator) %>%
      dplyr::select(variable_name, good_or_bad, unit_of_measure)
    variable_name = ind_info$variable_name
    good_or_bad = ind_info$good_or_bad
    unit_of_measure = ind_info$unit_of_measure
    
    # Get the data, subsetted by inputs
    pd <- hefpi::hefpi_df %>%
      dplyr::filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      dplyr::filter(indic == variable_name) %>%
      dplyr::group_by(ISO3 = iso3c) %>%
      dplyr::filter(year == max(year, na.rm = TRUE)) %>%
      dplyr::filter(referenceid_list == first(referenceid_list)) %>%
      dplyr::summarise(value = first(pop),
                indic = indic,
                year = year,
                region_name = region_name,
                #survey_list = survey_list,
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
      # shp <- rbind(shp, wb_bound)
      
      shp@data <- shp@data %>% left_join(pd)
      # shp@data$value[shp@data$NAME == 'Cyprus No Mans Land'] <- 0.2
      # shp@data$value[shp@data$NAME == 'Sahrawi Arab Democratic Republic'] <- 0.2
      
      # adjust value and color palette based indicator info
      if(unit_of_measure == '%'){
        shp@data$value <- shp@data$value*100
      } 
      if(good_or_bad == 'Good'){
        map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="#CECECE")
        map_palette_rev <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="#CECECE", reverse = TRUE)
      } else {
        map_palette <- colorNumeric(palette = brewer.pal(9, "Reds"), domain=shp@data$value, na.color="#CECECE", reverse = TRUE)
        map_palette_rev <- colorNumeric(palette = brewer.pal(9, "Reds"), domain=shp@data$value, na.color="#CECECE", reverse = TRUE)
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
      pop_map <- leaflet::leaflet(shp, 
                         options = leaflet::leafletOptions(minZoom = 1, 
                                                  maxZoom = 10)) %>% 
        leaflet::addProviderTiles('Esri.WorldShadedRelief') %>%
        leaflet::addPolygons( 
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
          labelOptions = leaflet::labelOptions( 
            noHide = FALSE,
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>% 
        leaflet::setView(lat=0, lng=0 , zoom=1.7) %>%
        leaflet::addLegend( pal=map_palette_rev,
                    title = unit_of_measure, 
                    values=~value, 
                   opacity=0.9, 
                   position = "bottomleft", 
                   na.label = "NA",
                   labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                   )
      # store palette, text, map object, and data in list
      pop_map_list<- list(pop_map, shp, unit_of_measure, good_or_bad, year_title)
    }
    return(pop_map_list)
  })
  
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- shiny::renderUI({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(any(is.na(pop_map))){
        
        h4('')
      } else {
        indicator_name <- input$indicator
        year_title <- pop_map[[5]]
        # HTML(paste(h4(paste0('Most recent value - National mean - ', indicator_name)), '\n',
        #            h5(year_title)))
        shiny::fluidPage(
          shiny::fluidRow(
            # h4(paste0('Most recent value - National mean - ', indicator_name)),
            HTML(stringr::str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Most recent value </div> 
                           <div class="chart-label"> {indicator_name} </div>
                          </div>
                          ')),
            h5(paste0(year_title))
            
          )
        )
        
      }
    }
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_mean_leaf <- leaflet::renderLeaflet({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(any(is.na(pop_map))){
        this_map <- leaflet::leaflet(options = leafletOptions(minZoom = 1, 
                                                     maxZoom = 10)) %>% 
          leaflet::addProviderTiles('CartoDB.VoyagerNoLabels') %>%
          leaflet::setView(lat=0, lng=0 , zoom=1.7) 
        this_map
      } else {
        this_map <- pop_map[[1]]
        this_map
      }
    }
  })
  
  # ---- GET COUNTRY LABELS ---- #
  shiny::observeEvent(input$recent_mean_leaf_zoom, {
    the_zoom <- input$recent_mean_leaf_zoom
    print('THE ZOOM LEVEL IS :')
    message('----', the_zoom)
    if(the_zoom <= 4 & the_zoom >= 1){ # change this to 2 if you want to suppress the continent labels
      leaflet::leafletProxy('recent_mean_leaf') %>%
        leaflet::addMapPane("country_labels", zIndex = 410) %>%
        leaflet::addProviderTiles('CartoDB.PositronOnlyLabels',
                         options = pathOptions(pane = "country_labels"),
                         layerId = 'country_labs')
    } else {
      leaflet::leafletProxy('recent_mean_leaf') %>%
        leaflet::removeTiles(layerId = 'country_labs')
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- shiny::downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_map <- get_pop_map()
      if(is.null(pop_map)){
        NULL
      } else {
        if(any(is.na(pop_map))){
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
          temp <- temp %>% dplyr::select(region_name, name, iso3, year, data_source, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          # add stampe 
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2022'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
          write.csv(temp, file)
        }
        
      }
    }
  )
  
  # ---- CAPTURE USER ZOOM LEVEL FOR DOWNLOAD ---- #
  user_zoom <- shiny::reactive({
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
  output$dl_plot <- shiny::downloadHandler(
                                    filename = "most_recent_value_mean_map.png",
                                    content = function(file) {
                                      pop_map <- user_zoom()
                                      if(is.null(pop_map)){
                                        NULL
                                      } else {
                                        if(any(is.na(pop_map))){
                                          
                                          this_map <- leaflet::leaflet(options = leafletOptions(minZoom = 1, 
                                                                                       maxZoom = 10)) %>% 
                                            leaflet::addProviderTiles('OpenStreetMap.DE') %>%
                                            leaflet::setView(lat=0, lng=0 , zoom=1.7) 
                                            mapview::mapshot(x = å,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        } else {
                                          this_map <- pop_map
                                          this_map <- this_map %>%
                                            leaflet::addMapPane("country_labels", zIndex = 410) %>%
                                            leaflet::addProviderTiles('CartoDB.PositronOnlyLabels',
                                                             options = pathOptions(pane = "country_labels"),
                                                             layerId = 'country_labs') %>%
                                            leaflet::addTiles(urlTemplate = "", attribution = 'HEFPI database, The World Bank, 2022') 
                                      
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_mean_plot <- plotly::renderPlotly({
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
      if(any(is.na(pop_map))){
        empty_plot <- function(title = NULL){
          p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            plotly::layout(
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
        y_axis_text = paste0(indicator)

        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        temp$value_col[temp$NAME == country_name] <- NA
        # add higlight functionality to plot
        temp <- plotly::highlight_key(temp, key=~NAME)
        p <- plotly::ggplotly(
          plotly::ggplotly(ggplot2::ggplot(temp, ggplot2::aes(NAME, value, text = plot_text)) +
                             ggplot2::geom_bar(stat = 'identity', ggplot2::aes(fill = value_col)) +

                             ggplot2::scale_fill_distiller(palette = bar_palette, direction = 1) +
                             ggplot2::labs(x='Country',
                          y = y_axis_text) +
                     hefpi::theme_hefpi(grid_major_x=NA,
                                        x_axis_angle = 90,
                                        x_axis_line = NA,
                                        legend_position = 'none') +
                       ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                           axis.ticks.x = ggplot2::element_blank()),
                   tooltip = 'text'))   
        p <- p %>% 
          plotly::config(displayModeBar = T) %>%
          plotly::highlight(on='plotly_hover',
                    persistent = FALSE,
                    color = 'white',
                    opacityDim = 0.6) %>%
          plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
      }
    }
  })
}

# -------------------------------------------------------------------------------------

# UI FOR MOST RECENT VALUE (CONCENTRATION INDEX) MAP
mod_recent_con_ui <- function(id){
  ns <- shiny::NS(id)
  # tagList(
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(8,
                    shiny::uiOutput(ns('map_title_ui')),
             leaflet::leafletOutput(
               ns('recent_con_leaf'), height = 700),
      ),
      shiny::column(4,
             #useShinyalert(),
             shiny::actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'),
             # actionButton(ns('share_chart'), 'Share chart'),
             p('Indicator'),
             shiny::selectInput(ns('indicator'),
                         label = NULL,
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults (%)'),
             p('Date range'),
             shiny::sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
             br(),br()
      )
    ),
    br(), br(),
    shiny::fluidRow(
      shiny::column(8,
             plotly::plotlyOutput(
               ns('recent_con_plot'), height = 550

             )),
      shiny::column(4,
             p('Choose country to highlight'),
             shiny::selectInput(ns('country'), 
                         label = NULL, 
                         choices = country_list, 
                         selected = 'Brazil'))

             ))
}

# SERVER FOR MOST RECENT VALUE (CONCENTRATION INDEX) MAP
mod_recent_con_server <- function(input, output, session){
  
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  shiny::observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(title = "Concentration index – Most recent value", 
               text = 'This chart displays a world map in which countries are color-coded according to the most recent value of an indicator’s concentration index. The concentration index is based on a measure of household wealth and bounded between -1 and 1. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. Negative values of the concentration index indicate disproportionate concentration of an indicator among the poor, and positive values disproportionate concentration among the rich. For instance, a negative value for infant mortality in a country means infant mortality is higher among the poor there. The map is complemented by a bar chart that ranks countries by the concentration index. By default, the map and bar chart use an indicator’s latest available concentration index, but users can choose the time period from which this latest concentration index value is chosen.', 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  

  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  map_data <- shiny::reactiveValues(map = NULL)
  get_con_map <- shiny::reactive({
    con_map_list <- list()
    plot_years <- input$date_range
    indicator <- input$indicator
    
    # generate data
    ind_info <- indicators %>%
      dplyr::filter(indicator_short_name == indicator) %>%
      dplyr::select(variable_name, unit_of_measure)
    variable_name <- ind_info$variable_name
    # hard code CI as unit of measure
    unit_of_measure <- 'CI'
    
    # get subset of data based on inputs
    pd<- hefpi::hefpi_df %>%
      dplyr::filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      dplyr::filter(indic == variable_name) %>%
      dplyr::group_by(ISO3 = iso3c) %>%
      dplyr::filter(year == max(year, na.rm = TRUE)) %>%
      dplyr::filter(referenceid_list == first(referenceid_list))%>%
      dplyr::summarise(value = first(CI),
                indic = indic,
                year = year,
                region_name = region_name,
                #survey_list = survey_list,
                data_source = referenceid_list) %>%
      dplyr::inner_join(indicators, by = c('indic'='variable_name'))
    
    # fill result list with NA if data is null
    if(nrow(pd)==0 | all(is.na(pd$value))){
      con_map_list <- NA
    } else {
      shp <- world
      shp@data <- shp@data %>% left_join(pd)
      
      # generate map
      map_palette <- leaflet::colorNumeric(palette = brewer.pal(11, "BrBG"), domain=shp@data$value, na.color="#CECECE")
      map_palette_rev <- leaflet::colorNumeric(palette = brewer.pal(11, "BrBG"), domain=shp@data$value, na.color="#CECECE", reverse = TRUE)
      
      map_text <- paste(
        "Indicator: ",  indicator,"<br>",
        "Economy: ", as.character(shp@data$NAME),"<br/>", 
        'Value: ', paste0(round(shp@data$value, digits = 2)),  "<br/>",
        "Year: ", as.character(shp@data$year),"<br/>",
        "Data source :", as.character(shp@data$data_source), "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
      year_title = paste0('From ',plot_years[1], ' to ', plot_years[2])
      con_map <- leaflet::leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
        leaflet::addProviderTiles('Esri.WorldShadedRelief') %>%
        leaflet::addPolygons(
          color = 'black',
          fillColor = ~map_palette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = map_text,
          highlightOptions = leaflet::highlightOptions(
            weight = 1,
            fillColor = 'white',
            fillOpacity = 1,
            color = "white",
            opacity = 1.0,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          labelOptions = leaflet::labelOptions(
            noHide = FALSE,
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>% leaflet::setView(lat=0, lng=0 , zoom=1.7) %>%
        leaflet::addLegend(pal=map_palette_rev, 
                  title = 'CI', 
                  values=~value, 
                  opacity=0.9, 
                  position = "bottomleft", 
                  na.label = "NA",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                  ) 
      con_map_list<- list(con_map, shp, unit_of_measure,year_title)
      map_data$map <- con_map
    }
    return(con_map_list)
  })
  
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- shiny::renderUI({
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      if(any(is.na(con_map))){
        shiny::fluidPage(
          shiny::fluidRow(
            h4('')
          )
        )
      } else {
        indicator_name <- input$indicator
        year_title <- con_map[[4]]
        shiny::fluidPage(
          shiny::fluidRow(
            # h4(paste0('Most recent value - Concentration index - ', indicator_name)),
            HTML(stringr::str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Concentration index </div>
                           <div class="chart-label"> Most recent value </div> 
                           <div class="chart-label"> {indicator_name} </div>
                          </div>
                          ')),
            h5(paste0(year_title))
          )
        )
      }
    }
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_con_leaf <- leaflet::renderLeaflet({
    # Plotly hover capture
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      if(any(is.na(con_map))){
        this_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1,
                                                     maxZoom = 10)) %>%
          leaflet::addProviderTiles('CartoDB.VoyagerNoLabels') %>%
          leaflet::setView(lat=0, lng=0 , zoom=1.7)
        this_map
      } else {
      this_map <- map_data$map #con_map[[1]]
      this_map 
      
      }
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- shiny::downloadHandler(
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
          temp <- temp %>% dplyr::select(region_name, name, iso3, year, data_source, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          # add stampe 
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2022'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Survey_name <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
          write.csv(temp, file)
        }
      }
    }
  )
  
  # ---- CAPTURE USER ZOOM LEVEL FOR DOWNLOAD ---- #
  user_zoom_ci <- shiny::reactive({
    con_map <- get_con_map()
    if(is.null(con_map)){
      NULL
    } else {
      this_map <- con_map[[1]]
      this_map %>% leaflet::setView(lng = input$recent_con_leaf_center$lng,
                           lat = input$recent_con_leaf_center$lat,
                           zoom = input$recent_con_leaf_zoom)
    }
  }) 
  
  # ---- GET COUNTRY LABELS ---- #
  shiny::observeEvent(input$recent_con_leaf_zoom, {
    the_zoom <- input$recent_con_leaf_zoom
    print('THE CON ZOOM LEVEL IS :')
    message('----', the_zoom)
    if(the_zoom <= 4 & the_zoom >= 1){ # BEN, change this to 2 if you want to suppress the continent labels
      leaflet::leafletProxy('recent_con_leaf') %>%
        leaflet::addMapPane("country_labels_con", zIndex = 410) %>%
        leaflet::addProviderTiles('CartoDB.PositronOnlyLabels',
                         options = leaflet::pathOptions(pane = "country_labels_con"),
                         layerId = 'country_labs_con')
    } else {
      leaflet::leafletProxy('recent_con_leaf') %>%
        leaflet::removeTiles(layerId = 'country_labs_con')
    }
  })
  
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- shiny::downloadHandler(filename = paste0("most_recent_value_ci_", Sys.Date(), ".png"),
                                    content = function(file) {
                                      con_map <- user_zoom_ci()
                                      if(is.null(con_map)){
                                        NULL
                                      } else {
                                        if(any(is.na(con_map))){
                                          this_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1,
                                                                                       maxZoom = 10)) %>%
                                            leaflet::addProviderTiles('OpenStreetMap.DE') %>%
                                            leaflet::setView(lat=0, lng=0 , zoom=1.7)
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        } else {
                                          this_map <- con_map
                                          # save(this_map, file = 'this_map.rda')
                                          # HERE need ot add image (inst/app/www/wb_stamp.png) over map

                                          this_map <- this_map %>%
                                            leaflet::addMapPane("country_labels", zIndex = 410) %>%
                                            leaflet::addProviderTiles('CartoDB.PositronOnlyLabels',
                                                             options = pathOptions(pane = "country_labels"),
                                                             layerId = 'country_labs') %>%
                                            leaflet::addTiles(urlTemplate = "", attribution = 'HEFPI database, The World Bank, 2022') 
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_con_plot <- plotly::renderPlotly({
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
      if(any(is.na(con_map))){
        empty_plot <- function(title = NULL){
          p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            plotly::layout(
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
        
        # remove unit of measure from indicator since its CI
        y_axis_text = paste0('CI - ', unlist(lapply(strsplit(indicator, '(', fixed = T), function(x) x[1])))
        plot_title = 'Concentration index - Most recent value'
        plot_limit <- max(abs(temp$value), na.rm = TRUE) * c(-1, 1)
        # create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        temp$value_col[temp$NAME == country_name] <- NA
        
        temp <- plotly::highlight_key(temp, key=~NAME)
        p <- plotly::ggplotly(ggplot2::ggplot(temp, ggplot2::aes(NAME, value, text = plot_text)) +
                                ggplot2::geom_bar(stat = 'identity', ggplot2::aes(fill = value_col)) +

                                ggplot2::scale_fill_distiller(palette = "BrBG", limit = plot_limit) +
                                ggplot2::labs(x='Country',
                             y = y_axis_text) +
                        hefpi::theme_hefpi(grid_major_x=NA,
                                           x_axis_angle = 90,
                                           x_axis_line = NA,
                                           legend_position = 'none') +
                          ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                              axis.ticks.x = ggplot2::element_blank()),
                      tooltip = 'text')
        p <- p %>% 
          plotly::config(displayModeBar = F) %>%
          plotly::highlight(on='plotly_hover',
                    persistent = FALSE,
                    color = 'white',
                    opacityDim = 0.6) %>%
          plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
    }
  })
}
