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
  fluidPage(
    fluidRow(
      column(8,
             uiOutput(ns('map_title_ui')),
             leafletOutput(
               ns('recent_mean_sub_leaf')),
             ),
      column(4,
             pickerInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(sub_national$indicator_short_name)),
                         selected = sort(unique(sub_national$indicator_short_name))[1],
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[[1]],
                         options = list(`style` = "btn-primary")),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
             br(),br(),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
    ),
    br(), br(),
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
    shinyalert(title = "Most recent value - Subnational mean", 
               text = "This chart displays maps in which countries’ subnational regions are color-coded according to the most recent value of a subnational region’s indicator mean. By default, the map uses the latest available HEFPI data point, but users can choose the time period from which this latest data point is chosen.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- REDNER UI OUTPUT ---- #
  output$ui_outputs <- renderUI({
    # get inputs
    indicator <- input$indicator
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # get data
    pd <- hefpi::sub_national[sub_national$region_code == region_code,]
    pd <- pd %>% 
      filter(indicator_short_name == indicator) %>%
      group_by(ISO3 = iso3c, country,gaul_code) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      # filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(value),
                indic = indic,
                year = year,
                region_name = region,
                survey_list = survey) 
    # get country_names 
    country_names <- sort(unique(pd$country))
    # get ui inputs
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = country_names,
                    selected = country_names,
                    options = list( `actions-box`=TRUE,
                                    `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} Countries",
                                    `style` = "btn-primary"),
                    multiple = TRUE),
        sliderInput(inputId = session$ns('date_range'),
                    label = 'Date range',
                    min = 1982,
                    max = 2017,
                    value = c(1982, 2017),
                    step = 1,
                    sep = '')
      )
    )
  })
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  get_pop_map <- reactive({
    # get list to store map data
    pop_map_list <- list()
    # get input 
    plot_years <- input$date_range
    indicator <- input$indicator
    region  <- input$region
    country_names <- input$country
    if(is.null(plot_years)){
      NULL
    } else {
      # get region code
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      # Get the variable from indicator input
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(good_or_bad, unit_of_measure)
      # variable_name = ind_info$variable_name
      good_or_bad = ind_info$good_or_bad
      unit_of_measure = ind_info$unit_of_measure
      # Get the data to be plotted
      temp <- hefpi::sub_national[sub_national$region_code == region_code,]
      pd <- temp %>% filter(year >= min(plot_years),
                            year <= max(plot_years)) %>%
        filter(indicator_short_name == indicator) %>%
        filter(country %in% country_names) %>%
        group_by(ISO3 = iso3c, country,gaul_code) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        # filter(referenceid_list == first(referenceid_list)) %>%
        summarise(value = first(value),
                  indic = indic,
                  year = year,
                  region_name = region,
                  survey_list = survey,
                  country = country,
                  iso3c = iso3c,
                  indicator_name = indicator_name,
                  indicator_short_name = indicator_short_name,
                  indicator_description = indicator_description,
                  unit_of_measure) 
      # get indicator short name joined to data
      if(nrow(pd)==0 | all(is.na(pd$value))){
        pop_map_list <- NA
      } else {
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
        # condition on unit of measure
        if(unit_of_measure == '%'){
          shp@data$value<- shp@data$value*100
        } 
        
        if(good_or_bad == 'Good'){
          # Make color palette
          map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="white")
        } else {
          # Make color palette
          map_palette <- colorNumeric(palette = brewer.pal(9, "Reds"), domain=shp@data$value, na.color="white")
        }
        year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
        # Make tooltip
        map_text <- paste(
          "Indicator: ",  indicator,"<br>",
          "Economy: ", as.character(shp@data$ADM1_NAME),"<br/>", 
          "Value: ", paste0(round(shp@data$value, digits = 2), ' (',unit_of_measure,')'), "<br/>",
          "Year: ", as.character(shp@data$year),"<br/>",
          sep="") %>%
          lapply(htmltools::HTML)
        pop_map <- leaflet(shp, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
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
          # setView(lat=0, lng=0 , zoom=1.7) %>%
          setView(lat=centroid$y, lng=centroid$x , zoom=3) %>%
          addLegend(pal=map_palette, title= unit_of_measure, values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
        # store palette, text, map object, and data
        pop_map_list[[1]] <- map_palette
        pop_map_list[[2]] <- map_text
        pop_map_list[[3]] <- pop_map
        pop_map_list[[4]] <- shp
        pop_map_list[[5]] <- good_or_bad
        pop_map_list[[6]] <- unit_of_measure
        pop_map_list[[7]] <- year_title
      }
      return(pop_map_list)
    }
  })
  # ---- RENDER MAP TITLE ---- #
  output$map_title_ui <- renderUI({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        
       fluidPage(
         fluidRow(
           h4('')
         )
       )
      } else {
        indicator_name <- input$indicator
        year_title <- pop_map[[7]]
        # HTML(paste(h4(paste0('Most recent value - National mean - ', indicator_name)), '\n',
        #            h5(year_title)))
        fluidPage(
          fluidRow(
            h4(paste0('Most recent value - Subational mean - ', indicator_name)),
            h5(paste0(year_title))
            
          )
        )
      }
    }
  })
  
  # ---- RENDER MAP FROM REACTIVE LIST ---- #
  output$recent_mean_sub_leaf <- renderLeaflet({
    pop_map <- get_pop_map()
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        this_map <- leaflet(options = leafletOptions(minZoom = 1, 
                                                     maxZoom = 10)) %>% 
          addProviderTiles('OpenStreetMap.DE') %>%
          setView(lat=0, lng=0 , zoom=1.7) 
        this_map
      } else {
        this_map <- pop_map[[3]]
        this_map
      }
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_regional_", Sys.Date(), ".csv")
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
          this_map <- pop_map[[4]]
          temp <- this_map@data
          temp <- temp %>% filter(!is.na(value))
          names(temp) <- tolower(names(temp))
          temp$parameter <- 'Mean'
          temp$level <- 'Subnational'
          temp <- temp %>% select(region_name, country,adm1_name, iso3, year,  survey_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Subregion','Country_iso3', 'Year', 'Survey_name', 
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
      this_map <- pop_map[[3]]
      this_map %>% setView(lng = input$recent_mean_sub_leaf_center$lng,
                           lat = input$recent_mean_sub_leaf_center$lat,
                           zoom = input$recent_mean_sub_leaf_zoom)
    }
  }) 
  
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  # DO THE SAME HERE
  output$dl_plot <- downloadHandler(filename = paste0("most_recent_value_mean_regional_", Sys.Date(), ".png"),
                                    content = function(file) {
                                      pop_map <- user_zoom()
                                      if(is.null(pop_map)){
                                        NULL
                                      } else {
                                        
                                        if(is.na(pop_map)){
                                          carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
                                          this_map <- leaflet(options = leafletOptions(minZoom = 1, 
                                                                                       maxZoom = 10)) %>% 
                                            addProviderTiles('CartoDB.VoyagerNoLabels') %>%
                                            setView(lat=0, lng=0 , zoom=1.7) 
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        } else {
                                          # get map
                                          this_map <- user_zoom()
                                          mapview::mapshot( x = this_map,
                                                            file = file,
                                                            cliprect = "viewport",
                                                            selfcontained = FALSE)
                                        }
                                      }
                                    })

}


## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')
