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
               ns('recent_mean_sub_leaf'), height  = '1000px'),
      ),
      column(4,
             pickerInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(sub_national$indicator_short_name)),
                         selected = sort(unique(sub_national$indicator_short_name))[1],
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[[2]],
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
    # indicator = sort(unique(sub_national$indicator_short_name))[1]
    # region = as.character(region_list$region)[[3]]
    indicator <- input$indicator
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # get data
    # TEMPORARILY COMMENT OUT CODE FOR FAKE DATA BELOW
    pd <- hefpi::df[df$regioncode == region_code,]
    pd <- pd %>%
      filter(indicator_short_name == indicator) %>%
      group_by(ISO3 = iso3c, country) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      # filter(referenceid_list == first(referenceid_list)) %>%
      summarise(value = first(pop),
                indic = indic,
                year = year,
                region_name = region)
    pd_country_names <- sort(unique(pd$country))
    # pd <- hefpi::sub_national_data[sub_national_data$region_code == region_code,]
    # pd <- pd %>% 
    #   filter(indicator_short_name == indicator) %>%
    #   group_by(ISO3 = iso3c, country,gaul_code) %>%
    #   filter(year == max(year, na.rm = TRUE)) %>%
    #   # filter(referenceid_list == first(referenceid_list)) %>%
    #   summarise(value = first(value),
    #             indic = indic,
    #             year = year,
    #             region_name = region,
    #             survey_list = survey) 
    # get country_names 
    # TEMPORARILY COMMENTED OUT FOR FAKE DATA BELOW
    shp <- hefpi::sub_national_shp
    country_names <- as.character(sort(unique(shp@data$country_name)))
    country_names <- intersect(pd_country_names, country_names)
    # country_names <- sort(unique(pd$country))

    # get ui inputs
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = country_names,
                    selected = country_names[1],
                    options = list(`style` = "btn-primary")),
        sliderInput(inputId = session$ns('date_range'),
                    label = 'Date range',
                    min = 1982,
                    max = 2018,
                    value = c(1982, 2018),
                    step = 1,
                    sep = '')
      )
    )
  })
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  get_pop_map <- reactive({
    # get list to store map data
    # indicator = sort(unique(sub_national$indicator_short_name))[1]
    # region = as.character(region_list$region)[[7]]
    # plot_years = c(1982, 2018)
    pop_map_list <- list()
    # get input 
    plot_years <- input$date_range
    indicator <- input$indicator
    region  <- input$region
    country_names <- input$country
    if(is.null(plot_years) | is.null(country_names)){
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
      temp <- hefpi::sub_national_data[sub_national_data$region_code == region_code,]
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

      #TEMORARILY ADD CONDITION FOR FAKE DATA
      if(nrow(pd)==0){
        shp <- hefpi::sub_national_shp
        # GET REGIONS FROM SHP DATA
        pd <- shp@data[shp@data$country_name==country_names,]
        pd <- pd %>% filter(survey_type == 'DHS')
        pd <- pd[!is.na(pd$reg_name),]
        pd$counts <- as.numeric(pd$reg_code)
        pd$value <- pd$counts/max(pd$counts)
        pd$counts <- NULL
        pd$country <- country_names
        names(pd)[2] <- 'gaul_code'
        pd$indic <- indicator
        pd$good_or_bad <- good_or_bad
        pd$unit_of_measure <- unit_of_measure
        pd$reg_name <- NULL
        # MAKE FAKE PD DATA FROM INPUT
        shp@data <- shp@data %>% dplyr::left_join(pd, by=c('reg_code'='gaul_code', 'country_name'='country'))
        dup_rows <- which(!is.na(shp@data$value))
        shp <- shp[dup_rows,]
      } else {
        # get shape files
        shp <- hefpi::sub_national_shp
        # Define centroid
        # rgeos::gCentroid(shp)@coords
        # joine with data
        shp@data <- shp@data %>% dplyr::left_join(pd, by=c('reg_code'='gaul_code', 'country_name'='country'))
      }
     

      # %>%
      #   filter(country_name == country_names) %>% filter(complete.cases(.)) %>%
      #   distinct()
      # %>%
      # HERE the issue is new data all the countries show up when afghanistan is chosen. maybe something in raw_data.R. 
      # group_by(reg_name) %>% mutate(value = mean(value))
      # remove polygons associated with NA - keeps only that region
      na_rows <- which(!is.na(shp@data$indic))
      shp <- shp[na_rows,]
      shp@data$reg_name <- as.character(shp@data$reg_name)
      # get indicator short name joined to data
      if(nrow(pd)==0 | nrow(shp@data)==0| all(is.na(pd$value))){
        pop_map_list <- NA
      } else {
        # get default zoom and lat long parameters
        mp <- hefpi::sn_map_params
        mp$country_name <- Hmisc::capitalize(as.character(mp$country_name))
        mp <- mp %>% filter(tolower(country_name)==tolower(country_names))
        
        # centroi
        # centroid <- coordinates(shp)
        # centroid <- data.frame(centroid)
        # names(centroid) <- c('x', 'y')
        # centroid <- centroid %>%
        #   summarise(x = mean(x, na.rm = TRUE),
        #             y = mean(y, na.rm = TRUE))

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
          "Economy: ", as.character(shp@data$reg_name),"<br/>", 
          "Value: ", paste0(round(shp@data$value, digits = 2), ' (',unit_of_measure,')'), "<br/>",
          "Year: ", as.character(shp@data$year),"<br/>",
          sep="") %>%
          lapply(htmltools::HTML)
        
        # put this back in later
        # pop_map <- leaflet(shp, options = leafletOptions(zoomControl = FALSE, minZoom = mp$the_zoom, maxZoom = 10)) 
        pop_map <- leaflet(shp, options = leafletOptions(zoomControl = FALSE, minZoom = mp$the_zoom, maxZoom = 10)) %>% 
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
          # setView(lat=mp$lat, lng=mp$lon , zoom=mp$the_zoom) %>%

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
  
 
  observeEvent(input$recent_mean_sub_leaf_center$lng, {
    pop_map <- get_pop_map()
    the_zoom <- input$recent_mean_sub_leaf_zoom
    the_lat <- input$recent_mean_sub_leaf_center$lat
    the_lon <- input$recent_mean_sub_leaf_center$lng
    
    
    print('THE ZOOM LEVEL IS :')
    message('###   ', the_zoom)
    print('THE LAT IS :')
    message('###  ', the_lat)
    print('THE LON IS :')
    message('###  ', the_lon)
    
    # JOE HERE
    
   
    
    if(is.null(pop_map)){
      NULL
    } else {
      if(is.na(pop_map)){
        NULL
      } else {
        shp <- pop_map[[4]]
        # loop through and store region name and centroid, to be joined back later
        region_names <- sort(unique(shp@data$reg_name))
        region_holder <- list()
        for(i in 1:length(region_names)){
          this_name <- region_names[i]
          temp_region <- shp[which(shp@data$reg_name == this_name),]
          temp_centroid <- rgeos::gCentroid(temp_region)@coords
          temp_data <- tibble(label = this_name, x= temp_centroid[1], y=temp_centroid[2])
          region_holder[[i]] <- temp_data
        }
        center_points <- do.call(rbind, region_holder)
        rm(temp_region, temp_data, temp_centroid, this_name, region_holder, region_names,i)
        
        # HERE after clicking back and forth the labels vanish
        # shp@data$label <- shp@data$reg_name
        # shp@data$label[duplicated(shp@data$label)] <- ""
        
        # coords <- coordinates(shp)
        # coords <- data.frame(coords)
        # names(coords) <- c('x1', 'y1')
        # coords$label <- shp@data$reg_name
        # temp <- left_join(coords, center_points)
        # print('HEAD OF COORDS')
        # print(head(center_points))
        
        # create a character with "px" that takes into account zoom
        # px_size <- paste0(ceiling(24/(the_zoom*2)),'px')
        if(the_zoom <= 10 & the_zoom >= 1){ # BEN, change this to 2 if you want to suppress the continent labels
          leafletProxy('recent_mean_sub_leaf') %>%
            # addMapPane("abc", zIndex = 4410) %>%
            addLabelOnlyMarkers(data = center_points,
                                # layerId = 'abc',
                                lng =center_points$x,
                                lat = center_points$y,
                                label = center_points$label,
                                labelOptions = labelOptions(noHide = T, direction = 'center',textsize  = 5, textOnly = T))
          
          # addProviderTiles('CartoDB.PositronOnlyLabels',
          #                  options = pathOptions(pane = "country_labels"),
          #                  layerId = 'country_labs')
        } else {
          leafletProxy('recent_mean_sub_leaf') %>%
            removeTiles(layerId = 'country_labs')
        }
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
          # condition for temporary fake data
          if(ncol(temp)==12){
            temp <- data_frame(' '= 'Simulated data for this selection. Will be updated with real data.')
            write.csv(temp, file)
          } else {
            temp <- temp %>% filter(!is.na(value))
            names(temp) <- tolower(names(temp))
            temp$parameter <- 'Mean'
            temp$level <- 'Subnational'
            temp <- temp %>% select(region_name, country_name,reg_name, iso3, year,  survey_list, indic, indicator_short_name,
                                    indicator_description, parameter, level, value, unit_of_measure)
            names(temp) <- c('Region', 'Country_name', 'Subregion','Country_iso3', 'Year', 'Survey_name', 
                             'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                             'Value', 'Unit_of_measurement')
            write.csv(temp, file)
          }
         
        }
      }
    }
  )
  
  
  # ---- CAPTURE USER ZOOM LEVEL FOR DOWNLOAD ---- #
  user_zoom <- reactive({
    pop_map <- get_pop_map()
    if(is.null(pop_map) | is.na(pop_map)){
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
  
}


## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')