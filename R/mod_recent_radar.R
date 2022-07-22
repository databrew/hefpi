# Module radar 
#' @title mod_recent_radar.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal

#' @keywords internal
#' @export 
# UI FOR MOST RECENT VALUE RADAR PLOT
mod_recent_radar_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      
      column(8,
             uiOutput(ns('radar_title_ui')),
             plotOutput(
               ns('recent_radar_plot'), height = 700 ),
      ),
      column(4,
             #useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Country'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the countries', 
                                          status = "danger",
                                          div(style='max-height: 30vh; overflow-y: auto;',
                                              checkboxGroupInput(inputId = ns("country"),
                                                                 label = '', 
                                                                 choices = as.character(country_list),
                                                                 selected = as.character(country_list)[1:4]))),
             
             p('Indicator'),
             uiOutput(ns('indicator_ui')),
             
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2018,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary')
      )
    ))
}

# SERVER FOR MOST RECENT VALUE MAP
mod_recent_radar_server <- function(input, output, session){
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
  
  # ui output for indicator
  output$indicator_ui <- renderUI({
    
    #plot_years <- c(1982, 2017)
    #country_names <- hefpi::country_list[2:4]
    country_names <- input$country
    plot_years <- input$date_range
    
    # HERE need to figure out a way to make the selection more smooth - that the plot data doesnt have NAs.
    pd <- hefpi::hefpi_df %>%
      filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      filter(country %in% country_names) %>%
      filter(indicator_short_name %in% percentage_inds$indicator_short_name)%>%
      group_by(country, indicator_short_name) %>%
      filter(year == max(year)) %>% 
      select(1:10, 356:362) %>% drop_na()
    
    indicator_names <-pd  %>% group_by(indicator_short_name) %>% summarise(counts = n()) %>% filter(counts == length(unique(pd$country))) %>% .$indicator_short_name
    
    
    
    shinyWidgets::dropdownButton(circle = FALSE,  
                                 label = 'Select indicators', 
                                 status = "danger",
                                 div(style='max-height: 30vh; overflow-y: auto;',
                                     checkboxGroupInput(inputId = session$ns("indicator"),
                                                        label = '', 
                                                        choices = as.character(indicator_names),
                                                        selected = as.character(indicator_names)[1:3]))) 
    
 
    
  })
  
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  get_radar_list <- reactive({

    #indicator <- indicator_names[c(1:4)]
    #plot_years <- c(1982, 2017)
    #country_names <- hefpi::country_list[1:4]
      
    # create list to store results from reactive object
    pop_radar_list <- list()
    
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    message('indicator is ', indicator)
    if(is.null(indicator)){
      NULL
    } else {
      # save(plot_years, indicator, country_names, file = 'inputs.rda')
      
      # Get the variable from indicator input
      ind_info <- hefpi::indicators %>%
        filter(indicator_short_name %in% indicator) %>%
        select(variable_name, good_or_bad, unit_of_measure)
      variable_name = ind_info$variable_name
      good_or_bad = ind_info$good_or_bad
      unit_of_measure = ind_info$unit_of_measure
      
      # Get the data, subsetted by inputs
      pd <- hefpi::hefpi_df %>%
        filter(year >= min(plot_years),
               year <= max(plot_years)) %>%
        filter(indic %in% variable_name) %>%
        filter(country %in% country_names) %>%
        group_by(country, indicator_short_name) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        filter(referenceid_list == first(referenceid_list)) %>%
        summarise(value = first(pop),
                  indic = indic,
                  year = year,
                  region_name = region_name,
                  #survey_list = survey_list,
                  data_source = referenceid_list,
                  indicator_short_name = indicator_short_name,
                  indicator_description = indicator_description,
                  unit_of_measure = unit_of_measure) %>%
        select(country, indicator_short_name, value) %>% 
        spread(key = 'indicator_short_name', value= 'value') 
      
      # if the indicator (column name) is part of the financial protection group, then subtract the value from 1. 
      for(i in 2:ncol(pd)){
        this_col <- names(pd)[i]
        this_group <- percentage_inds$level_1[percentage_inds$indicator_short_name == this_col]
        if(this_group == 'Financial Protection' ){
          pd[,i] <- 1-pd[,i]
        }
      }
      # get indicator short name joined to data
      if(nrow(pd)==0 | any(is.na(pd))){
        pop_radar_list <- NA
      } else {
        # 
        # if(unit_of_measure == '%'){
        #   pd$value <- pd$value*100
        # } 
        # get into format for radar plot
        
        # lcols <- c("#EEA236", "#5CB85C", "#46B8DA")
        # use this as guide https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R
        pd <- as.data.frame(pd)
        # save(pd, file = 'temp_pd.rda')
        pop_radar <- ggradar::ggradar(pd,
                                      axis.label.size = 3,
                                      grid.label.size = 5,
                                      # group.colours = lcols,
                                      background.circle.colour = "white",
                                      gridline.min.linetype = 1,
                                      gridline.mid.linetype = 1,
                                      gridline.max.linetype = 1, 
                                      legend.position = 'bottom') 
        
        
        year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
        
        # store palette, text, map object, and data in list
        pop_radar_list<- list(pop_radar, pd,year_title)
        
      }
      return(pop_radar_list)
    }

  })
  
  # ---- RENDER MAP TITLE ---- #
  output$radar_title_ui <- renderUI({
    pop_radar <- get_radar_list()
    if(is.null(pop_radar)){
      NULL
    } else {
      if(is.na(pop_radar)){
        
        h4('')
      } else {
        indicator_name <- input$indicator
        year_title <- pop_radar[[3]]
        # HTML(paste(h4(paste0('Most recent value - National mean - ', indicator_name)), '\n',
        #            h5(year_title)))
        fluidPage(
          fluidRow(
            # h4(paste0('Most recent value - National mean - ', indicator_name)),
            HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Most recent value </div> 
                          </div>
                          ')),
            h5(paste0(year_title))
            
          )
        )
        
      }
    }
  })
  
  # ---- RENDER RADAR PLOT ---- #
  output$recent_radar_plot <- renderPlot({
    pop_radar <- get_radar_list()
    if(is.null(pop_radar)){
      NULL
    } else {
      if(is.na(pop_radar)){
        p <- ggplot() + labs(title = "No data available for the selected inputs")
        p
      } else {
        p <- pop_radar[[1]]
        p
        
      }
    }
  })
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_radar <- get_radar_list()
      if(is.null(pop_radar)){
        NULL
      } else {
        if(is.na(pop_radar)){
          temp <- data_frame()
          write.csv(temp, file)
        } else {
          # get the map data from the second element of the list
          temp <- pop_radar[[2]]

          names(temp)[1] <- 'Country'
          # save(temp, file = 'temp_data.rda')
          # add stampe
          temp_stamp <- temp[1,]
          temp_stamp$Country<- 'HEFPI database, The World Bank, 2021'
          temp_stamp[, -1] <- ''
          temp <- rbind(temp, temp_stamp)
          write.csv(temp, file)
        }

      }
    }
  )


  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("most_recent_value_mean_", Sys.Date(), ".jpg"),
                                    content = function(file) {
                                      pop_radar <- get_radar_list()
                                      if(is.null(pop_radar)){
                                        NULL
                                      } else {
                                        if(is.na(pop_radar)){
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
                                          }
                                          p <- empty_plot("No data available for the selected inputs")
                                          ggsave(file, width = 8, height = 8)

                                        } else {
                                         pop_radar
                                        ggsave(file, width = 8, height = 8)
                                        }
                                      }
                                    })
  
}
