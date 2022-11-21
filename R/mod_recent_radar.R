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
  ns <- shiny::NS(id)
  # tagList(
  shiny::fluidPage(
    shiny::fluidRow(
      
      shiny::column(8,
                    shiny::uiOutput(ns('radar_title_ui')),
                    shiny::plotOutput(
               ns('recent_radar_plot'), height = 700 ),
      ),
      shiny::column(4,
                    shinyalert::useShinyalert(),
                    shiny::actionButton(ns("plot_info"), label = "Plot Info"),
             # actionButton(ns('share_chart'), 'Share chart'),
                    shiny::actionButton(ns('generate_chart'),label = 'Generate chart'),
             br(), br(),
             p('Country'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the countries', 
                                          status = "danger",
                                          div(style='max-height: 30vh; overflow-y: auto;',
                                              shiny::checkboxGroupInput(inputId = ns("country"),
                                                                 label = '', 
                                                                 choices = as.character(country_list),
                                                                 selected = as.character(country_list)[1:4]))),
             
             p('Indicator'),
             shiny::uiOutput(ns('indicator_ui')),
             
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
      )
    ))
}

# SERVER FOR MOST RECENT VALUE MAP
mod_recent_radar_server <- function(input, output, session){
  # ---- OBSERVE EVENT FOR PLOT INFO BUTTON ---- #
  shiny::observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(title = "Most recent value - National mean", 
               text = "Radar or spiderweb charts allow the displaying of achievement for multiple indicators at the same time and hence are a method to display the different dimensions of Universal Health Coverage in a single chart. Moreover, when data from multiple countries is included, radar charts allow multi-indicators cross-country comparisons. By default, radar charts use the latest available HEFPI data point for each selected indicator, but users can choose the time period from which this latest data point is chosen.",
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ui output for indicator
  output$indicator_ui <- shiny::renderUI({
    
    plot_years <- c(1982, 2021)
    country_names <- hefpi::country_list[1:4]
    country_names <- input$country
    plot_years <- input$date_range
    
    # HERE need to figure out a way to make the selection more smooth - that the plot data doesnt have NAs.
    pd <- hefpi::df %>%
      dplyr::filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      dplyr::filter(country %in% country_names) %>%
      dplyr::filter(indicator_short_name %in% percentage_inds$indicator_short_name)%>%
      dplyr::group_by(country, indicator_short_name) %>%
      dplyr::filter(year == max(year)) %>% 
      tidyr::drop_na() 
    
    indicator_names <- pd  %>% 
                        dplyr::group_by(indicator_short_name) %>% 
                        dplyr::summarise(counts = n()) %>% 
                        filter(counts == length(unique(pd$country))) %>% 
                        .$indicator_short_name
    
    
    
    shinyWidgets::dropdownButton(circle = FALSE,  
                                 label = 'Select indicators', 
                                 status = "danger",
                                 div(style='max-height: 30vh; overflow-y: auto;',
                                     shiny::checkboxGroupInput(inputId = session$ns("indicator"),
                                                        label = '', 
                                                        choices = as.character(indicator_names),
                                                        selected = as.character(indicator_names)[1:3]))) 
    
    
    
  })
  
  # ---- GENERATE REACTIVE LIST OF MAP ATTRIBUTES ---- #
  chart_data <- shiny::reactiveValues(plot_data = 'new') 
  shiny::observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs 
    plot_years <- input$date_range
    # plot_years <- c(1972, 2017)
    indicator <- input$indicator
    # indicator <- c('4+ antenatal care visits (%)', 'Modern contraceptive use, women (%)', 'Skilled birth attendance (%)')
    country_names <- input$country
    # country_names <- c("Afghanistan", "Angola", "Armenia")
    message('indicator is ', indicator)
    
    
    # create list to store results from reactive object
    pop_radar_list <- list()
    
    # Get the variable from indicator input
    ind_info <- hefpi::indicators %>%
      dplyr::filter(indicator_short_name %in% indicator) %>%
      dplyr::select(variable_name, good_or_bad, unit_of_measure)
    variable_name = ind_info$variable_name
    good_or_bad = ind_info$good_or_bad
    unit_of_measure = ind_info$unit_of_measure
    
    # Get the data, subsetted by inputs
    pd <- hefpi::df %>%
      dplyr::filter(year >= min(plot_years),
             year <= max(plot_years)) %>%
      dplyr::filter(indic %in% variable_name) %>%
      dplyr::filter(country %in% country_names) %>%
      dplyr::group_by(country, indicator_short_name) %>%
      dplyr::filter(year == max(year, na.rm = TRUE)) %>%
      dplyr::filter(referenceid_list == first(referenceid_list)) %>%
      dplyr::summarise(value = first(pop),
                indic = indic,
                year = year,
                region_name = region_name,
                survey_list = survey_list,
                data_source = referenceid_list,
                indicator_short_name = indicator_short_name,
                indicator_description = indicator_description,
                unit_of_measure = unit_of_measure) %>%
      dplyr::select(country, indicator_short_name, value) %>% 
      tidyr::spread(key = 'indicator_short_name', value= 'value') 
    
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
      pd <- pd %>% tibble::column_to_rownames(var="country")
      save(pd, file = 'temp_pd.rda')
      # names(pd) <- stringr::str_replace_all(names(pd),pattern = ' ', replacement = '\n' )
      # CUSTOM radar plot

      # clear plots in workspace
      graphics::plot.new()
      
      
      par(
                      fmsb::radarchart(
                        pd, axistype = 1,
                        # Customize the polygon
                        pcol = rcartocolor::carto_pal(ncol(pd), "Vivid"), pfcol = scales::alpha(rcartocolor::carto_pal(ncol(pd), "Vivid"), 0.35), plwd = 2, plty = 1,
                        # Customize the grid
                        cglcol = "grey", cglty = 1, cglwd = 0.8,
                        # Customize the axis
                        axislabcol = "grey", 
                        # Variable labels
                        vlcex = 0.7, vlabels = colnames(pd),
                        maxmin = FALSE,
                        caxislabels = NULL, title = NULL
                      ),
                      mfrow = c(1, 1)
      )
      # Add an horizontal legend
      graphics::legend(
        x = "bottom", legend = rownames(pd), horiz = TRUE,
        bty = "n", pch = 20 , col = rcartocolor::carto_pal(ncol(pd), "Vivid"),
        text.col = "black", cex = 1, pt.cex = 1.5
      )
      
      pop_radar <- grDevices::recordPlot()
      
      # pop_radar <- ggradar::ggradar(pd,
      #                               # axis.label.size = 3,
      #                               # grid.label.size = 5,
      #                               # group.colours = lcols,
      #                               plot.extent.y.sf = 1.2,
      #                               plot.extent.x.sf = 2.5,
      #                               background.circle.colour = "white",
      #                               gridline.min.linetype = 1,
      #                               gridline.mid.linetype = 1,
      #                               gridline.max.linetype = 1, 
      #                               legend.position = 'bottom') 
      
      
      year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
      
      # store palette, text, map object, and data in list
      pop_radar_list <- list(pop_radar, pd, year_title)
      # save(pop_radar_list, file = 'data/pop_radar_list.rda')
      # save(pop_radar_list, file = 'data/pop_radar_list.rda')
    }
    
    chart_data$plot_data <- pop_radar_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # get_radar_list <- reactive({
  # 
  #   # indicator <- indicator_names[c(1:4)]
  #   # plot_years <- c(1982, 2017)
  #   # country_names <- hefpi::country_list[1:4]
  #     
  #   # create list to store results from reactive object
  #   pop_radar_list <- list()
  #   
  #   # get inputs
  #   plot_years <- input$date_range
  #   indicator <- input$indicator
  #   country_names <- input$country
  #   message('indicator is ', indicator)
  #   if(is.null(indicator)){
  #     NULL
  #   } else {
  #     # save(plot_years, indicator, country_names, file = 'inputs.rda')
  #     
  #     # Get the variable from indicator input
  #     ind_info <- hefpi::indicators %>%
  #       filter(indicator_short_name %in% indicator) %>%
  #       select(variable_name, good_or_bad, unit_of_measure)
  #     variable_name = ind_info$variable_name
  #     good_or_bad = ind_info$good_or_bad
  #     unit_of_measure = ind_info$unit_of_measure
  #     
  #     # Get the data, subsetted by inputs
  #     pd <- hefpi::df %>%
  #       filter(year >= min(plot_years),
  #              year <= max(plot_years)) %>%
  #       filter(indic %in% variable_name) %>%
  #       filter(country %in% country_names) %>%
  #       group_by(country, indicator_short_name) %>%
  #       filter(year == max(year, na.rm = TRUE)) %>%
  #       filter(referenceid_list == first(referenceid_list)) %>%
  #       summarise(value = first(pop),
  #                 indic = indic,
  #                 year = year,
  #                 region_name = region_name,
  #                 survey_list = survey_list,
  #                 data_source = referenceid_list,
  #                 indicator_short_name = indicator_short_name,
  #                 indicator_description = indicator_description,
  #                 unit_of_measure = unit_of_measure) %>%
  #       select(country, indicator_short_name, value) %>% 
  #       spread(key = 'indicator_short_name', value= 'value') 
  #     
  #     # if the indicator (column name) is part of the financial protection group, then subtract the value from 1. 
  #     for(i in 2:ncol(pd)){
  #       this_col <- names(pd)[i]
  #       this_group <- percentage_inds$level_1[percentage_inds$indicator_short_name == this_col]
  #       if(this_group == 'Financial Protection' ){
  #         pd[,i] <- 1-pd[,i]
  #       }
  #     }
  #     # get indicator short name joined to data
  #     if(nrow(pd)==0 | any(is.na(pd))){
  #       pop_radar_list <- NA
  #     } else {
  #       # 
  #       # if(unit_of_measure == '%'){
  #       #   pd$value <- pd$value*100
  #       # } 
  #       # get into format for radar plot
  #       
  #       # lcols <- c("#EEA236", "#5CB85C", "#46B8DA")
  #       # use this as guide https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R
  #       pd <- as.data.frame(pd)
  #       save(pd, file = 'temp_pd.rda')
  #       pop_radar <- ggradar::ggradar(pd,
  #                                     # axis.label.size = 3,
  #                                     # grid.label.size = 5,
  #                                     # group.colours = lcols,
  #                                     background.circle.colour = "white",
  #                                     gridline.min.linetype = 1,
  #                                     gridline.mid.linetype = 1,
  #                                     gridline.max.linetype = 1, 
  #                                     legend.position = 'bottom') 
  #       
  #       
  #       year_title = paste0('From ', plot_years[1], ' to ', plot_years[2])
  #       
  #       # store palette, text, map object, and data in list
  #       pop_radar_list<- list(pop_radar, pd,year_title)
  #       
  #     }
  #     return(pop_radar_list)
  #   }
  # 
  # })
  
  # ---- RENDER MAP TITLE ---- #
  output$radar_title_ui <- shiny::renderUI({
    pop_radar <- chart_data$plot_data
    if(length(pop_radar)==1){
      pop_radar <- hefpi::pop_radar_list
    }
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
        shiny::fluidPage(
          shiny::fluidRow(
            # h4(paste0('Most recent value - National mean - ', indicator_name)),
            HTML(stringr::str_glue('
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
  output$recent_radar_plot <- shiny::renderPlot({
    pop_radar <- chart_data$plot_data
    if(length(pop_radar) == 1){
      pop_radar <- hefpi::pop_radar_list
    }
    if(is.null(pop_radar)){
      NULL
    } else {
      if(is.na(pop_radar)){
        p <- ggplot2::ggplot() + ggplot2::labs(title = "No data available for the selected inputs")
        p
      } else {
        p <- pop_radar[[1]]
        p
        
      }
    }
  })
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- shiny::downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_radar <- chart_data$plot_data
      if(length(pop_radar)==1){
        pop_radar <- hefpi::pop_radar_list
      }
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
  output$dl_plot <- shiny::downloadHandler(filename = paste0("most_recent_value_mean_", Sys.Date(), ".jpg"),
                                    content = function(file) {
                                      pop_radar <- chart_data$plot_data
                                      if(length(pop_radar) == 1){
                                        pop_radar <- hefpi::pop_radar_list
                                      }
                                      if(is.null(pop_radar)){
                                        NULL
                                      } else {
                                        if(is.na(pop_radar)){
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
                                          }
                                          p <- empty_plot("No data available for the selected inputs")
                                          ggplot2::ggsave(file, width = 8, height = 8)
                                          
                                        } else {
                                          pop_radar
                                          ggplot2::ggsave(file, width = 8, height = 8)
                                        }
                                      }
                                    })
  
}