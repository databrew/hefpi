# Module Trends 

#' @title mod_trends.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_trends_mean_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import shinyWidgets
#' @import reshape2
#' @importFrom shiny NS tagList 
#' 
#' 
mod_trends_mean_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             plotlyOutput(
               ns('trends_mean'), height = '600px'
             )),
      column(3,
             # useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             br(), br(),
             selectInput(ns('indicator'),
                         'Indicator',
                         choices = indicators_list,
                         selected = '4+ antenatal care visits'),
             dropdown(
               pickerInput(inputId = ns("region"),
                           label = 'Region', 
                           choices = as.character(region_list$region),
                           selected = as.character(region_list$region)[1],
                           options = pickerOptions(
                             `actions-box` = TRUE),
                           multiple = TRUE)
             ),
             
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2018,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# Module Server
#' @rdname mod_trends_mean_server
#' @export
#' @import tidyverse
#' @import plotly
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_mean_server <- function(input, output, session){
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Trends - National mean", 
               text = "This chart allows tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  
  # ---- GENERATE UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    indicator <- '4+ antenatal care visits'
    region = region_list$region[1]
    # get inputs
    indicator <- input$indicator
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$regioncode %in% region_code,]
    df <- df[df$indic == variable,]
    max_value <- round(max(df$pop), 2)
    min_value <- round(min(df$pop), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    countries <- unique(df$country)
    fluidPage(
      fluidRow(
        selectInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = countries,
                    selected = countries,
                    multiple = TRUE),
        sliderInput(session$ns('value_range'),
                    'Y axis range',
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = '')
      )
    )
  })
  
  # Observe the "generate chart" button to put together the data for the chart
  
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    # date_range = c(1982, 2018)
    # value_range = c(0,1)
    # country_names = countries
    pop_list <- list()
    indicator <- input$indicator
    region <- input$region
    date_range <- input$date_range
    value_range <- input$value_range
    country_names <- input$country
    yn <- input$interpolate
    
    # control for charts that are temporarily NULL between input selection
    if(is.null(value_range)){
      NULL
    } else {
      indicators <- hefpi::indicators
      # get variable
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      unit_of_measure = ind_info$unit_of_measure
      # subet by variable, region code and a list of countries
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      df <- hefpi::df
      df <- df[df$indic == variable_name,]
      df <- df[df$regioncode %in% region_code,]
      pd <- df[df$country %in% country_names,]
      pd <- pd %>% filter(year >= min(date_range),
                          year <= max(date_range)) 
      pd <- pd %>% filter(pop >= min(value_range),
                          pop <= max(value_range)) 
      
      
      pop_list <- list(pd, unit_of_measure, indicator, date_range, value_range,yn)
      # trends_national_mean <- pop_list
      chart_data$plot_data <- pop_list
      # message('pop_list is of type:')
      # print(str(pop_list))
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("trends_mean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_list <- chart_data$plot_data
      if(length(pop_list)==1){
        pop_list <- hefpi::trends_national_mean_default
      }
      if(is.null(pop_list)){
        NULL
      } else {
        pd <- pop_list[[1]]
        if(nrow(pd)==0){
          temp <- data_frame()
        } else {
          temp <- pd
          # subset by  
          temp$parameter <- 'Mean'
          temp$level <- 'National'
          temp <- temp %>% select(region_name, country, iso3c, 
                                  year, referenceid_list, survey_list, 
                                  indic, indicator_short_name,
                                  indicator_description, parameter, 
                                  level, pop, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
        }
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("trends_mean_",Sys.Date(), ".png"),
                                    content = function(file) {
                                      
                                      
                                      pop_list <- chart_data$plot_data
                                      if(length(pop_list)==1){
                                        pop_list <- hefpi::trends_national_mean_default
                                      }
                                      if(is.null(pop_list)){
                                        NULL
                                      } else {
                                        pd <- pop_list[[1]]
                                        if(nrow(pd)==0){
                                          empty_plot <- function(title = NULL){
                                            p <- plotly_empty(type = "scatter", mode = "markers                                                                                                                                                                        ") %>%
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
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        } else {
                                          pd <- pop_list[[1]]
                                          unit_of_measure <- pop_list[[2]]
                                          indicator <- pop_list[[3]]
                                          date_range <- pop_list[[4]]
                                          value_range <- pop_list[[5]]
                                          yn <- pop_list[[6]]
                                          
                                          # get title and subtitle
                                          y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          temp <- tableau_color_pal(palette = "Tableau 20")
                                          trend_palette <- rep(temp(n = 20), 10)
                                         
                                          
                                          if(unit_of_measure == '%'){
                                            pd$pop <- pd$pop*100
                                            value_range[2] <- value_range[2]*100
                                            value_range[1] <- value_range[1]*100
                                          }
                                          
                                          if(yn){
                                            p <- ggplot(data = pd, aes(year, pop, color= country)) +
                                              geom_point() + 
                                              geom_line(aes(group = country)) +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] +1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                 expand = c(0,0)) +
                                              labs(x=x_axis_text,
                                                   y = y_axis_text,
                                                   title = '')
                                          } else {
                                            # condition if we connect the dots
                                            p <- ggplot(data = pd, aes(year, pop, color= country)) +
                                              geom_point() +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] +1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1),
                                                                 expand = c(0,0)) +
                                              labs(x='Year',
                                                   y = y_axis_text,
                                                   title = '') 
                                          }
                                          p <- p +
                                            hefpi::theme_hefpi(grid_major_x = NA,
                                                               x_axis_angle = 90,
                                                               x_axis_vjust =0.5,
                                                               y_axis_vjust = 0.5,
                                                               y_axis_hjust = 1,
                                                               x_axis_size = 12,
                                                               legend_position = 'top',
                                                               legend_direction = 'horizontal',
                                                               legend_text_size = 2/3)
                                          p
                                          ggsave(file, width = 8, height = 8, type = "cairo")
                                        }
                                      }
                                    })
  
  # ---- RENDER PLOT ---- 
  output$trends_mean <- renderPlotly({
    
    pop_list <- chart_data$plot_data
    if(length(pop_list)==1){
      pop_list <- hefpi::trends_national_mean_default
    }
    if(is.null(pop_list)){
      NULL
    } else {
      pd <- pop_list[[1]]
      if(nrow(pd)==0){
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
        fig <- empty_plot("No data available for the selected inputs")
      } else {
        pd <- pop_list[[1]]
        unit_of_measure <- pop_list[[2]]
        indicator <- pop_list[[3]]
        date_range <- pop_list[[4]]
        value_range <- pop_list[[5]]
        yn <- pop_list[[6]]
        # get title and subtitle
        plot_title <- paste0('Trends - National mean - ', indicator)
        y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
        x_axis_text <- paste0('', '\n', 'Year')
        # condition on unit of measure
        
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country),"<br>", 
          "Value: ", paste0(round(pd$pop, digits = 2), ' (', unit_of_measure, ')'), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          "Data source: ", as.character(pd$referenceid_list), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 10)
        
        
        if(unit_of_measure == '%'){
          pd$pop <- pd$pop*100
          value_range[2] <- value_range[2]*100
          value_range[1] <- value_range[1]*100
        }
        
        if(yn){
          p <- ggplot(data = pd, aes(year, pop, color= country, text=mytext)) +
            geom_point() + 
            geom_line(aes(group = country)) +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] +1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                               expand = c(0,0)) +
            labs(x=x_axis_text,
                 y = y_axis_text,
                 title = plot_title)
        } else {
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(year, pop, color= country, text=mytext)) +
            geom_point() +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] +1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1),
                               expand = c(0,0)) +
            labs(x='Year',
                 y = y_axis_text,
                 title = plot_title) 
        }
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_angle = 90,
                                    x_axis_vjust =0.5,
                                    y_axis_vjust = 0.5,
                                    y_axis_hjust = 1,
                                    x_axis_size = 12)
        fig <- ggplotly(p, 
                        tooltip = 'text') %>%
          config(displayModeBar = F)
        fig
      }
    }
  })
}


#' @rdname mod_trends_mean_sub_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 

mod_trends_mean_sub_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             plotlyOutput(
               ns('trends_mean'), height = '600px'
             )),
      
      column(3,
             useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), label = 'Generate chart'),
             br(), br(),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = "4+ antenatal care visits"),
             selectInput(inputId = ns("country"),
                         label = 'Country', 
                         choices = as.character(sort(unique(sub_national$country))),
                         selected ='Belize'),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# Module Server
#' @rdname mod_trends_mean_server
#' @export
#' @import tidyverse
#' @import plotly
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_mean_sub_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Trends - Subnational mean", 
               text = "This chart allows tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    # get inputs
    indicator <- input$indicator
    country_name <- input$country
    date_range <- c(1982, 2018)
    # Get the data to be plotted
    pd <- hefpi::sub_national %>%
      filter(country == country_name)%>%
      filter(indicator_short_name == indicator) %>%
      group_by(ISO3 = iso3c, country,gaul_code) %>%
      filter(year >= min(date_range),
             year <= max(date_range)) 
    # get shape files
    shp <- hefpi::gaul
    # joine with data
    shp@data <- shp@data %>% dplyr::right_join(pd, by=c('ADM1_CODE'='gaul_code'))
    # remove polygons associated with NA - keeps only that region
    na_rows <- which(!is.na(shp@data$value))
    shp <- shp[na_rows,]
    shp@data$ADM1_NAME <- as.character(shp@data$ADM1_NAME)
    df <- shp@data
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    sub_regions_top <- df %>% group_by(country,ADM1_NAME) %>% 
      summarise(counts = n()) %>%
      top_n(10) %>%
      arrange(ADM1_NAME) %>%
      .$ADM1_NAME
    sub_regions <- sort(unique(df$ADM1_NAME))
    
    fluidPage(
      fluidRow(
        selectInput(inputId = session$ns("sub_country"),
                    label = 'Subnational region', 
                    choices = sub_regions,
                    selected = sub_regions_top,
                    multiple = TRUE),
        sliderInput(session$ns('value_range'),
                    'Y axis range',
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = ''),
        sliderInput(session$ns('date_range'),
                    'Date range',
                    min = 1982,
                    max = 2018,
                    value = c(1982, 2018),
                    step = 1,
                    sep = ''),
        checkboxInput(session$ns('interpolate'), 
                      'Interpolate missing values',
                      value = TRUE)
      )
    )
  })

  
 
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    indicator <- input$indicator
    region <- input$region
    sub_regions <- input$sub_country
    date_range <- input$date_range
    value_range <- input$value_range
    country_name <- input$country
    yn <- input$interpolate
    
    # condition for data that is temporariliy null
    if(is.null(value_range) | is.null(date_range)){
      NULL
    } else {
      # get list to store plot objects
      pop_list <- list()
      indicators <- hefpi::indicators
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      unit_of_measure = ind_info$unit_of_measure
      # get data
      pd <- hefpi::sub_national %>%
        filter(country == country_name) %>%
        filter(indicator_short_name == indicator) %>%
        group_by(ISO3 = iso3c, country,gaul_code) %>%
        filter(year >= min(date_range),
               year <= max(date_range))
      names(pd)[names(pd)=='region'] <- 'region_name'
      # get shape files
      shp <- hefpi::gaul
      # joine with data
      shp@data <- shp@data %>% dplyr::right_join(pd, by=c('ADM1_CODE'='gaul_code'))
      # remove polygons associated with NA - keeps only that region
      na_rows <- which(!is.na(shp@data$value))
      shp <- shp[na_rows,]
      shp@data$ADM1_NAME <- as.character(shp@data$ADM1_NAME)
      pd <- shp@data
      pd <- pd %>% filter(ADM1_NAME %in% sub_regions)
      pd <- pd %>% mutate_all(as.character)
      pd$value <- as.numeric(pd$value)
      drop_cols <- c("G2008_1_", "G2008_1_ID", "ADM0_NAME", "ADM0_CODE", "AREA", "PERIMETER")
      pd <- pd %>% select(-one_of(drop_cols)) %>% group_by_if(is.character) %>% summarise_if(is.numeric, funs(mean))
      # change name of subregion to national if it is the countries name 
      this_name = unique(pd$ADM1_NAME)[unique(pd$ADM1_NAME) %in% country_name]
      if(length(this_name) != 0){
        pd$ADM1_NAME <- ifelse(pd$ADM1_NAME == this_name, 'National', pd$ADM1_NAME)
      }
      pd <- pd %>% filter(value >= value_range[1],
                          value <= value_range[2])
      
      pop_list <- list(pd, unit_of_measure, indicator, date_range, value_range,yn)
      
      chart_data$plot_data <- pop_list
      
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("trends_mean_sub_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      pop_list <- chart_data$plot_data
      if(length(pop_list)==1){
        pop_list <- hefpi::trends_subnational_mean_default
      }
      
      if(is.null(pop_list)){
        NULL
      } else {
        pd <- pop_list[[1]]
        if(nrow(pd)==0){
          temp <- data_frame()
        } else {
          temp <- pd
          temp <- temp %>% filter(!is.na(value))
          names(temp) <- tolower(names(temp))
          names(temp)[which(names(temp)=='adm1_name')] <- 'level'
          temp$parameter <- 'Mean'
          temp <- temp %>% ungroup %>% select(region_name, country, iso3, year,  
                                              survey, indic, indicator_short_name,
                                              indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'national','Country_iso3', 'Year', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
        }
        write.csv(x = temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("trends_mean_sub", Sys.Date(),".png"),
                                    content = function(file) {
                                      pop_list <- chart_data$plot_data
                                      if(length(pop_list)==1){
                                        pop_list <- hefpi::trends_subnational_mean_default
                                      }
                                      if(is.null(pop_list)){
                                        NULL
                                      } else {
                                        pd <- pop_list[[1]]
                                        if(nrow(pd)==0){
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
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        } else {
                                          pd <- pop_list[[1]]
                                          unit_of_measure <- pop_list[[2]]
                                          indicator <- pop_list[[3]]
                                          date_range <- pop_list[[4]]
                                          value_range <- pop_list[[5]]
                                          yn <- pop_list[[6]]
                                          # get title and subtitle
                                          plot_title <- paste0('Trends - Subnational mean - ', indicator)
                                          y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          # condition on unit of measure
                                          if(unit_of_measure == '%'){
                                            pd$value<- pd$value*100
                                            value_range[2] <- value_range[2]*100
                                            value_range[1] <- value_range[1]*100
                                            
                                          }
                                          pd <- pd %>% filter(value >= value_range[1],
                                                              value <= value_range[2])
                                          # text for plot
                                          mytext <- paste(
                                            "Indicator: ", indicator,"<br>", 
                                            "Economy: ", as.character(pd$country), '<br>',
                                            "Subregion: ", as.character(pd$ADM1_NAME),"<br>", 
                                            "Value: ", paste0(round(pd$value, digits = 2), ' (', unit_of_measure, ')'), "<br>",
                                            "Year: ", as.character(pd$year),"<br>",
                                            sep="") %>%
                                            lapply(htmltools::HTML)
                                          temp <- tableau_color_pal(palette = "Tableau 20")
                                          trend_palette <- rep(temp(n = 20), 50)
                                          
                                          
                                          if(yn){
                                            # condition if we connect the dots
                                            p <-  ggplot(data = pd, aes(as.numeric(year), value,color= ADM1_NAME, group =ADM1_NAME)) +
                                              geom_point() + 
                                              geom_line() +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                 expand = c(0,0)) +
                                              labs(x=x_axis_text,
                                                   y = y_axis_text,
                                                   title = '') 
                                          } else {
                                            # condition if we connect the dots
                                            p <- ggplot(data = pd, aes(as.numeric(year), value, color= ADM1_NAME)) +
                                              geom_point() + 
                                              # geom_line(aes(group = ADM1_NAME)) +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                 expand = c(0,0)) +
                                              labs(x=x_axis_text,
                                                   y=y_axis_text,
                                                   title = '') 
                                            p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                                                        x_axis_angle = 90,
                                                                        x_axis_hjust = 1)
                                          }
                                          
                                          p <- p + ggtitle('') +
                                            hefpi::theme_hefpi(grid_major_x = NA,
                                                               x_axis_angle = 90,
                                                               x_axis_vjust =0.5,
                                                               y_axis_vjust = 0.5,
                                                               y_axis_hjust = 1,
                                                               x_axis_size = 12,
                                                               legend_position = 'top',
                                                               legend_direction = 'horizontal',
                                                               legend_text_size = 2/3)
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        }
                                      }
                                    })
  # ---- RENDER PLOT ---- #
  output$trends_mean <- renderPlotly({
    pop_list <- chart_data$plot_data
    if(length(pop_list)==1){
      pop_list <- hefpi::trends_subnational_mean_default
    }
    if(is.null(pop_list)){
      NULL
    } else {
      pd <- pop_list[[1]]
      if(nrow(pd)==0){
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
        fig <- empty_plot("No data available for the selected inputs")
      } else {
        pd <- pop_list[[1]]
        unit_of_measure <- pop_list[[2]]
        indicator <- pop_list[[3]]
        date_range <- pop_list[[4]]
        value_range <- pop_list[[5]]
        yn <- pop_list[[6]]
        # get title and subtitle
        plot_title <- paste0('Trends - Subnational mean - ', indicator)
        y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
        x_axis_text <- paste0('', '\n', 'Year')
        # condition on unit of measure
        if(unit_of_measure == '%'){
          pd$value<- pd$value*100
          value_range[2] <- value_range[2]*100
          value_range[1] <- value_range[1]*100
          
        }
       
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country), '<br>',
          "Subregion: ", as.character(pd$ADM1_NAME),"<br>", 
          "Value: ", paste0(round(pd$value, digits = 2), ' (', unit_of_measure, ')'), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 50)
        
        
        if(yn){
          # condition if we connect the dots
          p <-  ggplot(data = pd, aes(as.numeric(year), value,color= ADM1_NAME, group =ADM1_NAME, text=mytext)) +
            geom_point() + 
            geom_line() +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], (value_range[2]+2)), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                               expand = c(0,0)) +
            labs(x=x_axis_text,
                 y = y_axis_text,
                 title = plot_title) 
        } else {
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(as.numeric(year), value, color= ADM1_NAME, text=mytext)) +
            geom_point() + 
            # geom_line(aes(group = ADM1_NAME)) +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], (value_range[2]+2)), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                               expand = c(0,0)) +
            labs(x=x_axis_text,
                 y=y_axis_text,
                 title = plot_title) 
          p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                      x_axis_angle = 90,
                                      x_axis_hjust = 1)
        }
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_angle = 90,
                                    x_axis_vjust =0.5,
                                    y_axis_vjust = 0.5,
                                    y_axis_hjust = 1,
                                    x_axis_size = 12)
        fig <- ggplotly(p, 
                        tooltip = 'text') %>%
          config(displayModeBar = F)
        fig
      }
      
    }
  })
}



#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_con_sub_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 

mod_trends_con_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             plotlyOutput(
               ns('trends_con'), height = '600px'
             )),
      column(3,
             useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             br(), br(),
             selectInput(ns('indicator'),
                         'Indicator',
                         choices = indicators_list,
                         selected = '4+ antenatal care visits'),
             selectInput(inputId = ns("region"),
                         label = 'Region', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1],
                         multiple = TRUE),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2018,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))

    )
  )
}

# Module Server
#' @rdname mod_trends_con_sub_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

### HERE NEED TO IMPLEMENT CORRECT DATA DOWNLOAD FOR REST OF TREND DATA
mod_trends_con_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Concentration Index - Trends", 
               text = "This chart allows users to track the over-time dynamics in an indicator’s concentration index. The concentration index is based on a measure of household wealth and bounded between -1 and 1. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. Negative values of the concentration index indicate disproportionate concentration of an indicator among the poor, and positive values disproportionate concentration among the rich. For instance, a negative value for infant mortality in a country means infant mortality is higher among the poor there. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- RENDER UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    # get inputs
    indicator <- input$indicator
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode %in% region_code,]
    countries <- unique(df$country)
    max_value <- 1
    min_value <- -1
    fluidPage(
      fluidRow(
        selectInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = countries,
                    selected = countries,
                    multiple = TRUE),
        sliderInput(session$ns('value_range'),
                    'Y axis range',
                    min = min_value,
                    max = max_value,
                    step = 0.1, 
                    value = c(min_value, max_value),
                    sep = '')
      )
    )
  })
  
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    con_list <- list()
    # indicator ='4+ antenatal care visits'
    # region = as.character(region_list$region)[1]
    # date_range = c(1982, 2018)
    # country_names = countries
    # value_range = c(min_value, max_value)
    #yn = TRUE
    indicator <- input$indicator
    region <- input$region
    country_names <- input$country
    date_range <- input$date_range
    yn <- input$interpolate
    value_range <- input$value_range
    if(is.null(value_range)){
      NULL
    } else {
      # get region code 
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      # get variable
      indicators <- hefpi::indicators
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      # subet by variable, region code and a list of countries
      df <- hefpi::df
      df <- df[df$indic == variable_name,]
      df <- df[df$regioncode %in% region_code,]
      pd <- df[df$country %in% country_names,]
      pd <- pd %>% filter(year >= min(date_range),
                          year <= max(date_range)) 
      pd$unit_of_measure <- 'CI'
    
      con_list <- list(pd, indicator, date_range, value_range,yn)
      
    }
    
    
    chart_data$plot_data <- con_list
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  

  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("trends_ci_", Sys.Date(), ".csv")
    },
    content = function(file) {
      con_list <- chart_data$plot_data
      if(length(con_list)==1){
        con_list <- hefpi::trends_national_ci_default
        
      }
      if(is.null(con_list)){
        NULL
      } else {
        pd <- con_list[[1]]
        if(nrow(pd)==0){
          temp <- data_frame()
          write.csv(temp, file)
          
        } else {
          temp <- pd
          temp <- temp %>% filter(!is.na(CI))
          names(temp) <- tolower(names(temp))
          temp$parameter <- 'Concentration Index'
          temp$level <- 'National'
          temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, survey_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, ci, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          write.csv(temp, file)
          
        }
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("trends_ci_", Sys.Date(),".png"),
                                    content = function(file) {
                                      con_list <- chart_data$plot_data
                                      if(length(con_list)==1){
                                        con_list <- hefpi::trends_national_ci_default
                                        
                                      }
                                      if(is.null(con_list)){
                                        NULL
                                      } else {
                                        pd <- con_list[[1]]
                                        if(nrow(pd)==0){
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
                                          pd <- con_list[[1]]
                                          # unit_of_measure <- con_list[[2]]
                                          indicator <- con_list[[2]]
                                          date_range <- con_list[[3]]
                                          value_range <- con_list[[4]]
                                          yn <- con_list[[5]]
                                          # get title and subtitle
                                          plot_title <- paste0('Trends - Concentration index - ', indicator)
                                          y_axis_text <- paste0(indicator, ' (CI) ')
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          
                                          temp <- tableau_color_pal(palette = "Tableau 20")
                                          trend_palette <- rep(temp(n = 20), 10)
                                          if(yn){
                                            # condition if we connect the dots
                                            p <- ggplot(data = pd, aes(year, CI, color= country)) +
                                              geom_point() + 
                                              geom_line(aes(group = country)) +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                 expand = c(0,0)) +
                                              labs(title = plot_title,
                                                   x=x_axis_text,
                                                   y = y_axis_text) 
                                          } else {
                                            # condition if we connect the dots
                                            p <- ggplot(data = pd, aes(year, CI, color= country)) +
                                              geom_point() +
                                              scale_color_manual(name = '',
                                                                 values = trend_palette) +
                                              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                 expand = c(0,0))+
                                              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                 expand = c(0,0)) +
                                              labs(x=x_axis_text,
                                                   y = y_axis_text) 
                                          }                                         
                                          p <- p + ggtitle('') +
                                            hefpi::theme_hefpi(grid_major_x = NA,
                                                               x_axis_angle = 90,
                                                               x_axis_vjust =0.5,
                                                               y_axis_vjust = 0.5,
                                                               y_axis_hjust = 1,
                                                               x_axis_size = 12,
                                                               legend_position = 'top',
                                                               legend_direction = 'horizontal',
                                                               legend_text_size = 2/3)
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        }
                                      }
                                    })
  
  # ---- GENERATE PLOT ---- #
  output$trends_con <- renderPlotly({
    con_list <- chart_data$plot_data
    if(length(con_list)==1){
      con_list <- hefpi::trends_national_ci_default
      
    }
    if(is.null(con_list)){
      NULL
    } else {
      pd <- con_list[[1]]
      if(nrow(pd)==0){
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
        fig <- empty_plot("No data available for the selected inputs")
      } else {
        pd <- con_list[[1]]
        # unit_of_measure <- con_list[[2]]
        indicator <- con_list[[2]]
        date_range <- con_list[[3]]
        value_range <- con_list[[4]]
        yn <- con_list[[5]]
        # get title and subtitle
        plot_title <- paste0('Trends - Concentration index - ', indicator)
        y_axis_text <- paste0(indicator, ' (CI) ')
        x_axis_text <- paste0('', '\n', 'Year')
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country),"<br>", 
          "Value: ", round(pd$CI, digits = 2), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          "Data source: ", as.character(pd$referenceid_list), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 10)
        if(yn){
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
            geom_point() + 
            geom_line(aes(group = country)) +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                               expand = c(0,0)) +
            labs(title = plot_title,
                 x=x_axis_text,
                 y = y_axis_text) 
        } else {
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
            geom_point() +
            scale_color_manual(name = '',
                               values = trend_palette) +
            scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                               expand = c(0,0))+
            scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                               expand = c(0,0)) +
            labs(x=x_axis_text,
                 y = y_axis_text) 
        }
        
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_angle = 90,
                                    x_axis_vjust =0.5,
                                    y_axis_vjust = 0.5,
                                    y_axis_hjust = 1,
                                    x_axis_size = 12)
        fig <- ggplotly(p, 
                        tooltip = 'text') %>%
          config(displayModeBar = F) 
        fig
      }
    }
  })
}



#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_quin_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 

mod_trends_quin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             plotlyOutput(
               ns('trends_quin'),  height = '600px'
             )),
      column(3,
             useShinyalert(), 
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             br(), br(),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             selectInput(ns('country'), '
                         Country',
                         choices = country_list,
                         selected = 'United States'),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# Module Server
#' @rdname mod_trends_quin_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_quin_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "quinile - Trends", 
               text = "This chart shows HEFPI indicator trends at the wealth quinile level, revealing if any inequalities have reduced, remained stable, or increased over time. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. Users can tailor the charts to their time period of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    # get input
    country_names <- input$country
    indicator <- input$indicator
    # get variable
    ind_info <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      select(variable_name, unit_of_measure)
    variable_name = ind_info$variable_name
    unit_of_measure = ind_info$unit_of_measure
    # subset by country and variable
    df <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic == variable_name) %>%
      select(year, Q1:Q5) 
    temp <- melt(df, id.vars = 'year')
    max_value <- round(max(temp$value, na.rm = TRUE), 2)
    min_value <- round(min(temp$value, na.rm = TRUE), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = floor(min_value)
      max_value = ceiling(max_value)
    }
    fluidRow(
      fluidPage(
        sliderInput(session$ns('value_range'),
                    'Y axis range',
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = ''),
        sliderInput(session$ns('date_range'),
                    'Date range',
                    min = 1982,
                    max = 2018,
                    value = c(1982, 2018),
                    step = 1,
                    sep = ''),
        selectInput(session$ns('view_as'), 'View as',
                    choices =c('Slope chart', 'Line chart'))
      )
    )
    
  })
  
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    country_names <- input$country
    indicator <- input$indicator
    date_range <- input$date_range
    view_as <- input$view_as
    value_range <- input$value_range
    # condition for the data set to be temporarily NULL
    if(is.null(value_range)){
      NULL
    } else {
      quin_list <- list()
      # get variable
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      unit_of_measure = ind_info$unit_of_measure
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country == country_names) %>%
        filter(indic == variable_name) %>%
        filter(year >= min(date_range),
               year <= max(date_range))  
      df <- df[complete.cases(df),]
      if(view_as == 'Slope chart'){
        year_begin = min(df$year)
        year_end = max(df$year)
        df <- df %>%
          filter(year == year_begin | year == year_end)
      }
      id_vars <- names(df)[!grepl('Q1|Q2|Q3|Q4|Q5', names(df))]
      df <- melt(df, id.vars = id_vars)
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      # condition on unit of measure
      if(unit_of_measure == '%'){
        df$value <- df$value*100
        value_range[2] <- value_range[2]*100
        value_range[1] <- value_range[1]*100
      }
      
      df <- df %>% 
        filter(year >= date_range[1],
               year <= date_range[2]) %>%
        filter(value >=value_range[1],
               value<= value_range[2])
      
     
      quin_list <- list(df, unit_of_measure, indicator, date_range, value_range, view_as, country_names)
     

    }
    
    chart_data$plot_data <- quin_list
    
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
 
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("trends_quiniles_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      quin_list <- chart_data$plot_data
      if(length(quin_list)==1){
        quin_list <- hefpi::trends_national_quin_default
        
      }
      if(is.null(quin_list)){
        NULL
      } else {
        df <- quin_list[[1]]
        if(nrow(df)==0){
          temp <- data_frame()
        } else {
          temp <- df
          names(temp) <- tolower(names(temp))
          names(temp)[names(temp)=='variable'] <- 'level'
          # subset by  
          temp$parameter <- 'Mean'
          # temp$level <- 'National'
          temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, survey_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, value, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
        }
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("trends_quiniles_", Sys.Date(),".png"),
                                    content = function(file) {
                                      quin_list <- chart_data$plot_data
                                      if(length(quin_list)==1){
                                        quin_list <- hefpi::trends_national_quin_default
                                      }
                                      if(is.null(quin_list)){
                                        NULL
                                      } else {
                                        df <- quin_list[[1]]
                                        if(nrow(df)==0){
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
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        } else {
                                          
                                          df <- quin_list[[1]]
                                          unit_of_measure <- quin_list[[2]]
                                          indicator <- quin_list[[3]]
                                          date_range <- quin_list[[4]]
                                          value_range <- quin_list[[5]]
                                          view_as<- quin_list[[6]]
                                          country_names <- quin_list[[7]]
                                          
                                          
                                          # get color graident 
                                          col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
                                          col_vec <- col_vec[-1]
                                          # make plot title
                                          plot_title = paste0('Quintile - Trends - ',indicator, ' - ', country_names)
                                          y_axis_text = paste0(indicator, ' (', unit_of_measure, ')')
                                          x_axis_text = paste0('', '\n', 'Year')
                                          # text for plot
                                          mytext <- paste(
                                            "Indicator: ", indicator, '\n',
                                            "Economy: ", country_names, '\n',
                                            "Value: ", paste0(round(df$value, digits = 2), ' (', unit_of_measure, ')'), "\n",
                                            "Year: ", as.character(df$year),"\n",
                                            "Data source: ", as.character(df$referenceid_list),"\n",
                                            sep="") %>%
                                            lapply(htmltools::HTML)
                                          p <- ggplot(data = df, aes(year, value, color = variable, text =mytext)) +
                                            geom_point() +
                                            geom_line(aes(group = as.character(variable))) +
                                            scale_color_manual(name = '',
                                                               values = col_vec) +
                                            scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                               expand = c(0,0))+
                                            scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                               breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                               expand = c(0,0)) +
                                            labs(y = y_axis_text,
                                                 x = x_axis_text,
                                                 title = plot_title)
                                          
                                          p <- p + ggtitle('') +
                                            hefpi::theme_hefpi(grid_major_x = NA,
                                                               x_axis_angle = 90,
                                                               x_axis_vjust =0.5,
                                                               y_axis_vjust = 0.5,
                                                               y_axis_hjust = 1,
                                                               x_axis_size = 12,
                                                               legend_position = 'top',
                                                               legend_direction = 'horizontal',
                                                               legend_text_size = 2/3)
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        }
                                      }
                                    })
  
  # ---- GENERATE PLOT ---- #
  output$trends_quin <- renderPlotly({
    quin_list <- chart_data$plot_data
    if(length(quin_list)==1){
      quin_list <- hefpi::trends_national_quin_default
    }
    if(is.null(quin_list)){
      NULL
    } else {
      pd <- quin_list[[1]]
      if(nrow(pd)==0){
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
        fig <- empty_plot("No data available for the selected inputs")
      } else {
        
        
        df <- quin_list[[1]]
        unit_of_measure <- quin_list[[2]]
        indicator <- quin_list[[3]]
        date_range <- quin_list[[4]]
        value_range <- quin_list[[5]]
        view_as<- quin_list[[6]]
        country_names <- quin_list[[7]]
        
        # get color graident 
        col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
        col_vec <- col_vec[-1]
        # make plot title
        plot_title = paste0('Quintile - Trends - ',indicator, ' - ', country_names)
        y_axis_text = paste0(indicator, ' (', unit_of_measure, ')')
        x_axis_text = paste0('', '\n', 'Year')
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator, '\n',
          "Economy: ", country_names, '\n',
          "Value: ", paste0(round(df$value, digits = 2), ' (', unit_of_measure, ')'), "\n",
          "Year: ", as.character(df$year),"\n",
          "Data source: ", as.character(df$referenceid_list),"\n",
          sep="") %>%
          lapply(htmltools::HTML)
        p <- ggplot(data = df, aes(year, value, color = variable, text =mytext)) +
          geom_point() +
          geom_line(aes(group = as.character(variable))) +
          scale_color_manual(name = '',
                             values = col_vec) +
          scale_y_continuous(limits = c(value_range[1], (value_range[2]+2)), 
                             expand = c(0,0))+
          scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                             breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                             expand = c(0,0)) +
          labs(y = y_axis_text,
               x = x_axis_text,
               title = plot_title) 
        
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_angle = 90,
                                    x_axis_vjust =0.5,
                                    y_axis_vjust = 0.5,
                                    y_axis_hjust = 1,
                                    x_axis_size = 12)
        fig <- ggplotly(p, 
                        tooltip = 'text') %>%
          config(displayModeBar = F)
        fig
      }
    }
  })
}

## To be copied in the UI
# mod_trends_mean_ui("trends_mean1")
# mod_trends_quin_ui("trends_quin1")
# mod_trends_con_ui("trends_con1")


## To be copied in the server
# callModule(mod_trends_mean_server, 'trends_mean1')
# callModule(mod_trends_quin_server, 'trends_quin1')
# callModule(mod_trends_con_server, 'trends_con1')