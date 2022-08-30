# Module Trends 
#' @title mod_trends.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @keywords internal
#' @export 

# UI FOR TRENDS (NATIONAL MEAN)
mod_trends_mean_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             uiOutput(ns('trends_mean_title_a')),
             plotlyOutput(
               ns('trends_mean'), height = '600px'
             )),
      column(3,
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             # HERE (try 3px or without 1px just solid #aaa)
             div(style='border: 1px #FF0000; color:black;',selectInput(ns('indicator'),
                                                                       label = NULL,
                                                                       choices = indicators_list,
                                                                       selected = '4+ antenatal care visits (%)')),
             # here need to create custom css for this to make size right
             p('Region'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the region(s)', 
                                          status = "danger",
                                          actionButton(ns("all_regions"), label="Select/Deselect all"),
                                          div(style='max-height: 80vh; overflow-y: auto;',
                                              checkboxGroupInput(ns('region'),
                                                                 label = "", 
                                                                 choices = as.character(region_list$region),selected = as.character(region_list$region)[1])
                                          )),
             uiOutput(ns('ui_outputs')),
             p('Date range'),
             chooseSliderSkin("Modern", color = "#002244"),
             sliderInput(ns('date_range'),
                         label =  NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER FOR TRENDS (NATIONAL MEAN)
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
    if(max_value < 1){
      min_value = 0
      max_value = 100
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    countries <- unique(df$country)
    fluidPage(
      fluidRow(
        p('Country'),
        shinyWidgets::dropdownButton(circle = FALSE,  
                                     label = 'Select the countries', 
                                     status = "danger",
                                     actionButton(session$ns("all_countries"), label="Select/Deselect all"),
                                     div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(session$ns('country'),
                                                                                                        label = NULL, 
                                                                                                        choices = countries,
                                                                                                        selected = countries))),
        
        p('Y axis range'),
        sliderInput(session$ns('value_range'),
                    label = NULL,
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = '')
      )
    )
  })
  
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # REGION
  observe({
    all_regions <- input$all_regions
    message(all_regions)
    if(is.null(all_regions)){
      NULL
    } else {
      if (all_regions > 0) {
        if (all_regions %% 2 == 0){
          message(region_list$region)
          updateCheckboxGroupInput(session=session,
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = as.character(region_list$region))
          
        } else {
          updateCheckboxGroupInput(session=session,  
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = c())
          
        }}
    }
    
  })
  
  # COUNTRY
  observe({
    all_countries <- input$all_countries
    message(all_countries)
    if(is.null(all_countries)){
      NULL
    } else {
      if (all_countries > 0) {
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
        
        countries <- unique(df$country)
        if (all_countries %% 2 == 0){
          
          updateCheckboxGroupInput(session=session,
                                   "country",
                                   choices = countries,
                                   selected = countries)
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   "country",
                                   choices = countries,
                                   selected = c())
          
        }}
    }
    
  })
  # Observe the "generate chart" button to put together the data for the chart
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
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
      chart_data$plot_data <- pop_list
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
        pop_list[[5]] <- c(0, 100)
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
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Survey_name <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
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
                                        pop_list[[5]] <- c(0, 100)
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
                                          y_axis_text <- paste0(indicator)
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          caption_text = 'HEFPI database, The World Bank, 2021'
                                          
                                          temp <- tableau_color_pal(palette = "Tableau 20")
                                          trend_palette <- rep(temp(n = 20), 10)
                                          
                                          
                                          if(unit_of_measure == '%'){
                                            pd$pop <- pd$pop*100
                                            # value_range[2] <- value_range[2]*100
                                            # value_range[1] <- value_range[1]*100
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
                                                   title = '',
                                                   caption=caption_text)
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
                                                   title = '',
                                                   caption = caption_text) 
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
                                                               legend_text_size = 2/3) +
                                            theme(
                                              panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                              panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                              panel.grid.major.x = element_blank(),
                                              panel.grid.minor.x = element_blank(),
                                              axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                              axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                                            )
                                          p
                                          ggsave(file, width = 8, height = 8, type = "cairo")
                                        }
                                      }
                                    })
  
  
  # ---- RENDER PLOT TITLE ---- 
  output$trends_mean_title_a <- renderUI({
    
    pop_list <- chart_data$plot_data
    if(length(pop_list)==1){
      pop_list <- hefpi::trends_national_mean_default
      pop_list[[5]] <- c(0, 100)
    }
    if(is.null(pop_list)){
      NULL
    } else {
      pd <- pop_list[[1]]
      
      indicator <- pop_list[[3]]
      # get title and subtitle
      # plot_title <- paste0('Trends', '-', '<br>', 'National mean', '-', indicator)
      plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Trends </div> 
                           <div class="chart-label"> {indicator} </div>
                          </div>
                          '))
      
      
      
    }
  })
  
  
  
  # ---- RENDER PLOT ---- 
  output$trends_mean <- renderPlotly({
    
    pop_list <- chart_data$plot_data
    if(length(pop_list)==1){
      pop_list <- hefpi::trends_national_mean_default
      pop_list[[5]] <- c(0, 100)
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
        # plot_title <- paste0('Trends', '-', '<br>', 'National mean', '-', indicator)
        y_axis_text <- paste0(indicator)
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
          # value_range[2] <- value_range[2]*100
          # value_range[1] <- value_range[1]*100
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
                 y = y_axis_text
                 # ,
                 # title = plot_title
            )
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

# ---------------------------------------------------------------------------------
# UI FOR TRENDS (SUBNATIONAL MEAN)
mod_trends_mean_sub_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             uiOutput(ns('trends_mean_title')),
             plotlyOutput(
               ns('trends_mean'), height = '600px'
             )),
      
      column(3,
             useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), label = 'Generate chart'),
             actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             div(style='border-color: grey; color:grey',selectInput(ns('indicator'), label=NULL,
                                                                    choices = indicators_list,
                                                                    selected = "4+ antenatal care visits (%)")),
             p('Country'),
             div(style='border-color: grey; color:grey',selectInput(inputId = ns("country"),
                                                                    label = NULL, 
                                                                    choices = as.character(sort(unique(sub_national$country))),
                                                                    selected ='Belize')),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER FOR TRENDS (SUBNATIONAL MEAN)
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
    date_range <- c(1982, 2021)
    
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
        p('Subnational region'),
        shinyWidgets::dropdownButton(circle = FALSE,  
                                     label = 'Select the region(s)', 
                                     status = "danger",
                                     actionButton(session$ns("all_regions"), label="Select/Deselect all"),
                                     div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = session$ns("sub_country"),
                                                                                                        label = '', 
                                                                                                        choices = sub_regions,
                                                                                                        selected = sub_regions_top))),
        p('Y axis range'),
        sliderInput(session$ns('value_range'),
                    label = NULL,
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = ''),
        p('Date range'),
        sliderInput(session$ns('date_range'),
                    label = NULL,
                    min = 1982,
                    max = 2021,
                    value = c(1982, 2021),
                    step = 1,
                    sep = ''),
        checkboxInput(session$ns('interpolate'), 
                      label = 'Interpolate missing values',
                      value = TRUE)
      )
    )
  })
  
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # REGIONS
  observe({
    all_regions <- input$all_regions
    message(all_regions)
    if(is.null(all_regions)){
      NULL
    } else {
      if (all_regions > 0) {
        # get inputs
        indicator <- input$indicator
        country_name <- input$country
        date_range <- c(1982, 2021)
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
        
        sub_regions_top <- df %>% group_by(country,ADM1_NAME) %>% 
          summarise(counts = n()) %>%
          top_n(10) %>%
          arrange(ADM1_NAME) %>%
          .$ADM1_NAME
        sub_regions <- sort(unique(df$ADM1_NAME))
        
        if (all_regions %% 2 == 0){
          updateCheckboxGroupInput(session=session,
                                   inputId ="sub_country",
                                   choices = sub_regions,
                                   selected = sub_regions)
          
        } else {
          updateCheckboxGroupInput(session=session,  
                                   inputId ="sub_country",
                                   choices =  sub_regions,
                                   selected = c())
          
        }}
    }
    
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
          save(temp, file='temp_subdata.rda')
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$national <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Survey_name <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
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
                                          y_axis_text <- paste0(indicator)
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          caption_text = 'HEFPI database, The World Bank, 2021'
                                          
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
                                                   title = '',
                                                   caption = caption_text) 
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
                                                   title = '',
                                                   caption = caption_text) 
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
  
  
  # ---- RENDER PLOT Title ---- #
  output$trends_mean_title <- renderUI({
    pop_list <- chart_data$plot_data
    if(length(pop_list)==1){
      pop_list <- hefpi::trends_subnational_mean_default
    }
    if(is.null(pop_list)){
      NULL
    } else {
      pd <- pop_list[[1]]
      
      indicator <- pop_list[[3]]
      
      plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Trends </div> 
                           <div class="chart-label"> {indicator} </div>
                          </div>
                          '))
      
      plot_title
      
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
        # plot_title <- paste0('Trends - Subnational mean - ', indicator)
        y_axis_text <- paste0(indicator)
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
                 y = y_axis_text
                 # ,
                 # title = plot_title
            ) 
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
                 y=y_axis_text
                 # ,
                 # title = plot_title
            ) 
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

# UI FOR TRENDS (CONENTRATION INDEX)
mod_trends_con_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             uiOutput(ns('chartRowLabels')),
             plotlyOutput(
               ns('trends_con'), height = '600px'
             )),
      column(3,
             useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             div(style='border-color: grey; color:grey',selectInput(ns('indicator'),
                                                                    label = NULL,
                                                                    choices = indicators_list,
                                                                    selected = '4+ antenatal care visits (%)')),
             p('Region'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the region(s)', 
                                          status = "danger",
                                          actionButton(ns("all_regions"), label="Select/Deselect all"),
                                          div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = ns("region"),
                                                                                                             label = '', 
                                                                                                             choices = as.character(region_list$region),
                                                                                                             selected = as.character(region_list$region)[1]))),
             uiOutput(ns('ui_outputs')),
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
      
    )
  )
}

# SERVER FOR TRENDS (CONENTRATION INDEX)
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
        p('Country'),
        dropdownButton(circle = FALSE,  
                       label = 'Select the countries', 
                       status = "danger",
                       actionButton(session$ns("all_countries"), label="Select/Deselect all"),
                       div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = session$ns("country"),
                                                                                          label = '', 
                                                                                          choices = countries,
                                                                                          selected = countries))),
        p('Y axis range'),
        sliderInput(session$ns('value_range'),
                    label = NULL,
                    min = min_value,
                    max = max_value,
                    step = 0.1, 
                    value = c(min_value, max_value),
                    sep = '')
      )
    )
  })
  
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # REGIONS
  observe({
    all_regions <- input$all_regions
    message(all_regions)
    if(is.null(all_regions)){
      NULL
    } else {
      if (all_regions > 0) {
        if (all_regions %% 2 == 0){
          message(region_list$region)
          updateCheckboxGroupInput(session=session,
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = as.character(region_list$region))
          
        } else {
          updateCheckboxGroupInput(session=session,  
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = c())
          
        }}
    }
  })
  
  # COUNTRY
  observe({
    all_countries <- input$all_countries
    message(all_countries)
    if(is.null(all_countries)){
      NULL
    } else {
      if (all_countries > 0) {
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
        if (all_countries %% 2 == 0){
          
          updateCheckboxGroupInput(session=session,
                                   "country",
                                   choices = countries,
                                   selected = countries)
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   "country",
                                   choices = countries,
                                   selected = c())
          
        }}
    }
  })
  
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    con_list <- list()
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
      unit_of_measure = ind_info$unit_of_measure
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
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Survey_name <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
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
                                          y_axis_text <- paste0(unlist(lapply(strsplit(indicator, '(', fixed = T), function(x) x[1])), ' (CI) ')
                                          x_axis_text <- paste0('', '\n', 'Year')
                                          caption_text = 'HEFPI database, The World Bank, 2021'
                                          
                                          temp <- tableau_color_pal(palette = "Tableau 20")
                                          trend_palette <- rep(temp(n = 20), 10)
                                          if(yn){ # condition if we connect the dots
                                            
                                            # if indicator % value
                                            if(str_detect(indicator, '%')) {
                                              p <- ggplot(data = pd, aes(year, CI, color= country)) +
                                                geom_point() + 
                                                geom_line(aes(group = country)) +
                                                scale_color_manual(name = '',
                                                                   values = trend_palette) +
                                                scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                   expand = c(0,0), labels = scales::percent)+
                                                scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                   breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                   expand = c(0,0)) +
                                                labs(title = plot_title,
                                                     x=x_axis_text,
                                                     y = y_axis_text,
                                                     caption=caption_text) 
                                            } else {
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
                                                     y = y_axis_text,
                                                     caption=caption_text) 
                                            }
                                            
                                          } else {# condition if we connect the dots
                                            
                                            # if indicator % value
                                            if(str_detect(indicator, '%')) {
                                              p <- ggplot(data = pd, aes(year, CI, color= country)) +
                                                geom_point() +
                                                scale_color_manual(name = '',
                                                                   values = trend_palette) +
                                                scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                                                   expand = c(0,0), labels = scales::percent)+
                                                scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                                                   breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                                   expand = c(0,0)) +
                                                labs(x=x_axis_text,
                                                     y = y_axis_text,
                                                     caption = caption_text) 
                                            } else {
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
                                                     y = y_axis_text,
                                                     caption = caption_text) 
                                            }
                                            
                                            
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
  
  # Plot title (trend)
  output$chartRowLabels <- renderUI({
    con_list <- chart_data$plot_data
    if(length(con_list)==1){
      con_list <- hefpi::trends_national_ci_default
      
    }
    if(is.null(con_list)){
      NULL
    } else {
      
      # unit_of_measure <- con_list[[2]]
      indicator <- con_list[[2]]
      # get title and subtitle
      
      plot_title_unlist_tmp <- unlist(lapply(strsplit(indicator, '(', fixed = T), function(x) x[1]))
      
      # plot_title <- paste0('Trends - Concentration index - ', )
      plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Concentration index </div>
                           <div class="chart-label"> Trends </div> 
                           <div class="chart-label"> {plot_title_unlist_tmp} </div>
                          </div>
                          '))
      
      plot_title
      
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
        # plot_title_unlist_tmp <- unlist(lapply(strsplit(indicator, '(', fixed = T), function(x) x[1]))
        
        # plot_title <- paste0('Trends - Concentration index - ', )
        # plot_title <- paste0(HTML(str_glue('
        #                 <div class="chart-header-labels-row">
        #                    <div class="chart-label"> Trends </div> 
        #                    <div class="chart-label"> Concentration index </div>
        #                    <div class="chart-label"> {plot_title_unlist_tmp} </div>
        #                   </div>
        #                   ')))
        y_axis_text <- paste0(unlist(lapply(strsplit(indicator, '(', fixed = T), function(x) x[1])), ' (CI) ')
        x_axis_text <- paste0('', '\n', 'Year')
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country),"<br>", 
          "Value: ", paste0(ifelse(str_detect(indicator, '%'), round(pd$CI, digits = 2) * 100, round(pd$CI, digits = 2)), ' (', pd$unit_of_measure, ')'), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          "Data source: ", as.character(pd$referenceid_list), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 10)
        if(yn) { # condition if we connect the dots
          
          # condition if indicator is % value
          if(str_detect(indicator, '%')) {
            p <- ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
              geom_point() + 
              geom_line(aes(group = country)) +
              scale_color_manual(name = '',
                                 values = trend_palette) +
              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                 expand = c(0,0),labels = scales::percent)+
              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                 expand = c(0,0)) +
              labs(
                # title = plot_title,
                x=x_axis_text,
                y = y_axis_text)
          } else {
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
              labs(
                # title = plot_title,
                x=x_axis_text,
                y = y_axis_text)
          }
          
        } else { # condition if we connect the dots
          
          # condition if indicator is % value
          if(str_detect(indicator, '%')) {
            p <- ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
              geom_point() +
              scale_color_manual(name = '',
                                 values = trend_palette) +
              scale_y_continuous(limits = c(value_range[1], value_range[2]), 
                                 expand = c(0,0), labels = scales::percent)+
              scale_x_continuous(limits = c(date_range[1], (date_range[2] + 1)), 
                                 breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                 expand = c(0,0)) +
              labs(x=x_axis_text,
                   y = y_axis_text) 
          } else {
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
# UI FOR TRENDS (QUINTILE)
mod_trends_quin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             uiOutput(ns('trends_quin_title')),
             plotlyOutput(
               ns('trends_quin'),  height = '600px'
             )),
      column(3,
             useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'), 'Generate chart'),
             actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             div(style='border-color: grey; color:grey',selectInput(ns('indicator'), 
                                                                    label = NULL,
                                                                    choices = indicators_list,
                                                                    selected = 'Inpatient care use, adults (%)')),
             p('Country'),
             div(style='border-color: grey; color:grey',selectInput(ns('country'), 
                                                                    label = NULL,
                                                                    choices = country_list,
                                                                    selected = 'United States')),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER FOR TRENDS (QUINTILE)
mod_trends_quin_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintile - Trends", 
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
                    max = 2021,
                    value = c(1982, 2021),
                    step = 1,
                    sep = ''),
        p('View as'),
        div(style='border-color: grey; color:grey',selectInput(session$ns('view_as'), 
                                                               label = NULL,
                                                               choices =c('Slope chart', 'Line chart')))
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
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Survey_name <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
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
                                          y_axis_text = paste0(indicator)
                                          x_axis_text = paste0('', '\n', 'Year')
                                          caption_text = 'HEFPI database, The World Bank, 2021'
                                          
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
                                                 title = plot_title,
                                                 caption = caption_text)
                                          
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
  output$trends_quin_title <- renderUI({
    quin_list <- chart_data$plot_data
    if(length(quin_list)==1){
      quin_list <- hefpi::trends_national_quin_default
    }
    if(is.null(quin_list)){
      NULL
    } else {
      pd <- quin_list[[1]]
      
      
      indicator <- quin_list[[3]]
      
      country_names <- quin_list[[7]]
      
      
      # plot_title = paste0('Quintile - Trends - ',indicator, ' - ', country_names)
      plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Quintile </div> 
                           <div class="chart-label"> Trends </div>
                           <div class="chart-label"> {indicator} </div>
                           <div class="chart-label"> {country_names} </div>
                          </div>
                          '))
      plot_title
      
      
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
        y_axis_text = paste0(indicator)
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
               x = x_axis_text
               # ,
               # title = plot_title
          ) 
        
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_angle = 90,
                                    x_axis_vjust =0.5,
                                    y_axis_vjust = 0.5,
                                    y_axis_hjust = 1,
                                    x_axis_size = 12) +
          theme(
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                              colour = "#cccccc"),
            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                              colour = "#cccccc"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks = element_line(size = 0.5, linetype = 'solid',
                                      colour = "#cccccc"),
            axis.line = element_line(colour = "#cccccc", 
                                     size = 0.5, linetype = "solid")
          )
        fig <- ggplotly(p, 
                        tooltip = 'text') %>%
          config(displayModeBar = F)
        fig
      }
    }
  })
}