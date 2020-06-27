# Module dotplots

#' @title   mod_dots.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dots_country_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dots_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('dots_country'), height = '800px', width = '1000px'
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region[1])),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_dots_country_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import shinyWidgets
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dots_country_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintile Dotpot for countries", 
               text = "charts enable users to compare inequalities in health and service coverage outcomes both within and across countries. For a set of countries and an indicator the user specifies, the dot plot shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  output$ui_outputs <- renderUI({
    indicator <- "Inpatient care use, adults"
    region <- "Latin America & Caribbean"
    date_range = c(1982,2016)
    # get inputs
    indicator <- input$indicator
    region <- input$region
    date_range <- input$date_range
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # Get the variable
    variable <- indicators %>%
      dplyr::filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode == region_code,]
    df <- df %>% filter(year >= date_range[1],
                        year<=date_range[2]) %>% 
      filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4) & !is.na(Q5)) %>%
      select(year, country, referenceid_list,indic, Q1:Q5) 
    
    # made data long form
    df <- melt(df, id.vars = c('year', 'country', 'referenceid_list', 'indic'))
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    # create select input for country
    countries <- unique(df$country)
    
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = countries,
                    selected = countries,
                    width = '60%',
                    options = list( `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} countries"),
                    multiple = TRUE),
        sliderInput(session$ns('value_range'),
                    'X axis range',
                    min = min_value,
                    max = max_value,
                    post = '%',
                    value = c(min_value, max_value),
                    sep = '')
      )
      
    )
    
  })
  
  output$dots_country <- renderPlotly({
    # last_date <- '2018'
    indicator <- "BMI, adults"
    region <- "East Asia & Pacific"
    temp <- hefpi::df_series %>% filter(region == 'East Asia & Pacific')
    country_names <- unique(temp$country_name)
     value_range = c(0,28)
    date_range = c(1982,2018)
    last_date <- input$last_date
    region <- input$region
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    date_range <- input$date_range
    if(is.null(country_names) | is.null(value_range)){
      NULL
    } else {
      # Get the variable
      variable <- indicators %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        .$variable_name
      
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country %in% country_names) %>%
        filter(indic == variable) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) %>%
        left_join(indicators, by = c('indic' = 'variable_name'))
      
      # get year and keep only necessary columns
      df <- df %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) %>%
        select(year, country, referenceid_list, Q1:Q5)
      
      # made data long form
      df <- melt(df, id.vars = c('year', 'country', 'referenceid_list'))
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      
      # only keep data with no NAs
      df <- df[complete.cases(df),]
      
      # get color graident 
      col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
      col_vec <- col_vec[-1]
      
      # make plot title 
      plot_title = paste0('Quintile Dot Plots for Economies', ' - ', indicator)
      y_axis_text = indicator
      x_axis_text = paste0('Most recent value for', '\n', 'time period: ', date_range[1], ' - ', date_range[2])
      df <- df %>%
        filter(value >= value_range[1],
               value <= value_range[2])
      # # # text for plot
      mytext <- paste(
        "Value: ", round(df$value, digits = 3), "\n",
        "Year: ", as.character(df$year),"\n",
        "Indicator: ", as.character(indicator),"\n",
        "Data source: ", as.character(df$referenceid_list),
        sep="") %>%
        lapply(htmltools::HTML)

      
      # if the dataframe is null of empty make plot null
      if(is.null(df) | nrow(df) == 0){
        NULL
      } else {
        
        # p <- plot_ly(data = df, 
        #              x = ~value, 
        #              y = ~country, 
        #              color = ~variable, 
        #              colors = col_vec, text = mytext, 
        #              hoverinfo = 'text') %>%
        #   layout(title = plot_title,
        #          xaxis= list(title = 'Value', showticklabels = TRUE),
        #          yaxis= list(title = '', showticklabels = TRUE))
        # 
        # p
        
        print(ggplotly(ggplot(df, aes(x=country,
                                      y=value,
                                      text = mytext)) +
                         geom_point(size=5, alpha = 0.7, aes(color = variable)) +
                         geom_line(aes(group = country)) +
                         scale_color_manual(name = '',
                                            values = col_vec) +
                         scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(value_range[1], value_range[2])) +
                         labs(title=plot_title, x = x_axis_text, y ='') +
                         coord_flip() +
                         hefpi::theme_gdocs(), tooltip = 'text'))
        
        
      }
      
      
      
    }
    
  })
}


# -----------------------------------------------------------------------------------------------------------------------------
#' @rdname mod_dots_ind_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dots_ind_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('dots_ind'), height = '800px', width = '1000px'
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = indicators_list[[1]],
                         multiple = TRUE),
             selectInput(ns('country'), 'Country',
                         choices = as.character(country_list),
                         selected = 'United States'),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             uiOutput(ns('ui_value_range')),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_dots_ind_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @plotly
#' @import htmltools
#' @keywords internal

mod_dots_ind_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintile Dotpot for indicators", 
               text = "charts allow users to shed light on overall health and service coverage inequality in a country and to explore differences in inequalities across indicators. For instance, the chart reveals if a country achieves universal coverage of maternal and child health services while failing to enable equitable access to inpatient care. For every health and service coverage indicator in the HEFPI database and a country the user selects, the dot plot shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  output$ui_value_range <- renderUI({
    date_range = c(1982,2016)
    indicator <- indicators_list[[1]]
    country_names <- 'Canada'
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name %in% indicator) %>%
      .$variable_name
    
    # subset by country and variable
    df <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic %in% variable) %>%
      filter(year >= date_range[1],
             year <= date_range[2]) %>%
      left_join(indicators, by = c('indic' = 'variable_name'))
    
    # get year and keep only necessary columns
    df <- df %>%
      group_by(indicator_short_name) %>%
      arrange(desc(year)) %>%
      dplyr::filter(year == dplyr::first(year)) %>%
      select(year, country, referenceid_list,indicator_short_name, Q1:Q5) 
    
    # made data long form
    df <- melt(df, id.vars = c('year', 'country', 'referenceid_list', 'indicator_short_name'))
    # recode Quintiels
    df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                          ifelse(df$variable == 'Q2', 'Q2: Poor',
                                 ifelse(df$variable == 'Q3', 'Q3: Middle',
                                        ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
    
    # only keep data with no NAs
    df <- df[complete.cases(df),]
    
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    sliderInput(session$ns('value_range'),
                'X axis range',
                min = min_value,
                max = max_value,
                post = '%',
                value = c(min_value, max_value),
                sep = '')
    
  })
  output$dots_ind <- renderPlotly({
    # date_range = c(1982,2016)
    # indicator <- c('BMI, adults', 'BMI, men', 'BMI, women', 'Catastrophic health spending, 10%',
    #                'Catastrophic health spending, 25%', 'Height, adults', 'Height, men', 'Height, women')
    # country_names <- 'United States'
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    if(is.null(value_range)){
      NULL
    } else {
      
      # Get the variable
      variable <- indicators %>%
        filter(indicator_short_name %in% indicator) %>%
        .$variable_name
      
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country == country_names) %>%
        filter(indic %in% variable) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) %>%
        left_join(indicators, by = c('indic' = 'variable_name'))
      
      # get year and keep only necessary columns
      df <- df %>%
        group_by(indicator_short_name) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) %>%
        select(year, country, referenceid_list,indicator_short_name, Q1:Q5) 
      
      # made data long form
      df <- melt(df, id.vars = c('year', 'country', 'referenceid_list', 'indicator_short_name'))
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      
      # only keep data with no NAs
      df <- df[complete.cases(df),]
      
      # get color graident 
      col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
      col_vec <- col_vec[-1]
      
      # make plot title 
      plot_title = paste0('Quintile Dot Plots for Indicators', ' - ', country_names)
      x_axis_text = paste0('Most recent value for', '\n', 'time period: ', date_range[1], ' - ', date_range[2])
      
      # subset by y axis
      df <- df %>% 
        filter(value >= value_range[1],
               value <= value_range[2])
      # # # text for plot
      mytext <- paste(
        "Value: ", round(df$value, digits = 3), "\n",
        "Year: ", as.character(df$year),"\n",
        "Country: ", as.character(df$country),"\n",
        "Data source: ", as.character(df$referenceid_list),
        sep="") %>%
        lapply(htmltools::HTML)
      
      # if the dataframe is null of empty make plot null
      if(is.null(df) | nrow(df) == 0){
        NULL
      } else {
        
        # p <- plot_ly(data = df, 
        #              x = ~value, 
        #              y = ~indicator_short_name, 
        #              color = ~variable, 
        #              colors = col_vec, text = mytext, 
        #              hoverinfo = 'text') %>%
        #   layout(title = plot_title,
        #          xaxis= list(title = 'Value', showticklabels = TRUE),
        #          yaxis= list(title = '', showticklabels = TRUE))
        
        # # plot
        # print(ggplotly(ggplot(df, aes(value, indicator_short_name,group = indicator_short_name, color = variable, text = mytext)) +
        #   geom_point(size = 2.5, alpha = 0.8) +
        #   geom_line(size = 1.5, alpha = 1, color = 'grey') +
        #   scale_color_manual(name = 'Quintiles',
        #                      values = col_vec) +
        #   labs(x = 'Most recent value (before selected year)',
        #        y = '',
        #        title = plot_title) +
        #   hefpi::theme_gdocs(), tooltip = 'text'))
        
        print(ggplotly(ggplot(df, aes(x=indicator_short_name,
                                      y=value,
                                      text = mytext)) +
                         geom_point(size=5, aes(color = variable), alpha = 0.7) +
                         geom_line(aes(group = indicator_short_name)) +
                         scale_color_manual(name = '',
                                            values = col_vec) +
                         scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(value_range[1], value_range[2])) +
                         labs(title=plot_title, x = x_axis_text, y = '') +
                         coord_flip() +
                         hefpi::theme_gdocs() +
                         theme(axis.text = element_text(size=9, face = 'bold')), tooltip = 'text'))
        
      }
      
    }
   
  })
}

## To be copied in the UI
# mod_dots_country_ui("dots_country1")
# mod_dots_country_ui("dots_ind1")



## To be copied in the server
# callModule(mod_dots_country_server, 'dots_country1')
# callModule(mod_dots_country_server, 'dots_ind1')


