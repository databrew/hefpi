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
                 tags$div(style='overflow-y: scroll; position: relative', plotlyOutput(ns('dots_country'), height = '600px', width = '1000px') )
      ),
      column(4,
             pickerInput(inputId = ns("indicator"),
                         label = 'Indicator', 
                         choices = indicators_list,
                         selected = "4+ antenatal care visits",
                         options = list(`style` = "btn-primary")),
             pickerInput(inputId = ns("region"),
                         label = 'Region', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region),
                         options = list( `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} Regions",
                                         `style` = "btn-primary",
                                         `actions-box`=TRUE),
                         multiple = TRUE),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('date_range'),
                         'Date range',
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
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
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

# HERE IMPLEMENT CORRECT DATA DOWNLOAD FOR DOT PLOTS AND THEN DATA AVAILABILITY
mod_dots_country_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintiles - Most recent value by country", 
               text = "This chart enables users to compare inequalities in HEFPI indicators by household wealth, both within and across countries. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. For a set of countries and an indicator the user specifies, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  output$ui_outputs <- renderUI({
    indicator <- "BMI, adults"
    region <-as.character(region_list$region)
    date_range = c(1982,2016)
    # get inputs
    indicator <- input$indicator
    region <- input$region
    date_range <- input$date_range
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    
    # Get the variable
    variable <- indicators %>%
      dplyr::filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode %in% region_code,]
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
                    options = list( `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} countries",
                                    `style` = "btn-primary",
                                    `actions-box`=TRUE),
                    multiple = TRUE),
        sliderInput(session$ns('value_range'),
                    'X axis range',
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = '')
      )
      
    )
    
  })
  
  get_dot_data <- reactive({
    # last_date <- '2018'
    indicator <- "Inpatient care use, adults"
    region <- "Latin America & Caribbean"
    temp <- hefpi::df_series %>% filter(region == 'Latin America & Caribbean')
    country_names <- unique(temp$country_name)
    value_range = c(0,28)
    date_range = c(1982,2018)
    last_date <- input$last_date
    region <- input$region
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    date_range <- input$date_range
    if(is.null(value_range)){
      NULL
    } else {
      dot_list <- list()
      
      # Get the variable
      ind_info <- indicators %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
      # subset by country and variable
      temp <- hefpi::df %>%
        filter(country %in% country_names) %>%
        filter(indic == variable_name) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) 
      
      # get year and keep only necessary columns
      df <- temp %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) 
      
      # made data long form
      id_vars <- names(df)[!grepl('Q', names(df))]
      df <- melt(df, id.vars = id_vars)
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      
      # only keep data with no NAs
      df <- df[complete.cases(df),]
      
      # order country
      df$country <- factor(df$country,levels= sort(unique(df$country), decreasing = TRUE ))
      
      # get color graident 
      col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
      col_vec <- col_vec[-1]
      
      # make plot title 
      plot_title = paste0('Quintiles - Most recent value by country', ' - ', indicator)
      sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
      y_axis_text = paste0(indicator, ' (', unit_of_measure, ')')
      

      if(unit_of_measure == '%'){
        df$value <- df$value*100
        value_range[2] <- value_range[2]*100
        value_range[1] <- value_range[1]*100
        
      }
      # # # text for plot
      mytext <- paste(
        "Value: ", paste0(round(df$value, digits = 2), ' (', unit_of_measure, ')'), "\n",
        "Year: ", as.character(df$year),"\n",
        "Indicator: ", as.character(indicator),"\n",
        "Data source: ", as.character(df$referenceid_list),
        sep="") %>%
        lapply(htmltools::HTML)
      
      

        
        # number of countries
        plot_height <- ceiling(((length(unique(df$country))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        
        p <- ggplot(df, aes(x=country,
                            y=value,
                            text = mytext)) +
          geom_point(size=5, alpha = 0.7, aes(color = variable)) +
          geom_line(aes(group = country)) +
          scale_color_manual(name = '',
                             values = col_vec) +
          scale_y_continuous(limits = c(value_range[1], value_range[2]), expand = c(0,0)) +
          labs(title=plot_title,
               subtitle = sub_title, x= '', y = y_axis_text) +
          coord_flip() 
       
        
        dot_list[[1]] <- p
        dot_list[[2]] <- df
        dot_list[[3]] <- list(plot_title, sub_title, col_vec, mytext, plot_height)
        return(dot_list)
      
      
      
      
    }
    
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("quintile_country_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      dot_list <- get_dot_data()
      
      if(is.null(dot_list)){
        NULL
      } else {
        df <- dot_list[[2]]
        temp <- df
        names(temp) <- tolower(names(temp))
        names(temp)[names(temp)=='variable'] <- 'level'
        # subset by  
        temp$parameter <- 'Mean'
        # temp$level <- 'National'
        temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, survey_list, indic, indicator_short_name,
                                indicator_description, parameter, level, ci, unit_of_measure)
        names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                         'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                         'Value', 'Unit_of_measurement')
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("quintile_country_", Sys.Date(),".png"),
                                    content = function(file) {
                                      
                                      dot_list <- get_dot_data()
                                     
                                      if(is.null(dot_list)){
                                        NULL
                                      } else {
                                        p <- dot_list[[1]]
                                        p <- p + hefpi::theme_hefpi() 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                      
                                      
                                    })
  
  output$dots_country <- renderPlotly({
    dot_list <- get_dot_data()
    if(is.null(dot_list)){
      NULL
    } else {
      df <- dot_list[[2]]
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
        fig <- empty_plot("No data available for the selected inputs")
        
      } else {
        # df <- dot_list[[2]]
        # plot_title <- dot_list[[3]][[1]]
        # sub_title <- dot_list[[3]][[2]]
        # col_vec <- dot_list[[3]][[3]]
        # mytext <- dot_list[[3]][[4]]
        p <- dot_list[[1]]
        plot_height <- dot_list[[3]][[5]]
        p <- p + hefpi::theme_hefpi(grid_major_x = NA)
        fig <- ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          config(displayModeBar = F)
        fig
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
             tags$div(style='overflow-y: scroll; position: relative', 
                      plotlyOutput(ns('dots_ind'), height = '600px', width = '1000px') )),
      column(4,
             tags$style("#indicator {font-size:10px;height:10px;}"),
             pickerInput(inputId = ns("indicator"),
                         label = 'Indicator', 
                         choices = indicators_list,
                         selected = indicators$indicator_short_name,
                         options = list( `actions-box`=TRUE,
                                         `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} indicators",
                                         `style` = "btn-primary"),
                         multiple = TRUE),
             pickerInput(ns('country'), 'Country',
                         choices = as.character(country_list),
                         selected = 'United States',
                         options = list(`style` = "btn-primary")),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('date_range'),
                         'Date range',
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
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
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
#' @import plotly
#' @import htmltools
#' @keywords internal

# HERE NEED TO HAVE UNIT OF MEASURE IN HOVER OVER FOR EACH INDICATOR (JOIN TO DATA?). ANYTHING WITH PERCENT SHOULD BE MULTIPLIED BY 100
mod_dots_ind_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintiles - Most recent value by indicator", 
               text = "This chart allows users to explore differences in inequalities by household wealth across HEFPI indicators within a country. For instance, the chart reveals if a country achieves universal coverage of maternal and child health services while failing to enable equitable access to inpatient care. For every HEFPI indicators and country the user selects, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  output$ui_outputs <- renderUI({
    date_range = c(1982,2016)
    indicator <- indicators_list[[1]]
    country_names <- 'United States'
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name %in% indicator) %>%
      .$variable_name
    
    # subset by country and variable
    temp <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic %in% variable) %>%
      filter(year >= date_range[1],
             year <= date_range[2]) 

    # get year and keep only necessary columns
    df <- temp %>%
      group_by(indicator_short_name) %>%
      arrange(desc(year)) %>%
      dplyr::filter(year == dplyr::first(year)) 
    
    # made data long form
    id_vars <- names(df)[!grepl('Q', names(df))]
    df <- melt(df, id.vars = id_vars)
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
                value = c(min_value, max_value),
                sep = '')
    
  })
  
  
  get_dot_data <- reactive({
    date_range = c(1982,2016)
    indicator <- c('BMI, adults', 'BMI, men', 'BMI, women', 'Catastrophic health spending, 10%',
                   'Catastrophic health spending, 25%', 'Height, adults', 'Height, men', 'Height, women')
    country_names <- 'United States'
    value_range <- c(0,30)
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    if(is.null(value_range)){
      NULL
    } else {
      
      dot_list <- list()
      # Get the variable
      ind_info <- indicators %>%
        dplyr::filter(indicator_short_name %in% indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
     
      
      # subset by country and variable
      temp <- hefpi::df %>%
        filter(country == country_names) %>%
        filter(indic %in% variable_name) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) 
      
      # get year and keep only necessary columns
      df <- temp %>%
        group_by(indicator_short_name) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) 
      
      # made data long form
      id_vars <- names(df)[!grepl('Q', names(df))]
      df <- melt(df, id.vars = id_vars)
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
      plot_title = paste0('Quintiles - Most recent value by indicator', ' - ', country_names)
      sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
      y_axis_text = paste0(indicator, ' (', unit_of_measure, ')')
      
      # make percent 
      df$value[df$unit_of_measure == '%'] <-    (df$value[df$unit_of_measure == '%'])*100
      
      # order indicator alphabetically
      df$indicator_short_name <- factor(df$indicator_short_name,levels= sort(unique(df$indicator_short_name), decreasing = TRUE ))
      
      mytext <- paste(
        "Value: ", paste0(round(df$value, digits = 2), ' (', df$unit_of_measure, ')'), "\n",
        "Year: ", as.character(df$year),"\n",
        "Country: ", as.character(df$country),"\n",
        "Data source: ", as.character(df$referenceid_list),
        sep="") %>%
        lapply(htmltools::HTML)
      
      # if the dataframe is null of empty make plot null
     
      
        # number of countries
        plot_height <- ceiling(((length(unique(df$indicator_short_name))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        
        p <- ggplot(df, aes(x=indicator_short_name,
                            y=value,
                            text = mytext)) +
          geom_point(size=5, alpha = 0.7, aes(color = variable)) +
          geom_line(aes(group = indicator_short_name)) +
          scale_color_manual(name = '',
                             values = col_vec) +
          scale_y_continuous(limits = c(value_range[1], value_range[2]), expand = c(0,0)) +
          labs(title=plot_title, x= '', y = '',
               subtitle = sub_title) +
          coord_flip() 
       
        
        dot_list[[1]] <- p
        dot_list[[2]] <- df
        dot_list[[3]] <- list(plot_title, sub_title, col_vec, mytext, plot_height)
        # save(dot_list, file = 'dot_list.rda')
        return(dot_list)
      
      
    }
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("quintile_indicator_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      dot_list <- get_dot_data()
      
      if(is.null(dot_list)){
        NULL
      } else {
        df <- dot_list[[2]]
        temp <- df
        names(temp) <- tolower(names(temp))
        names(temp)[names(temp)=='variable'] <- 'level'
        # subset by  
        temp$parameter <- 'Mean'
        # temp$level <- 'National'
        temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, survey_list, indic, indicator_short_name,
                                indicator_description, parameter, level, ci, unit_of_measure)
        names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 'Survey_name', 
                         'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                         'Value', 'Unit_of_measurement')
        write.csv(df, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("quintile_dot_plots_indicator_", Sys.Date(),".png"),
                                    content = function(file) {
                                      
                                      dot_list <- get_dot_data()
                                      
                                      if(is.null(dot_list)){
                                        NULL
                                      } else {
                                        p <- dot_list[[1]]
                                        
                                        p =  p + theme(axis.text = element_text(size = rel(1/2))) 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                      
                                      
                                    })
  
    
  output$dots_ind <- renderPlotly({
    dot_list <- get_dot_data()
    if(is.null(dot_list)){
      NULL
    } else {
      df <- dot_list[[2]]
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
        fig <- empty_plot("No data available for the selected inputs")
        
      } else {
        # df <- dot_list[[2]]
        # plot_title <- dot_list[[3]][[1]]
        # sub_title <- dot_list[[3]][[2]]
        # col_vec <- dot_list[[3]][[3]]
        # mytext <- dot_list[[3]][[4]]
        p <- dot_list[[1]]
        plot_height <- dot_list[[3]][[5]]
        p <- p + hefpi::theme_hefpi(grid_major_x = NA) 
        fig <- ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          config(displayModeBar = F)
        fig
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


