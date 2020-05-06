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
             plotOutput(
               ns('dots_country')
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list[[1]]),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region[1])),
             uiOutput(ns('country_ui')),
             selectInput(ns('last_date'), 'First available year before',
                         choices =year_list,
                         selected = year_list[length(year_list)]))
    )
  )
}

# Module Server
#' @rdname mod_dots_country_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dots_country_server <- function(input, output, session){
  
  output$country_ui <- renderUI({
    
    
    # get inputs
    indicator <- input$indicator
    region <- input$region

    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode == region_code,]
    # For now, below is not needed, but will keep it in comments
    # start_index <- which(names(df) == 2001)
    # end_index <- which(names(df) == 2015)
    # good_index <- which(rowSums(is.na(df[, start_index:end_index])) != ncol(df[, start_index:end_index]))
    # df <- df[good_index,]
    # create select input
    countries <- unique(df$country)
    selectInput(session$ns("country"), 
                label = 'Country', 
                choices = countries,
                multiple = TRUE, selected = countries)
    
  })
  
  output$dots_country <- renderPlot({
    last_date <- '2017'
    indicator <- "Catastrophic health spending, 10%"
    region <- "Latin America & Caribbean"
    temp <- hefpi::df_series %>% filter(region == 'Latin America & Caribbean')
    country_names <- unique(temp$country_name)
    last_date <- input$last_date
    region <- input$region
    indicator <- input$indicator
    country_names <- input$country
    if(is.null(country_names)){
      NULL
    } else {
      # Get the variable
      variable <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        .$variable_name
      
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country %in% country_names) %>%
        filter(indic == variable) 
      
      # get last available year before last_date
      year_last <- as.character(as.numeric(last_date) - 1)
      
      # get year and keep only necessary columns
      df <- df %>%
        filter(year <= year_last) %>%
        select(year, country, Q1:Q5) 
      
      # made data long form
      df <- melt(df, id.vars = c('year', 'country'))
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
      plot_title = paste0('Quintile Dot Plots for Economies', ' - ', indicator, ', ', year_last)
      
      # plot
      p<-   ggplot(df, aes(value, country, group = country, color = variable)) + 
        geom_point(size = 2.5, alpha = 0.8) +
        geom_line(size = 2.5, alpha = 0.8) +
        scale_color_manual(name = 'Quintiles',
                           values = col_vec) +
        labs(x = 'Most recent value (before selected year)',
             y = '',
             title = plot_title) +
        hefpi::theme_gdocs()
      
      return(p)
      
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
             plotOutput(
               ns('dots_ind')
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = c('BMI, adults', 'BMI', 'men', 'BMI, women', 'Catastrophic health spending, 10%', 
                                      'Catastrophic health spending, 25%', 'Height, adults', 'Height', 'men', 'Height, women'),
                         multiple = TRUE),
             selectInput(ns('country'), 'Country',
                         choices = as.character(country_list),
                         selected = 'United States'),
             selectInput(ns('last_date'), 'First available year before',
                         choices =year_list,
                         selected = year_list[length(year_list)]))
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
#' @import htmltools
#' @keywords internal

mod_dots_ind_server <- function(input, output, session){
  
  
  
  output$dots_ind <- renderPlot({
    last_date <- '2017'
    indicator <- c('BMI, adults', 'BMI, men', 'BMI, women', 'Catastrophic health spending, 10%', 
                   'Catastrophic health spending, 25%', 'Height, adults', 'Height, men', 'Height, women')
    country_names <- 'United States'
    last_date <- input$last_date
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
        left_join(indicators, by = c('indic' = 'variable_name'))
      
      # get last available year before last_date
      year_last <- as.character(as.numeric(last_date) - 1)
      
      
      # get year and keep only necessary columns
      df <- df %>%
        filter(year <= year_last) %>%
        select(year,indicator_short_name, Q1:Q5) 
      
      # made data long form
      df <- melt(df, id.vars = c('year', 'indicator_short_name'))
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
      plot_title = paste0('Quintile Dot Plots for Indicators', ' - ', country_names, ', ', year_last)
      
      # plot
      p<-   ggplot(df, aes(value, indicator_short_name,group = indicator_short_name, color = variable)) + 
        geom_point(size = 2.5, alpha = 0.8) +
        geom_line(size = 1.5, alpha = 1) +
        scale_color_manual(name = 'Quintiles',
                           values = col_vec) +
        labs(x = 'Most recent value (before selected year)',
             y = '',
             title = plot_title) +
        hefpi::theme_gdocs()
      
      return(p)
      
    
    
  })
}

## To be copied in the UI
# mod_dots_country_ui("dots_country1")
# mod_dots_country_ui("dots_ind1")



## To be copied in the server
# callModule(mod_dots_country_server, 'dots_country1')
# callModule(mod_dots_country_server, 'dots_ind1')



