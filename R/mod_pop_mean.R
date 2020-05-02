# Module population mean UI

#' @title   mod_pop_mean_ui and mod_pop_mean_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_pop_mean
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_pop_mean_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  fluidPage(
    column(8,
           plotOutput(
             ns('pop_mean')
           )),
    column(4,
           selectInput(ns('indicator'), 'Indicator',
                       choices = indicators_list[[1]]),
           selectInput(ns('interpolate'), 'Interpolate missing values',
                       choices = yn_list),
           selectInput(ns('region'), 'Region',
                       choices = as.character(region_list$region),
                       selected = as.character(region_list$region[1])),
           uiOutput(ns('country_ui')))
  )
  )
}

# Module Server
#' @rdname mod_pop_mean
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_pop_mean_server <- function(input, output, session){
 
    output$country_ui <- renderUI({
      
      # get inputs
      indicator <- input$indicator
      region <- input$region
      yn <- input$interpolate
      
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
    
    output$pop_mean <- renderPlot({
      # get inputs
      indicator <- input$indicator
      region <- input$region
      yn <- input$interpolate
      country_names <- input$country
      
      # get region code 
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region == region])
     
      # get variable
      variable <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        .$variable_name
      
      # subet by variable, region code and a list of countries
      df <- hefpi::df
      df <- df[df$indic == variable,]
      df <- df[df$regioncode == region_code,]
      pd <- df[df$country %in% country_names,]
        
      # get title and subtitle
      plot_title <- paste0('Trend - population mean')
      sub_title <- indicator
      
      # condition if we connect the dots
      if(yn == 'Yes'){
        p <- ggplot(pd, aes(year, pop, color = country)) + geom_point(size = 2, alpha = 0.8) +
          geom_line(aes(group = country), size = 1.5, alpha = 0.8) +
          labs(x = 'Year',
               y = 'Population mean',
               title = plot_title,
               subtitle= indicator) +
          scale_color_gdocs(name = 'Country') +
          theme_gdocs()
      } else {
        p <- ggplot(pd, aes(year, pop, color = country)) + geom_point(size = 2, alpha = 0.8) +
          labs(x = 'Year',
               y = 'Population mean',
               title = plot_title,
               subtitle = indicator) +
          scale_color_gdocs(name = 'Country') +
          theme_gdocs()
      }
      p
    })
}



## To be copied in the UI
# mod_leaflet_ui("pop_mean1")

## To be copied in the server
# callModule(mod_pop_mean_server, 'pop_mean1')
