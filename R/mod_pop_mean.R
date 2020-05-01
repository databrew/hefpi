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
                       choices = region_list,
                       selected = region_list[[1]]),
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
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_pop_mean_server <- function(input, output, session){
 
    output$country_ui <- renderUI({
      indicator <- input$indicator
      region <- input$region
      yn <- input$interpolate
        
        df <- hefpi::df_series
        df <- df[df$indicator_name == indicator,]
        df <- df[df$region == region,]
        #remove countries with all NA in year columns
        start_index <- which(names(df) == 2001)
        end_index <- which(names(df) == 2015)
        
        good_index <- which(rowSums(is.na(df[, start_index:end_index])) != ncol(df[, start_index:end_index]))
        df <- df[good_index,]
        countries <- unique(df$country_name)
        selectInput(session$ns("country"), 
                    label = 'Country', 
                    choices = countries,
                    multiple = TRUE, selected = countries)
      
    })
    
    output$pop_mean <- renderPlot({
        indicator <- input$indicator
        region <- input$region
        yn <- input$interpolate
        country_names <- input$country
        message('the country_names are', country_names)
        # save(country_names, file = 'cnames2.rda')
        # Get the data to be plotted
        df <- hefpi::df_series
        pd <- df[df$indicator_name == indicator,]
        pd <- pd[pd$region == region,]
        pd <- pd[pd$country_name %in% country_names,]
        #remove countries with all NA in year columns
        start_index <- which(names(pd) == 2001)
        end_index <- which(names(pd) == 2015)
        
        good_index <- which(rowSums(is.na(pd[, start_index:end_index])) != ncol(pd[, start_index:end_index]))
        pd <- pd[good_index,]
        
        # put in long format for plotting
        pd <- pd %>% select(country_name, indicator_name, `2001`:`2015`)
        # pd[,3:ncol(pd)] <- apply(pd[, 3:ncol(pd)], 2, function(x) as.numeric(as.character(x)))
        pd <- reshape2::melt(as.data.frame(pd), id.vars = c('country_name', 'indicator_name'))
        pd$variable <- as.character(pd$variable)
        # save(pd, file = 'pd.rda')
        p <- ggplot(pd, aes(variable, value, color = country_name)) + geom_point()
        
        p
    })
}



## To be copied in the UI
# mod_leaflet_ui("pop_mean1")

## To be copied in the server
# callModule(mod_pop_mean_server, 'pop_mean1')
