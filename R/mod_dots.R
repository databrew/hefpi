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
    ggplot() + labs(title = 'In progress')
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
                         choices = indicators_list[[1]]),
             selectInput(ns('country'), 'Country',
                         choices = as.character(country_list),
                         selected = as.character(country_list[1])),
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
    ggplot() + labs(title = 'In progress')
  })
}

## To be copied in the UI
# mod_dots_country_ui("dots_country1")
# mod_dots_country_ui("dots_ind1")



## To be copied in the server
# callModule(mod_dots_country_server, 'dots_country1')
# callModule(mod_dots_country_server, 'dots_ind1')



