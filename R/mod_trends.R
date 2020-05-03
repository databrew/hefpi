# Module Trends 

#' @title   mod_trens.R
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
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_trends_mean_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  fluidPage(
    column(8,
           plotOutput(
             ns('trends_mean')
           )),
    column(4,
           selectInput(ns('indicator'), 'Indicator',
                       choices = indicators_list[[1]]),
          checkboxInput(ns('interpolate'), 'Interpolate missing values',
                       value = TRUE),
           selectInput(ns('region'), 'Region',
                       choices = as.character(region_list$region),
                       selected = as.character(region_list$region[1])),
           uiOutput(ns('country_ui')))
  )
  )
}

# Module Server
#' @rdname mod_trends_mean_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_mean_server <- function(input, output, session){
 
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
    
    output$trends_mean <- renderPlot({
      # indicator <- "Catastrophic health spending, 10%"
      # region <- "Latin America & Caribbean"
      # temp <- hefpi::df_series %>% filter(region == 'Latin America & Caribbean')
      # country_names <- unique(temp$country_name)
      # get inputs
      indicator <- input$indicator
      region <- input$region
      yn <- input$interpolate
      country_names <- input$country
      # get region code 
      region_list <- hefpi::region_list
      indicators <- hefpi::indicators
      df <- hefpi::df
      
      if(is.null(country_names)){
        NULL
      } else {
        
        region_code <- as.character(region_list$region_code[region_list$region == region])
        
        # get variable
        variable <- indicators %>%
          filter(indicator_short_name == indicator) %>%
          .$variable_name
        
        # subet by variable, region code and a list of countries
        
        df <- df[df$indic == variable,]
        df <- df[df$regioncode == region_code,]
        pd <- df[df$country %in% country_names,]
        
        # get title and subtitle
        plot_title <- paste0('Trend - population mean')
        sub_title <- indicator
        
        # condition if we connect the dots
        if(yn){
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
      column(8,
             plotOutput(
               ns('trends_quin')
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list[[1]]),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list[[1]]),
             selectInput(ns('first_date'), 'First available year after',
                         choices =year_list,
                         selected = year_list[1]),
             selectInput(ns('last_date'), 'First available year before',
                         choices =year_list,
                         selected = year_list[length(year_list)]),
             selectInput(ns('view_as'), 'View as',
                         choices =c('Slope chart', 'Line chart')))
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
  
  
  output$trends_quin <- renderPlot({
    ggplot() + labs(title = 'In progress')
  })
    
  
}


#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_con_ui
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
      column(8,
             plotOutput(
               ns('trends_con')
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list[[1]]),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region[1])),
             uiOutput(ns('country_ui')))
    )
  )
}

# Module Server
#' @rdname mod_trends_cib_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_con_server <- function(input, output, session){
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
  
  output$trends_con <- renderPlot({
    ggplot() + labs(title = 'In progress')
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

