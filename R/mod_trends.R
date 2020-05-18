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
           plotlyOutput(
             ns('trends_mean'), height = '800px'
           )),
    column(4,
           selectInput(ns('indicator'), 'Indicator',
                       choices = indicators_list,
                       selected = '4+ antenatal care visits'),
          checkboxInput(ns('interpolate'), 'Interpolate missing values',
                       value = TRUE),
           selectInput(ns('region'), 'Region',
                       choices = as.character(region_list$region),
                       selected = 'East Asia & Pacific'),
           uiOutput(ns('country_ui')),
          sliderInput(ns('date_range'),
                                  'Date range',
                                  min = 1982,
                                  max = 2017,
                                  value = c(1982, 2017),
                                  step = 1,
                                  sep = ''),
          sliderInput(ns('value_range'),
                      'Y axis range',
                      min = 0,
                      max = 1,
                      value = c(0, 1),
                      step = 0.05,
                      sep = ''),
          useShinyalert(),  # Set up shinyalert
          actionButton(ns("plot_info"), "Plot Info"))
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
 
    output$country_ui <- renderUI({
      
      
      # Observe changes to inputs in order to generate changes to the map
      observeEvent(input$plot_info, {
        # Show a modal when the button is pressed
        shinyalert(title = "Trends - Population mean", 
                   text = "charts allow tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
                   type = "info", 
                   closeOnClickOutside = TRUE, 
                   showCancelButton = FALSE, 
                   showConfirmButton = FALSE)
      })
      
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
       
       
      
        countries <- unique(df$country)
        selectInput(session$ns("country"), 
                    label = 'Country', 
                    choices = countries,
                    multiple = TRUE, selected = countries)
      
    })
    
    output$trends_mean <- renderPlotly({
      # indicator <- "Catastrophic health spending, 10%"
      # region <- "Latin America & Caribbean"
      # temp <- hefpi::df_series %>% filter(region == 'Latin America & Caribbean')
      # country_names <- unique(temp$country_name)
      # date_range <- c(1982, 2017)
      # value_range <- c(0,1)
      # get inputs
      indicator <- input$indicator
      region <- input$region
      yn <- input$interpolate
      country_names <- input$country
      date_range <- input$date_range
      value_range <- input$value_range
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
        pd <- pd %>% filter(year >= min(date_range),
                            year <= max(date_range)) 
        pd <- pd %>% filter(pop >= min(value_range),
                            pop <= max(value_range))
        
        # get title and subtitle
        plot_title <- paste0('Trend - population mean')
        sub_title <- indicator
        
        # text for plot
        mytext <- paste(
          "Country: ", as.character(pd$country),"\n", 
          "Population: ", round(pd$pop, digits = 3), "\n",
          "Year: ", as.character(pd$year),"\n",
          "Data source: ", as.character(pd$referenceid_list), "\n",
          sep="") %>%
          lapply(htmltools::HTML)
        # condition if we connect the dots
        if(yn){
          p <- plot_ly(data = pd, x = ~year, y = ~pop, color = ~country, 
                       text = mytext, hoverinfo = 'text') %>%
            add_trace(x = ~year, y = ~pop, color = ~country, mode = 'lines+markers') 
          
         
        } else {
          p <- plot_ly(data = pd, x = ~year, y = ~pop, color = ~country,
                       text = mytext, hoverinfo = 'text')
          
        }
       p <- p %>%
         layout(title = plot_title,
                xaxis= list(title = 'Year', showticklabels = TRUE),
                yaxis= list(title = 'Value', showticklabels = TRUE))
       p
      }
      
      
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
             plotlyOutput(
               ns('trends_con'),  height = '800px'
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = '4+ antenatal care visits'),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = 'East Asia and Pacific'),
             uiOutput(ns('country_ui')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             sliderInput(ns('value_range'),
                         'Y axis range',
                         min = -1,
                         max = 1,
                         value = c(-1, 1),
                         step = 0.05,
                         sep = ''),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
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
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Trends - Concentration Index", 
               text = "charts allow users to track the over-time dynamics in an indicator’s concentration index. tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # country ui
  output$country_ui <- renderUI({
   
    # get inputs
    indicator <- input$indicator
    region <- input$region
    yn <- input$interpolate
    country_names <- input$country
    
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
    
    
    countries <- unique(df$country)
    selectInput(session$ns("country"), 
                label = 'Country', 
                choices = countries,
                multiple = TRUE, selected = countries)
    
  })
  
  output$trends_con <- renderPlotly({
    # indicator <- "Catastrophic health spending, 10%"
    # region <- "Latin America & Caribbean"
    # temp <- hefpi::df_series %>% filter(region == 'Latin America & Caribbean')
    # country_names <- unique(temp$country_name)
    # date_range <- c(1982, 2017)
    # value_range <- c(-1,1)
    # get inputs
    indicator <- input$indicator
    region <- input$region
    yn <- input$interpolate
    country_names <- input$country
    date_range <- input$date_range
    value_range <- input$value_range
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
      pd <- pd %>% filter(year >= min(date_range),
                          year <= max(date_range)) 
      pd <- pd %>% filter(CI >= min(value_range),
                          CI <= max(value_range))
      
      # get title and subtitle
      plot_title <- paste0('Trend - Concentration index')
      sub_title <- indicator
      
      # text for plot
      mytext <- paste(
        "Country: ", as.character(pd$country),"\n", 
        "Concentration Index: ", round(pd$CI, digits = 3), "\n",
        "Year: ", as.character(pd$year),"\n",
        "Data source: ", as.character(pd$referenceid_list), "\n",
        sep="") %>%
        lapply(htmltools::HTML)
      # condition if we connect the dots
      if(yn){
        p <- plot_ly(data = pd, x = ~year, y = ~CI, color = ~country, 
                     text = mytext, hoverinfo = 'text') %>%
          add_trace(x = ~year, y = ~CI, color = ~country, mode = 'lines+markers') 
        
        
      } else {
        p <- plot_ly(data = pd, x = ~year, y = ~CI, color = ~country,
                     text = mytext, hoverinfo = 'text')
        
      }
      p <- p %>%
        layout(title = plot_title,
               xaxis= list(title = 'Year', showticklabels = TRUE),
               yaxis= list(title = 'Value', showticklabels = TRUE))
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
             plotlyOutput(
               ns('trends_quin'),  height = '800px'
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States'),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             selectInput(ns('first_date'), 'First available year after',
                         choices =year_list,
                         selected = year_list[1]),
             selectInput(ns('last_date'), 'First available year before',
                         choices =year_list,
                         selected = year_list[length(year_list)]),
             selectInput(ns('view_as'), 'View as',
                         choices =c('Slope chart', 'Line chart')),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
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
    shinyalert(title = "Trends - Quintile", 
               text = "charts show HEFPI health outcome and health service coverage indicator trends at the wealth quintile level, revealing if any inequalities have reduced, remained stable, or increased over time. Users can tailor the charts to their time period of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  output$trends_quin <- renderPlotly({
    # country_names <- 'United States'
    # indicator <- 'Catastrophic health spending, 10%'
    # first_date <- 1990
    # last_date <- 2017
    # view_as <- 'Slope chart'
    # 
    country_names <- input$country
    indicator <- input$indicator
    first_date <- input$first_date
    last_date <- input$last_date
    view_as <- input$view_as
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset by country and variable
    df <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic == variable) 
    
    # if the last_date is greater than the first date, print a message in the plot
    if(first_date >= last_date){
      p <- ggplot() + labs(title = paste0("The 'First Available Year After' input needs","\n" ,"to be later than the 'First Available Year Before' input"))
    } else {
      # get the first available year after first_date and first available year after last_date
      df_years <- sort(unique(df$year))
      first_index <- df_years > first_date
      year_one <- df_years[first_index][which(first_index)][1]
      last_index <- df_years < last_date
      year_last <- df_years[last_index][length(which(last_index))]
      
      if(view_as == 'Slope chart'){
        # filter to get year_one and year_last
        df <- df %>%
          filter(year == year_one | year == year_last) %>%
          select(year, Q1:Q5) 
        # save(df, file = 'df.rda')
      } else {
        df <- df %>%
          filter(year >=year_one | year <= year_last) %>%
          select(year, Q1:Q5) 
      }
     
    
    df <- melt(df, id.vars = 'year')
    
    # recode Quintiels
    df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                          ifelse(df$variable == 'Q2', 'Q2: Poor',
                                 ifelse(df$variable == 'Q3', 'Q3: Middle',
                                        ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
    
    # # get color graident 
    col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
    col_vec <- col_vec[-1]
    
    # make plot title
    plot_title = paste0('Quintile Trends - ', country_names, ' , ', indicator)
      
    # text for plot
    mytext <- paste(
      "Value: ", round(df$value, digits = 3), "\n",
      "Year: ", as.character(df$year),"\n",
      sep="") %>%
      lapply(htmltools::HTML)
    # condition if we connect the dots
    p <- plot_ly(data = df, x = ~year, y = ~value, color = ~variable, colors = col_vec,
                 text = mytext, hoverinfo = 'text') %>%
      add_trace(x = ~year, y = ~value, color = ~variable, colors = col_vec, mode = 'lines+markers') %>%
      layout(title = plot_title,
             xaxis= list(title = 'Year', showticklabels = TRUE),
             yaxis= list(title = 'Value', showticklabels = TRUE))
    
    
      
    }
   return(p)
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

