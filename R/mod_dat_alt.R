# Module data availability alternate

#' @title   mod_dat_alt.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dat_country_alt_ui
#'
#' @keywords internal
#' @export 
#' @import plotly
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_country_alt_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('dat_country'), height = '900px', width = '1000px',
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States'),
             uiOutput(ns('indicator_ui')),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_dat_country_alt_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import tidyr
#' @import ggthemes
#' @import scales
#' @import plotly
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dat_country_alt_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Data availability by Country", 
               text = "charts zoom in on the general data availability situation for a specific country. They allow users to explore, for instance, if data are frequently available for maternal and child health service coverage, while being largely missing for catastrophic healthcare spending. The charts’ vertical axis sorts all indicators in the database by the three HEFPI domains: health outcomes, service coverage, and financial protection. The horizontal axis represents time. Years for which data are available for an indicator are marked by colored squares in the chart area. Hence, larger colored chart areas represent better data availability for the user’s country of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  output$indicator_ui <- renderUI({
    # get country input default
    # country_name = 'United States'
    country_name <- input$country
    
    # subset data by country to get available level2 indicators
    country_data<- hefpi::df %>%
      filter(country == country_name) %>%
      inner_join(indicators, by = c('indic' = 'variable_name')) 
      
    class_names <- names(indicators_list)[names(indicators_list) %in% unique(country_data$bin)]
    
   
    selectInput(session$ns("ind_class"), 
                label = 'Indicator class', 
                choices = class_names,
                selected = 'Healthcare Coverage')
    
  })
  
  output$dat_country <- renderPlotly({
    

    ind_class <- input$ind_class
    if(is.null(ind_class)){
      NULL
    } else {
      # country_name = 'United States'
      # ind_class <- 'Financial Protection'
      country_name <- input$country
      ind_class <- input$ind_class
      
      # subset data by country and join to get indicator short name 
      df<- hefpi::df %>%
        filter(country == country_name) %>%
        filter(bin == ind_class) %>%
        left_join(indicators, by = c('indic' = 'variable_name')) %>%
        select(country, year, indicator_short_name, referenceid_list)
      
      # get color graident 
      col_vec <- c(brewer.pal(name = 'Accent', n = length(unique(df$indicator_short_name))),
                   brewer.pal(name = 'Accent', n = length(unique(df$indicator_short_name))))

      
      # make plot title 
      plot_title = paste0('Missing data profile', ' - ', country_name)
      
      mytext <- paste(
        "Year: ", as.character(unique(df$year)),"\n",
        "Indicator: ", as.character(unique(df$indicator_short_name)),"\n",
        "Country: ", as.character(unique(df$country)), "\n",
        "Data source: ", as.character(df$referenceid_list),
        sep="") %>%
        lapply(htmltools::HTML)
      
      p<-   ggplot(df, aes(as.character(year), indicator_short_name, fill = indicator_short_name, text = mytext)) + 
        geom_tile(size = 2.5, alpha = 0.8) +
        scale_fill_manual(name = 'Indicator class',
                          values = col_vec) +
        labs(x = 'Year',
             y = '',
             title = plot_title) +
        hefpi::theme_gdocs() +
        theme(legend.position = 'none',
              axis.text.y = element_text(face = "plain", size = rel(8/12)),
              axis.text.x = element_text(face = "plain", size = rel(5/12), angle = 45, hjust = 1)) +
      
        coord_flip()
      ggplotly(p, tooltip = 'mytext')
     
    }
   
  })
  
  
}

#-------------------------------------------------------------------------------------------------------------

#' @rdname mod_dat_ind_alt_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_ind_alt_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotOutput(
               ns('dat_ind'), height = '900px'
             )),
      column(4,
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         multiple = TRUE, 
                         selected = as.character(region_list$region)),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_dat_ind_alt_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dat_ind_alt_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Data availability by Indicator", 
               text = "charts allow user to compare data availability for an indicator across countries, regions, and over time. The units on the chart’s vertical axis represent countries (sorted by regions), and the chart’s horizontal axis represents time. Years for which data are available for a country are marked by colored squares in the chart area. Hence, larger colored chart areas represent better data availability for the user’s indicator of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  
  output$dat_ind <- renderPlot({
    
    indicator <- input$indicator
    region <- input$region
    
    region <- region_list$region
    indicator <- 'Inpatient care use, adults'
    # 
    # create a region year country data
    country_data <- hefpi::df %>% select(year, country, regioncode) 
    all_years <- sort(unique(country_data$year))
    all_countries <- sort(unique(country_data$country))
    regions_country <- sort(unique(paste0(country_data$regioncode, '_',country_data$country)))
    
    temp_data <- expand_grid(year = all_years, regions_country = regions_country) 
    temp_data$region_code <- unlist(lapply(strsplit(temp_data$regions_country, '_'), function(x) x[1]))
    temp_data$country <- unlist(lapply(strsplit(temp_data$regions_country, '_'), function(x) x[2]))
    temp_data$regions_country <- NULL
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code - HERE need to get level_2 for plot
    df<- hefpi::df %>%
      filter(indic == variable) %>%
      filter(regioncode %in% region_code) %>%
      select(year,country, regioncode) %>%
      right_join(temp_data) %>%
      mutate(region_name = recode_factor(regioncode, EAS = 'East Asia & Pacific',
                                         ECS = 'Europe & Central Asia',
                                         LCN = 'Latin America & Caribbean',
                                         MEA = 'Middle East & North Africa',
                                         NAC = 'North America',
                                         SAS = 'South Asia',
                                         SSF = 'Sub-Saharan Africa')) %>%
      select(-regioncode)
    
    # make characters
    df$year <- as.character(df$year)
    df$region_name <- as.character(df$region_name)
    
    df <- df %>% dplyr::arrange(desc(year), region_code, country)
    df$country <- factor(df$country, levels = unique(df$country))
    
    
    y_df <- df %>% arrange(desc(year), region_name, country) %>%
      dplyr::distinct(region_name, country) %>%
      filter(!is.na(region_name)) %>% 
      arrange(region_name, country) %>%
      group_by(region_name) %>%
      dplyr::filter(country == dplyr::first(country))
    
    # get color graident 
    col_vec <- brewer.pal(name = 'Accent', n = length(unique(df$region_name)))
    col_vec[1] <- 'lightgrey'
    
    # make plot title 
    plot_title = paste0('Missing data profile', ' - ', indicator)
    
    
    p<-   ggplot(df, aes(year, country, fill =region_name)) + 
      geom_tile(size = 2.5, alpha = 0.8) +
      scale_fill_manual(name = 'Region',
                        values = sort(col_vec),
                        breaks = c(NA,
                                   'East Asia & Pacific',
                                   'Europe & Central Asia',
                                   'Latin America & Caribbean',
                                   'Middle East & North Africa',
                                   'North America',
                                   'South Asia',
                                   'Sub-Saharan Africa'),
                        labels = c('Missing Data',
                                   'East Asia & Pacific',
                                   'Europe & Central Asia',
                                   'Latin America & Caribbean',
                                   'Middle East & North Africa',
                                   'North America',
                                   'South Asia',
                                   'Sub-Saharan Africa')) +
      labs(x = 'Year',
           y = '',
           title = plot_title) +
      scale_y_discrete(breaks = y_df$country,
                       labels = y_df$region_name,
                       limits = rev(levels(df$country))) +
      hefpi::theme_gdocs() +
      theme(axis.text.y = element_text(face = "plain", size = rel(10/12)),
            axis.text.x = element_text(face = "plain", size = rel(8/12), angle = 45, hjust = 1))
    
    return(p)
  })
}

## To be copied in the UI
# mod_dat_country_alt_ui("dat_country1")
# mod_dat_country_alt_ui("dat_ind1")


## To be copied in the server
# callModule(mod_dat_country_alt_server, 'dat_country1')
# callModule(mod_dat_ind_alt_server, 'dat_ind1')




