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
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_country_alt_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotOutput(
               ns('dat_country'), height = '900px', width = '1000px',
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States'),
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
  
  
  output$dat_country <- renderPlot({
    country_name <- input$country
    
    # get all unique years and indicators
    temp <- hefpi::df
    all_years <- sort(unique(temp$year))
    all_ind <- sort(unname(unlist(indicators_list)))
    
    # subset data by country and join to get indicator short name 
    country_data<- hefpi::df %>%
      left_join(indicators, by = c('indic' = 'variable_name')) %>%
      filter(country == country_name) 
    
    # create data frame with year and indicator combinations
    df <- tidyr::expand_grid(year = all_years, indicator_short_name = all_ind) %>%
      left_join(country_data) %>%
      select(country, year, indicator_short_name, level_2) 
    
    # fill country NAs with United States and levle_2 NAs with "Missing Data"
    df$country[is.na(df$country)] <- country_name
    df$level_2[is.na(df$level_2)] <- 'Missing Data'
    df$year <- as.character(df$year)
    
    # order level_2
    df$level_2 <- factor(df$level_2, levels =c('Missing Data', 'Catastrophic OOP spending', 'Health Outcomes', 'Impoverishing OOP spending', 'OOP spending', 'Service Coverage') )
    
    df$indicator_short_name <- factor(df$indicator_short_name, levels = sort(unique(df$indicator_short_name), decreasing = TRUE))
    
    # get color graident 
    col_vec <- brewer.pal(name = 'Accent', n = length(unique(df$level_2)))
    col_vec[1] <- 'white'
    
    
    # make plot title 
    plot_title = paste0('Missing data profile', ' - ', country_name)
    
    # plot
    p<-   ggplot(df, aes(year, indicator_short_name, fill = level_2)) + 
      geom_tile(size = 2.5, alpha = 0.8) +
      scale_fill_manual(name = 'Indicator class',
                        values = col_vec) +
      labs(x = 'Year',
           y = '',
           title = plot_title) +
      hefpi::theme_gdocs() +
      theme(axis.text.y = element_text(face = "plain", size = rel(8/12)),
            axis.text.x = element_text(face = "plain", size = rel(8/12), angle = 45, hjust = 1))
    
    
    return(p)
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
    
    # region <- region_list$region
    # indicator <- 'Inpatient care use, adults'
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




