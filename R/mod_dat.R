# Module data availability

#' @title   mod_dat.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dat_country_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(10,
             plotOutput(
               ns('dat_country'), height = '800px', 
             )),
      column(2,
             selectInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States'),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_dat_country_server
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

mod_dat_country_server <- function(input, output, session){
  
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
    country_name = 'United States'
    country_name <- input$country
    
    # get all unique years and indicators
    temp <- hefpi::df
    all_years <- sort(unique(temp$year))
    all_ind <- unname(unlist(indicators_list))
    
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
    df$level_2 <- factor(df$level_2, levels =c( 'OOP spending', 'Catastrophic OOP spending', 'Impoverishing OOP spending', 'Service Coverage', 'Health Outcomes', 'Missing Data') )
    
    df$indicator_short_name <- factor(df$indicator_short_name, levels = rev(all_ind))
    
    # get color vector (first 3 different shades of blue, 4th green, 5th orange, NA white)
   col_vec =  c("#9BCFFF", "#57AEFF", '#0C88FC', '#14DA00', '#FFB80A', 'white')
    
    # make plot title 
    plot_title = paste0('Missing data profile', ' - ', country_name)
    
    # plot
    p<-   ggplot(df, aes(year, indicator_short_name, fill = level_2)) + 
      geom_tile(alpha = 0.8, color = 'darkgrey') +
      scale_fill_manual(name = 'Indicator class',
                         values = col_vec) +
      labs(x = 'Year',
           y = '',
           title = plot_title) +
      hefpi::theme_gdocs() +
      theme(axis.text.y = element_text(face = "plain", size = rel(6/12)),
            axis.text.x = element_text(face = "plain", size = rel(6/12), angle = 45, hjust = 1))
    
    
    return(p)
  })
  
 
}

#-------------------------------------------------------------------------------------------------------------
#' @rdname mod_dat_ind_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_ind_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(10,
             plotlyOutput(
               ns('dat_ind'), height = '800px'
             )),
      column(2,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = 'Europe & Central Asia',
                         multiple = TRUE),
             uiOutput(ns('country_ui')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
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

mod_dat_ind_server <- function(input, output, session){
  
  
  
  
  output$country_ui <- renderUI({
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
    region <- c('Europe & Central Asia', 'Sub-Saharan Africa')
    region <- input$region
    
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    
    # subset data by variable and region code - HERE need to get level_2 for plot
    df<- hefpi::df %>%
      # filter(indic == variable) %>%
      filter(regioncode %in% region_code) %>%
      # filter(country %in% country_name) %>%
      select(year,country, indic, regioncode, referenceid_list) 
    
    top_countries <- df %>% group_by(regioncode, country) %>% summarise(counts = n()) %>%
      arrange(desc(counts)) %>% 
      top_n(3) %>%
      .$country


    selectInput(inputId = session$ns("country"), 
                label = 'Countries', 
                choices = unique(df$country), 
                selected = top_countries,
                multiple = TRUE)
    
  })
  
  
  output$dat_ind <- renderPlotly({
    # region <- 'Europe & Central Asia'
    # indicator <- 'Catastrophic health spending, 10%'
    # country_names <- top_countries[1:3]
    # date_range <- c(2012, 2017)
    # country_name = c('Argentina', 'Brazil', 'Chile', 'Ecuador')
    # country_name <- input$country
    indicator <- input$indicator
    region <- input$region
    country_names <- input$country
    date_range <- input$date_range
    if(is.null(country_names)){
      NULL
    } else {
      
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
        filter(country %in% country) %>%
        select(year,country, indic, regioncode, referenceid_list) 
      
      names(df)[names(df) == 'regioncode'] <- 'region'
      
      # HERE
      # create a region year country data
      country_data <- hefpi::df %>% 
        # filter(indic == variable) %>%
        filter(regioncode == region_code) %>% # consider removing this, to show all years, not just the years where a region has any data
        select(year, country,regioncode, indic) 
      all_years <- sort(unique(country_data$year))
      all_countries <- sort(unique(country_data$country))
      
      
      temp_data <- expand_grid(year = all_years, country = all_countries) %>%
        left_join(df)
      
      # subset by country_names
      temp_data <- temp_data %>% filter(country %in% country_names)
      
      # subset by year 
      temp_data <- temp_data %>%filter(year >= min(date_range),
                                       year <= max(date_range)) 
      
      # temp_data$country <- ifelse(is.na(temp_data$country), 'No data', temp_data$country)
      temp_data$indic <- ifelse(is.na(temp_data$indic), 'No data', indicator)
      
      # # find no data index
      all_indic <- as.character(sort(unique(temp_data$indic)))
      no_data_index <- which(all_indic == 'No data')
      
      if(is.null(temp_data) | nrow(temp_data) == 0){
        NULL
      } else {
        # get color graident
        col_vec <- c(brewer.pal(name = 'Accent', n = length(unique(temp_data$indic))))
        col_vec[no_data_index] <- 'white'
        
        # make plot title 
        plot_title = paste0('Missing data profile', ' - ', indicator, ' for ', region)
        
        mytext <- paste(
          "Year: ", as.character(temp_data$year),"\n",
          "Country: ", as.character(temp_data$country), "\n",
          "Data source: ", as.character(temp_data$referenceid_list),
          sep="") %>%
          lapply(htmltools::HTML)
        
        print(ggplotly(ggplot(temp_data, aes(country, as.character(year), fill =indic, text =mytext)) + 
                         geom_tile(size = 2.5, alpha = 0.8) +
                         scale_fill_manual(name = 'Country',
                                           values = col_vec) +
                         labs(x = 'Year',
                              y = '',
                              title = plot_title) +
                         hefpi::theme_gdocs() +
                         theme(legend.position = 'none',
                               axis.text.y = element_text(face = "plain", size = rel(10/12)),
                               axis.text.x = element_text(face = "plain", size = rel(8/12), angle = 45, hjust = 1)) + coord_flip(), tooltip = 'text'))
        
        
      }
      
    }
    
  })
}



## To be copied in the UI
# mod_dat_country_ui("dat_country1")
# mod_dat_country_ui("dat_ind1")


## To be copied in the server
# callModule(mod_dat_country_server, 'dat_country1')
# callModule(mod_dat_ind_server, 'dat_ind1')




