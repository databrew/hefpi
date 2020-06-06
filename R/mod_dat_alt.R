
# Module data availability alternate

#' @title   mod_dat_alt.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' 
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
             plotlyOutput(
               ns('dat_ind'), height = '900px'
             )),
      column(4,
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list),
             selectInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = 'Europe & Central Asia'),
             uiOutput(ns('country_ui')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(2012, 2017),
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

mod_dat_ind_alt_server <- function(input, output, session){
  
  

  
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
    region <- 'Europe & Central Asia'
    region <- input$region
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # subset data by variable and region code - HERE need to get level_2 for plot
    df<- hefpi::df %>%
      # filter(indic == variable) %>%
      filter(regioncode %in% region_code) %>%
      # filter(country %in% country_name) %>%
      select(year,country, indic, regioncode, referenceid_list) 
    
    country_names <- sort(unique(df$country))
  
    selectInput(inputId = session$ns("country"), 
                label = 'Countries', 
                choices = country_names, 
                selected = country_names[1:3],
                multiple = TRUE)
    
  })
  
  
  output$dat_ind <- renderPlotly({
    # region <- 'Europe & Central Asia'
    # indicator <- 'Catastrophic health spending, 10%'
    # country_names <- country_names[1:3]
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
      region_code <- as.character(region_list$region_code[region_list$region == region])
      
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
                axis.text.x = element_text(face = "plain", size = rel(8/12), angle = 45, hjust = 1)), tooltip = 'text'))
        

      }
      
    }
   
  })
}

## To be copied in the UI
# mod_dat_country_alt_ui("dat_country1")
# mod_dat_country_alt_ui("dat_ind1")


## To be copied in the server
# callModule(mod_dat_country_alt_server, 'dat_country1')
# callModule(mod_dat_ind_alt_server, 'dat_ind1')



