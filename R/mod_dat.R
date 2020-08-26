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
  #tagList(
    fluidPage(
      column(8,
             plotlyOutput(
               ns('dat_country'), height = '800px', width = '1000px', 
             )),
      column(4,
             pickerInput(inputId = ns("indicator"),
                         label = 'Indicator', 
                         choices = indicators_list,
                         selected = indicators$indicator_short_name,
                         options = list( `actions-box`=TRUE,
                                         `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} indicators",
                                         `style` = "btn-primary"),
                         multiple = TRUE),
             pickerInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States',
                         options = list(`style` = "btn-primary")),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2018,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             br(),br(),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
    )
  #)
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
    shinyalert(title = "Data availability - By Country", 
               text = "This chart zooms in on the general data availability situation for a specific country. It allows users to explore, for instance, if data are frequently available for maternal and child health service coverage, while being largely missing for catastrophic healthcare spending. The chart’s vertical axis shows the indicators chosen by the user and the horizontal axis represents time. Years for which data are available for an indicator are marked by colored squares in the chart area. Hence, larger colored chart areas represent better data availability for the user’s country of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE PLOT DATA---- #
  get_dat <- reactive({
    country_name = 'United States'
    indicator = indicators$indicator_short_name
    date_range = c(1982, 2018)
    country_name <- input$country
    indicator = input$indicator
    date_range = input$date_range
    dat_list <- list()
    # get all unique years and indicators
    temp <- hefpi::df
    all_years <- sort(unique(temp$year))
    all_ind <- unname(unlist(indicators_list))
    all_ind <- all_ind[all_ind %in% indicator]
    # subset data by country and join to get indicator short name 
    country_data<- hefpi::df %>%
      filter(country == country_name) %>%
      filter(indicator_short_name %in% indicator) %>%
      filter(year >= date_range[1],
             year <= date_range[2]) 
    # create data frame with year and indicator combinations
    df <- tidyr::expand_grid(year = all_years, indicator_short_name = all_ind) %>%
      left_join(country_data) %>%
      select(country, year, indicator_short_name, level2) 
    # fill country NAs with United States and levle_2 NAs with "Missing Data"
    df$country[is.na(df$country)] <- country_name
    df$level2[is.na(df$level2)] <- 'Missing Data'
    df$year <- as.character(df$year)
    col_data <- data_frame(level_2 = c( 'OOP spending', 'Catastrophic OOP spending', 'Impoverishing OOP spending', 'Service Coverage', 'Health Outcomes', 'Missing Data'), 
                           color = c("#9BCFFF", "#57AEFF", '#0C88FC', '#14DA00', '#FFB80A', 'white'))
    # recode level2
    df$level2 <- ifelse(df$level2 == 'h_cov', 'Service Coverage',
                        ifelse(df$level2 == 'h_out', 'Health Outcomes',
                               ifelse(df$level2 == 'f_cata', 'Catastrophic OOP spending',
                                      ifelse(df$level2 == 'f_impov', 'Impoverishing OOP spending',
                                             ifelse(df$level2 == 'f_oop', 'OOP spending', 'Missing Data')))))
    # subset col data by data selected
    level2_levels = col_data$level_2[col_data$level_2 %in% unique(df$level2)]
    col_vec = col_data$color[col_data$level_2 %in% unique(df$level2)]
    # order level2
    df$level2 <- factor(df$level2, levels =level2_levels )
    df$indicator_short_name <- factor(df$indicator_short_name, levels = rev(all_ind))
    # make plot title 
    plot_title = paste0('Data availability', ' - ', country_name)
    # plot
    p<-   ggplot(df, aes(as.numeric(year), indicator_short_name, fill = level2)) + 
      geom_tile(alpha = 0.8, color = 'lightgrey') +
      scale_x_continuous(breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                         expand = c(0,0)) +
      scale_fill_manual(name = '',
                        values = col_vec) +
      labs(x = 'Year',
           y = '',
           title = plot_title)
    p
    dat_list[[1]] <- p
    dat_list[[2]] <- df
    dat_list[[3]] <- list(plot_title, col_vec)
    return(dat_list)
  })
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_data_availability_country", ".png"),
                                    content = function(file) {
                                      dat_list <- get_dat()
                                      if(is.null(dat_list)){
                                        NULL
                                      } else {
                                        p <- dat_list[[1]]
                                        p =  p + hefpi::theme_hefpi(grid_major_x = NA,
                                                                    grid_major_y = NA,
                                                                    grid_minor_x = NA,
                                                                    grid_minor_y = NA,
                                                                    y_axis_line = 'white',
                                                                    x_axis_line = 'white',
                                                                    x_axis_size = rel(1),
                                                                    y_axis_size = rel(2/3),
                                                                    y_axis_hjust = 1,
                                                                    x_axis_angle = 90,
                                                                    x_axis_vjust = 0.5,
                                                                    legend_position = 'top',
                                                                    legend_direction = 'horizontal',
                                                                    legend_text_size = rel(1/2)) + 
                                          labs(title = '')
                                        p
                                        ggsave(file, width = 10, height = 8)
                                      }
                                    })
  
  # ---- GENERATE PLOT ---- #
  output$dat_country <- renderPlotly({
    dat_list <- get_dat()
    if(is.null(dat_list)){
      NULL
    } else {
      df= dat_list[[2]]
      if(nrow(df) == 0) {
        empty_plot <- function(title = NULL){
          p <- plotly_empty(type = "scatter", mode = "markers") %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = title,
                yref = "paper",
                y = 0.5
              )
            )
        } 
        fig <- empty_plot("No data available for the selected inputs")
        fig
      } else {
        p <- dat_list[[1]]
        p  <- p + hefpi::theme_hefpi(x_axis_angle = 90, 
                                     x_axis_vjust = 0.5,
                                     y_axis_hjust = 1,
                                     x_axis_size = 8, 
                                     y_axis_size = 8, 
                                     grid_major_x = NA,
                                     grid_major_y = NA,
                                     legend_text_size = 2/3)
        ggplotly(p, tooltip = 'none') %>%
          config(displayModeBar = F)
      }
    }
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
      column(8,
             tags$div(style='overflow-y: scroll; position: relative', plotlyOutput(ns('dat_ind'), height = '600px', width = '1000px') )
             ),
      column(4,
             pickerInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected =indicators$indicator_short_name[1],
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('region'), 'Region',
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1],
                         options = list( `actions-box`=TRUE,
                                         `style` = "btn-primary",
                                         `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} Regions"),
                         multiple = TRUE),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             br(),br(),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
             ))
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
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Data availability - By indicator", 
               text = "This chart allows user to compare data availability for an indicator across countries and over time. Years for which data are available for a country are marked by colored squares in the chart area. Hence, larger colored chart areas represent better data availability for the user’s indicator of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    # get inputs
    # region = as.character(region_list$region)[1]
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # subset data by variable and region code - HERE need to get level2 for plot
    df<- hefpi::df %>%
      # filter(indic == variable) %>%
      filter(regioncode %in% region_code) %>%
      # filter(country %in% country_name) %>%
      select(year,country, indic, regioncode, referenceid_list) 
    country_names <- sort(unique(df$country))
    # get ui page
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("country"), 
                    label = 'Countries', 
                    choices = country_names, 
                    selected = country_names,
                    options = list( `actions-box`=TRUE,
                                    `style` = "btn-primary",
                                    `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} Countries"),
                    multiple = TRUE),
        sliderInput(inputId = session$ns('date_range'),
                    label = 'Date range',
                    min = 1982,
                    max = 2018,
                    value = c(1982, 2018),
                    step = 1,
                    sep = '')
      )
    )
  })
  
  get_dat <- reactive({
    # get inputs
    # indicator <- indicators$indicator_short_name[1]
    date_range = c(1982,2018)
    indicator <- input$indicator
    region <- input$region
    country_names <- input$country
    date_range <- input$date_range
    if(is.null(date_range)){
      NULL
    } else {
      dat_list <- list()
      # get region code
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      # Get the variable
      variable <- indicators %>%
        filter(indicator_short_name %in% indicator) %>%
        .$variable_name
      # subset data by variable and region code - HERE need to get level2 for plot
      df<- hefpi::df %>%
        filter(indic %in% variable) %>%
        filter(regioncode %in% region_code) %>%
        filter(country %in% country_names) %>%
        select(year,country, indic, regioncode, referenceid_list, level2, indicator_short_name) 
      names(df)[names(df) == 'regioncode'] <- 'region'
      # create a region year country data
      country_data <- hefpi::df %>% 
        # filter(indic == variable) %>%
        filter(regioncode %in% region_code) %>% # consider removing this, to show all years, not just the years where a region has any data
        select(year, country,regioncode, indic) 
      all_years <- sort(unique(hefpi::df$year))
      all_countries <- sort(unique(country_data$country))
      temp_data <- expand_grid(year = all_years, country = all_countries) %>%
        left_join(df)
      # subset by country_names
      temp_data <- temp_data %>% filter(country %in% country_names)
      col_data <- data_frame(level_2 = c( 'OOP spending', 'Catastrophic OOP spending', 'Impoverishing OOP spending', 'Service Coverage', 'Health Outcomes', 'Missing Data'), 
                             color = c("#9BCFFF", "#57AEFF", '#0C88FC', '#14DA00', '#FFB80A', 'transparent'))
      # recode level2
      temp_data$level2 <- ifelse(temp_data$level2 == 'h_cov', 'Service Coverage',
                          ifelse(temp_data$level2 == 'h_out', 'Health Outcomes',
                                 ifelse(temp_data$level2 == 'f_cata', 'Catastrophic OOP spending',
                                        ifelse(temp_data$level2 == 'f_impov', 'Impoverishing OOP spending',
                                               ifelse(temp_data$level2 == 'f_oop', 'OOP spending', 'Missing Data')))))
      temp_data$level2[is.na(temp_data$level2)] <- 'Missing Data'
      # subset col data by data selected
      level2_levels = col_data$level_2[col_data$level_2 %in% unique(temp_data$level2)]
      col_vec = col_data$color[col_data$level_2 %in% unique(temp_data$level2)]
      # order level2
      temp_data$level2 <- factor(temp_data$level2, levels =level2_levels )
      temp_data$country <- factor(temp_data$country, levels = sort(unique(temp_data$country), decreasing = TRUE))
        # make plot title 
        plot_title = paste0('Data availability',' - ', indicator)
        mytext <- paste(
          "Economy: ", as.character(temp_data$country), "\n",
          "Indicator class: ", as.character(temp_data$level2), "\n",
          sep="") %>%
          lapply(htmltools::HTML)
        # number of countries
        plot_height <- ceiling(((length(unique(temp$country))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        p <- ggplot(temp_data, aes(country, as.numeric(year), fill =level2, text =mytext)) + 
                        geom_tile(size = 0.5, alpha = 0.8, color = 'lightgrey') +
          scale_y_continuous(breaks = seq(from = date_range[1],to = date_range[2], by = 1),
                             expand = c(0,0)) +
                        scale_fill_manual(name = '',
                                          values = col_vec) +
                        labs(x = '',
                             y = 'Year',
                             title = plot_title) +
          coord_flip() +
          theme(legend.position = "none") 
        dat_list[[1]] <- p
        dat_list[[2]] <- df
        dat_list[[3]] <- list(plot_title, col_vec, mytext, plot_height)
        return(dat_list)
    }
  })
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("data_indicators_",Sys.Date(), ".png"),
                                    content = function(file) {
                                      dat_list <- get_dat()
                                      if(is.null(dat_list)){
                                        NULL
                                      } else {
                                        p <- dat_list[[1]]
                                        p =  p + theme_hefpi(grid_major_x = NA,
                                                             grid_major_y = NA,
                                                             grid_minor_x = NA,
                                                             grid_minor_y = NA,
                                                             y_axis_line = 'white',
                                                             x_axis_line = 'white',
                                                             x_axis_size = rel(1),
                                                             y_axis_size = rel(2/3),
                                                             y_axis_hjust = 1,
                                                             x_axis_angle = 90,
                                                             x_axis_vjust = 0.5,
                                                             legend_position = 'top',
                                                             legend_direction = 'horizontal',
                                                             legend_text_size = rel(1/2)) +
                                          
                                          labs(title = '')
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                    })
  
  # ---- GENERATE PLOT ---- #
  output$dat_ind <- renderPlotly({
    dat_list <- get_dat()
    if(is.null(dat_list)){
      NULL
    } else {
      pd <- dat_list[[2]]
      save(pd, file = 'full.rda')
      if(nrow(pd)==0){
        empty_plot <- function(title = NULL){
          p <- plotly_empty(type = "scatter", mode = "markers") %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = title,
                yref = "paper",
                y = 0.5
              )
            )
        } 
        fig <- empty_plot("No data available for the selected inputs")
      } else {
        p <- dat_list[[1]]
        plot_height <- dat_list[[3]][[4]]
        p <- p +
          hefpi::theme_hefpi(x_axis_angle = 90,
                             x_axis_vjust = 0.5, 
                             x_axis_size = 10,
                             y_axis_size = 10,
                             y_axis_hjust = 1,
                             grid_major_x = NA,
                             grid_major_y = NA,
                             grid_minor_x = NA,
                             grid_minor_y = NA,
                             y_axis_line = 'white',
                             x_axis_line = 'white',
                             legend_position = 'none') 
      
        fig <- ggplotly(p, 
                        tooltip = 'text',
                        height = plot_height) %>%
          config(displayModeBar = F)
        fig
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
