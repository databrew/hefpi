# Module data availability
#' @title mod_dat.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @keywords internal
#' @export 
# UI FOR DATA AVAILABILITY (COUNTRY)
mod_dat_country_ui <- function(id){
  ns <- NS(id)
  #tagList(
    fluidPage(
      column(9,
             plotlyOutput(
               ns('dat_country'), height = '800px', width = '1000px', 
             )),
      column(3,
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             br(), br(),
             p('Indicator'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select indicators', 
                                          status = "danger",
                                          actionButton(ns("all_inds"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = ns("indicator"),
                         label = NULL, 
                         choices = indicators$indicator_short_name,
                         selected = indicators$indicator_short_name))),
             p('Country'),
             div(style='border-color: grey; color:grey',selectInput(ns('country'), 
                         label = NULL,
                         choices = country_list,
                         selected = 'United States')),
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2018,
                         value = c(1982, 2018),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'))
    )
  #)
}

# SERVER FOR DATA AVAILABILITY (COUNTRY)
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
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # INDICATORS
  observe({
    all_inds <- input$all_inds
    message(all_inds)
    if(is.null(all_inds)){
      NULL
    } else {
      if (all_inds > 0) {
        if (all_inds %% 2 == 0){
          updateCheckboxGroupInput(session=session,
                                   inputId ="indicator",
                                   choices = indicators$indicator_short_name,
                                   selected = indicators$indicator_short_name)
          
        } else {
          updateCheckboxGroupInput(session=session,  
                                   inputId ="indicator",
                                   choices = indicators$indicator_short_name,
                                   selected = c())
          
        }}
    }
    
  })
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
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
    dat_list <- list(df, date_range, col_vec)
    chart_data$plot_data <- dat_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
 
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_data_availability_country", ".png"),
                                    content = function(file) {
                                      dat_list <- chart_data$plot_data
                                      if(length(dat_list)==1){
                                        dat_list <- hefpi::dat_country_default
                                      }
                                      if(is.null(dat_list)){
                                        NULL
                                      } else {
                                        df <- dat_list[[1]]
                                        date_range <- dat_list[[2]]
                                        col_vec <-dat_list[[3]]
                                        # make plot title 
                                        plot_title = paste0('Data availability', ' - By country')
                                        # plot
                                        p<-   ggplot(df, aes(as.numeric(year), indicator_short_name, fill = level2)) + 
                                          geom_tile(alpha = 0.8, color = 'lightgrey') +
                                          scale_x_continuous(breaks = seq(from = date_range[1],to = date_range[2], by = 1), 
                                                             expand = c(0,0)) +
                                          scale_fill_manual(name = '',
                                                            values = col_vec) +
                                          labs(x = 'Year',
                                               y = '',
                                               title = '')
                                        
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
    dat_list <- chart_data$plot_data
    if(length(dat_list)==1){
      dat_list <- hefpi::dat_country_default
    }
    if(is.null(dat_list)){
      NULL
    } else {
      df= dat_list[[1]]
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
        df <- dat_list[[1]]
        date_range <- dat_list[[2]]
        col_vec <-dat_list[[3]]
        # make plot title 
        plot_title = paste0('Data availability', ' - By country')
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
# UI FOR DATA AVAILABILITY (INDICATOR)
mod_dat_ind_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(9,
             tags$div(style='overflow-y: scroll; position: relative', plotlyOutput(ns('dat_ind'), height = '600px', width = '1000px') )
             ),
      column(3,
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             br(), br(),
             p('Indicator'),
             div(style='border-color: grey; color:grey',selectInput(ns('indicator'),
                         label = NULL,
                         choices = indicators$indicator_short_name,
                         selected =indicators$indicator_short_name[1])),
             p( 'Region'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the region(s)', 
                                          status = "danger",
                                          actionButton(ns("all_regions"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(ns('region'),
                         label = NULL,
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1]))),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'))
    )
  )
}

# SERVER FOR DATA AVAILABILITY (INDICATOR)
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
       p('Countries'),
       shinyWidgets::dropdownButton(circle = FALSE,  
                                    label = 'Select the countries)', 
                                    status = "danger",
                                    actionButton(session$ns("all_countries"), label="Select/Deselect all"),
       div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = session$ns("country"), 
                    label = NULL, 
                    choices = country_names, 
                    selected = country_names))),
        p('Date range'),
        sliderInput(inputId = session$ns('date_range'),
                    label = NULL,
                    min = 1982,
                    max = 2018,
                    value = c(1982, 2018),
                    step = 1,
                    sep = '')
      )
    )
  })
  
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # REGIONS
  observe({
    all_regions <- input$all_regions
    message(all_regions)
    if(is.null(all_regions)){
      NULL
    } else {
      if (all_regions > 0) {
        if (all_regions %% 2 == 0){
          message(region_list$region)
          updateCheckboxGroupInput(session=session,
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = as.character(region_list$region))
          
        } else {
          updateCheckboxGroupInput(session=session,  
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = c())
          
        }}
    }
    
  })
  
  
  
  # COUNTRY
  observe({
    all_countries <- input$all_countries
    message(all_countries)
    if(is.null(all_countries)){
      NULL
    } else {
      if (all_countries > 0) {
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
        if (all_countries %% 2 == 0){
          
          updateCheckboxGroupInput(session=session,
                                   "country",
                                   choices = country_names,
                                   selected = country_names)
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   "country",
                                   choices = country_names,
                                   selected = c())
          
        }}
    }
    
  })
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs
    region = as.character(region_list$region)[1]
    indicator <- indicators$indicator_short_name[1]
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
      temp_data <- temp_data %>% filter(year >= date_range[1],
                                        year <= date_range[2]) 
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
      
      dat_list <- list(temp_data, date_range, col_vec,indicator)
    }
    
    chart_data$plot_data <- dat_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("data_indicators_",Sys.Date(), ".png"),
                                    content = function(file) {
                                      dat_list <- chart_data$plot_data
                                      if(length(dat_list)==1){
                                        dat_list <- hefpi::dat_indicator_default
                                      }
                                      if(is.null(dat_list)){
                                        NULL
                                      } else {
                                        temp_data <- dat_list[[1]]
                                        date_range <- dat_list[[2]]
                                        col_vec <- dat_list[[3]]
                                        indicator <- dat_list[[4]]
                                        
                                        
                                        # make plot title 
                                        plot_title = paste0('Data availability',' - By indicator')
                                        mytext <- paste(
                                          "Economy: ", as.character(temp_data$country), "\n",
                                          "Indicator class: ", as.character(temp_data$level2), "\n",
                                          sep="") %>%
                                          lapply(htmltools::HTML)
                                        # number of countries
                                        plot_height <- ceiling(((length(unique(temp_data$country))* 100) + 100)/3)
                                        if(plot_height < 250){
                                          plot_height <- 250
                                        }
                                        p <- ggplot(temp_data, aes(country, as.numeric(year), fill =level2)) + 
                                          geom_tile(size = 0.5, alpha = 0.8, color = 'lightgrey') +
                                          scale_y_continuous(limits = c(min(temp_data$year),max(temp_data$year)),
                                                             breaks = seq(from = min(temp_data$year),
                                                                          to =max(temp_data$year), by = 1),
                                                             expand = c(0,-0.5)) +
                                          scale_fill_manual(name = '',
                                                            values = col_vec) +
                                          labs(x = '',
                                               y = 'Year',
                                               title = '') +
                                          coord_flip() +
                                          theme(legend.position = "none") 
                                        
                                        
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
    dat_list <- chart_data$plot_data
    if(length(dat_list)==1){
      dat_list <- hefpi::dat_indicator_default
    }
    if(is.null(dat_list)){
      NULL
    } else {
      pd <- dat_list[[1]]
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
        # save(dat_list, file = 'temp_dat_list.rda')
        temp_data <- dat_list[[1]]
        date_range <- dat_list[[2]]
        col_vec <- dat_list[[3]]
        indicator <- dat_list[[4]]
        
        
        # make plot title 
        plot_title = paste0('Data availability',' - By ', 'indicator')
        mytext <- paste(
          "Economy: ", as.character(temp_data$country), "\n",
          "Indicator class: ", as.character(temp_data$level2), "\n",
          sep="") %>%
          lapply(htmltools::HTML)
        # number of countries
        plot_height <- ceiling(((length(unique(temp_data$country))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        p <- ggplot(temp_data, aes(as.numeric(year),country, fill =level2, text =mytext)) + 
          geom_tile(size = 0.5, alpha = 0.8, color = 'lightgrey') +
          scale_x_continuous(limits = c(min(temp_data$year),max(temp_data$year)),
                             breaks = seq(from = min(temp_data$year),
                                          to =max(temp_data$year), by = 1),
                             expand = c(0,-0.5)) +
          scale_fill_manual(name = '',
                            values = col_vec) +
          labs(x = '',
               y = 'Year',
               title = plot_title) +
          theme(legend.position = "none") 
        
        p <- p +
          hefpi::theme_hefpi(x_axis_angle = 90,
                             x_axis_hjust = 0.5,
                             y_axis_hjust = 1,
                             y_axis_vjust = 0.5,
                             x_axis_size = 10,
                             y_axis_size = 10,
                             grid_major_x = NA,
                             grid_major_y = NA,
                             grid_minor_x = NA,
                             grid_minor_y = NA,
                             y_axis_line = 'white',
                             x_axis_line = 'white',
                             legend_position = 'none') 
        p
      
        fig <- ggplotly(p, 
                        tooltip = 'text',
                        height = plot_height) %>%
          config(displayModeBar = F) %>%
          layout(xaxis = list(side ="top" ), margin = list(t=130))  
        fig
      }
    }
  })
}
