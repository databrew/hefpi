# Module dotplots
#' @title mod_dots.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @keywords internal
#' @export 


# UI QUINTILES COUNTRY
mod_dots_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
       column(8,
                 uiOutput(ns('dots_country_title')),
                 tags$div(style='overflow-y: scroll; position: relative', plotlyOutput(ns('dots_country'), height = '600px', width = '1000px') )
      ),
      column(3,
             class="selectizeWidth",
             #useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             # actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             selectInput(inputId = ns("indicator"),
                         label = NULL, 
                         choices = indicators_list,
                         selected = "4+ antenatal care visits (%)"),
             p('Region'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the region(s)', 
                                          status = "danger",
                                          actionButton(ns("all_regions"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = ns("region"),
                         label = '', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)))),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER QUINTILES COUNTRY
mod_dots_country_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintiles - Most recent value by country", 
               text = "This chart enables users to compare inequalities in HEFPI indicators by household wealth, both within and across countries. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. For a set of countries and an indicator the user specifies, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GET UI OUTPUTS ---- #
  output$ui_outputs <- renderUI({
    # get inputs
    indicator <- input$indicator
    region <- input$region
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region %in% region])
    # Get the variable
    variable <- indicators %>%
      dplyr::filter(indicator_short_name == indicator) %>%
      .$variable_name
    # subset data by variable and region code
    df <- hefpi::hefpi_df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode %in% region_code,]
    df <- df %>% 
      filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4) & !is.na(Q5)) %>%
      select(year, country, referenceid_list,indic, Q1:Q5) 
    # made data long form
    df <- melt(df, id.vars = c('year', 'country', 'referenceid_list', 'indic'))
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    # create select input for country
    countries <- unique(df$country)
    # ui inputs
    fluidPage(
      fluidRow(
        p('Country'),
        shinyWidgets::dropdownButton(circle = FALSE,  
                                     label = 'Select the countries', 
                                     status = "danger",
                                     actionButton(session$ns("all_countries"), label="Select/Deselect all"),
        div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = session$ns("country"),
                    label = NULL, 
                    choices = countries,
                    selected = countries))),
        p('X axis range'),
        sliderInput(session$ns('value_range'),
                    label = NULL,
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = ''),
        p('Date range'),
        sliderInput(session$ns('date_range'),
                    label = NULL,
                    min = 1982,
                    max = 2021,
                    value = c(1982, 2021),
                    step = 1,
                    sep = ''),
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
        # get inputs
        indicator <- input$indicator
        region <- input$region
        # get region code
        region_list <- hefpi::region_list
        region_code <- as.character(region_list$region_code[region_list$region %in% region])
        # Get the variable
        variable <- indicators %>%
          dplyr::filter(indicator_short_name == indicator) %>%
          .$variable_name
        # subset data by variable and region code
        df <- hefpi::hefpi_df
        df <- df[df$indic == variable,]
        df <- df[df$regioncode %in% region_code,]
        df <- df %>% 
          filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4) & !is.na(Q5)) %>%
          select(year, country, referenceid_list,indic, Q1:Q5) 
       
        # create select input for country
        countries <- unique(df$country)
        if (all_countries %% 2 == 0){
          
          updateCheckboxGroupInput(session=session,
                                   "country",
                                   choices = countries,
                                   selected = countries)
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   "country",
                                   choices = countries,
                                   selected = c())
          
        }}
    }
  
  })
  
  chart_data <- reactiveValues(plot_data = 'new') 
  observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get input data
    region <- input$region
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    date_range <- input$date_range
    # condition for ui being temorarily null
    if(is.null(value_range)){
      NULL
    } else {
      dot_list <- list()
      # Get the variable
      ind_info <- indicators %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
      # subset by country and variable
      temp <- hefpi::hefpi_df %>%
        filter(country %in% country_names) %>%
        filter(indic == variable_name) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) 
      # get year and keep only necessary columns
      df <- temp %>%
        group_by(country) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) 
      # made data long form
      id_vars <- names(df)[!grepl('Q', names(df))]
      df <- melt(df, id.vars = id_vars)
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      # only keep data with no NAs
      df <- df[!is.na(df$value),]
      #df <- df[complete.cases(df),]
      # order country
      df$country <- factor(df$country,levels= sort(unique(df$country), decreasing = TRUE ))
      df <- df %>% filter(value >= value_range[1],
                          value <= value_range[2])
      if(unit_of_measure == '%'){
        df$value <- df$value*100
        value_range[2] <- value_range[2]*100
        value_range[1] <- value_range[1]*100
      }

      # just save data, not implement 
      dot_list <- list(df,unit_of_measure, indicator, value_range,date_range)
      # save(dot_list, file ='temp_dot_list.rda')
    }
    chart_data$plot_data <- dot_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("quintile_country_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      dot_list <- chart_data$plot_data
      if(length(dot_list)==1){
        dot_list <- hefpi::dots_country_default
      }
      if(is.null(dot_list)){
        NULL
      } else {
        df <- dot_list[[1]]
        if(nrow(df) == 0){
          temp <- data_frame()
        } else {
          temp <- df
          names(temp) <- tolower(names(temp))
          names(temp)[names(temp)=='variable'] <- 'level'
          # subset by  
          temp$parameter <- 'Mean'
          # temp$level <- 'National'
          temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, ci, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid<- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
        }
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("quintile_country_", Sys.Date(),".png"),
                                    content = function(file) {
                                      
                                      dot_list <- chart_data$plot_data
                                      if(length(dot_list)==1){
                                        dot_list <- hefpi::dots_country_default
                                      }
                                     
                                      if(is.null(dot_list)){
                                        NULL
                                      } else {
                                        df <- dot_list[[1]]
                                        unit_of_measure <- dot_list[[2]]
                                        indicator <- dot_list[[3]]
                                        value_range <- dot_list[[4]]
                                        date_range <- dot_list[[5]]
                                        
                                        
                                        # get color graident 
                                        # col_vec <- brewer.pal(name = 'PRGn', n = length(unique(df$variable)) + 1)
                                        # col_vec <- col_vec[-1]
                                        col_vec <- c("#006e38", "#75a56e","#a89fe1", "#6d60bb", "#312271")
                                        
                                        # make plot title 
                                        plot_title = paste0('Quintiles - Most recent value by country', ' - ', indicator)
                                        sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
                                        y_axis_text = paste0(indicator)
                                        caption_text = 'HEFPI database, The World Bank, 2021'
                                        
                                        mytext <- paste(
                                          "Value: ", paste0(round(df$value, digits = 2), ' (', unit_of_measure, ')'), "\n",
                                          "Year: ", as.character(df$year),"\n",
                                          "Indicator: ", as.character(indicator),"\n",
                                          "Data source: ", as.character(df$referenceid_list),
                                          sep="") %>%
                                          lapply(htmltools::HTML)
                                        # number of countries
                                        plot_height <- ceiling(((length(unique(df$country))* 100) + 100)/3)
                                        if(plot_height < 250){
                                          plot_height <- 250
                                        }
                                        p <- ggplot(df, aes(x=country,
                                                            y=value)) +
                                          geom_point(size=rel(2), alpha = 0.7, aes(color = variable)) +
                                          geom_line(aes(group = country), color = 'black', size = 0.25) +
                                          scale_color_manual(name = '',
                                                             values = col_vec) +
                                          scale_y_continuous(limits = c((value_range[1]), (value_range[2] + 5)), 
                                                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                                                             expand = c(0,0)) +
                                          labs(title='',
                                               subtitle = '', 
                                               x = y_axis_text, 
                                               y = 'Country', 
                                               caption=caption_text) +
                                          coord_flip() 
                                        
                                        p <- p +
                                          hefpi::theme_hefpi(grid_major_x = NA,
                                                                    y_axis_size = rel(2/3),
                                                                    x_axis_size = rel(1),
                                                                    x_axis_hjust = 0.5,
                                                                    y_axis_hjust = 1,
                                                                    y_axis_vjust = 0.5) +
                                                          theme(
                                                            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            panel.grid.major.x = element_blank(),
                                                            panel.grid.minor.x = element_blank(),
                                                            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                                                          ) +
                                                          labs(title = '',
                                                               subtitle = '',
                                                               x = y_axis_text, 
                                                               y = 'Country'
                                                               ) 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                    })

  
  # ---- PLOT TITLE dots_country ---- #
  output$dots_country_title <- renderUI({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_country_default
    }
    if(is.null(dot_list)){
      NULL
    } else {
      df <- dot_list[[1]]

      unit_of_measure <- dot_list[[2]]
      indicator <- dot_list[[3]]

      # make plot title 
      # plot_title = paste0('Quintiles - Most recent value by country', ' - ', indicator)
      plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Quintiles </div> 
                           <div class="chart-label"> Most recent value by country </div>
                           <div class="chart-label"> {indicator} </div>
                          </div>
                          '))
      
      plot_title
      
      
    }
  })
  
  
    
  # ---- GENERATE PLOT ---- #
  output$dots_country <- renderPlotly({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_country_default
    }
    if(is.null(dot_list)){
      NULL
    } else {
      df <- dot_list[[1]]
      if(nrow(df)==0){
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
        df <- dot_list[[1]]
        unit_of_measure <- dot_list[[2]]
        indicator <- dot_list[[3]]
        value_range <- dot_list[[4]]
        date_range <- dot_list[[5]]
        
        
        # get color graident 
        # col_vec <- brewer.pal(name = 'PRGn', n = length(unique(df$variable)) + 1)
        col_vec <- c("#006e38", "#75a56e","#a89fe1", "#6d60bb", "#312271")
        # col_vec <- col_vec[-1]
        # make plot title 
        # plot_title = paste0('Quintiles - Most recent value by country', ' - ', indicator)
        sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
        y_axis_text = paste0(indicator)
        
        mytext <- paste(
          "Value: ", paste0(round(df$value, digits = 2), ' (', unit_of_measure, ')'), "\n",
          "Year: ", as.character(df$year),"\n",
          "Indicator: ", as.character(indicator),"\n",
          "Data source: ", as.character(df$referenceid_list),
          sep="") %>%
          lapply(htmltools::HTML)
        # number of countries
        plot_height <- ceiling(((length(unique(df$country))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        p <- ggplot(df, aes(x=value,
                            y=country,
                            text =mytext)) +
          geom_line(aes(group = country), color = 'black', size = 0.25) +
          geom_point(size=rel(2), alpha = 0.7, aes(color = variable)) +
          scale_color_manual(name = '',
                             values = col_vec) +
          scale_x_continuous(position = 'top',limits = c((value_range[1]), (value_range[2] + 5)), 
                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                             expand = c(0,0)) +
          labs(
              # title=plot_title,
              subtitle = sub_title, 
              x = y_axis_text, 
              y = 'Country'
              ) 
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_hjust = 0.5,
                                    y_axis_hjust = 1,
                                    y_axis_vjust = 0.5)+
          theme(plot.title = element_text(margin = margin(0,0,30,0))) +
          theme(
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
          )
        
        fig <- ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          config(displayModeBar = F)%>%
          layout(xaxis = list(side ="top" ), margin = list(t=100))  
        fig 
      }
    }
  })
}

# -----------------------------------------------------------------------------------------------------------------------------

# UI QUINTILES INDICATOR
mod_dots_ind_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(8,
             uiOutput(ns('dots_ind_title')),
             p(class='note_class', 'Chart only displays indicators for which data are available'),
             tags$div(style='overflow-y: scroll; position: relative', 
                      plotlyOutput(ns('dots_ind'), height = '600px', width = '2500px') )),
      column(3,
             class="selectizeWidth",
             #useShinyalert(),
             actionButton(ns("plot_info"), label = "Plot Info"),
             actionButton(ns('generate_chart'),label = 'Generate chart'),
             # actionButton(ns('share_chart'), 'Share chart'),

             br(), br(),
             p('Indicator'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                         label = 'Select the indicator', 
                                         status = "danger",
                                         actionButton(ns("all_inds"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;',checkboxGroupInput(inputId = ns("indicator"),
                         label = NULL, 
                         choices = indicators$indicator_short_name,
                         selected = indicators$indicator_short_name))),
             checkboxInput(ns('only_percent_measure'), 'Show only % indicators', value = TRUE, width = NULL),
             uiOutput(ns('ui_outputs')),
             p('Date range'),
             sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER QUINTILES INDICATOR
mod_dots_ind_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Quintiles - Most recent value by indicator", 
               text = "This chart allows users to explore differences in inequalities by household wealth across HEFPI indicators within a country. For instance, the chart reveals if a country achieves universal coverage of maternal and child health services while failing to enable equitable access to inpatient care. For every HEFPI indicators and country the user selects, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # # ---- GENERATE UI OUTPUTS---- #
  output$ui_outputs <- renderUI({
    #date_range <- c(1982, 2018)
    #indicator <- indicators$indicator_short_name[1:2]
    #country_names = 'Afghanistan'
    date_range <- input$date_range
    indicator <- input$indicator
    percent_measure <- input$only_percent_measure
    #country_names <- input$country

    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name %in% indicator) %>%
      .$variable_name
    # subset by country and variable
   df <- hefpi::hefpi_df %>%
      #filter(country == country_names) %>%
      filter(indic %in% variable) %>%
      filter(year >= date_range[1],
             year <= date_range[2])
    # get year and keep only necessary columns
    df <- df %>%
      group_by(country, indicator_short_name) %>%
      arrange(desc(year)) %>%
      dplyr::filter(year == dplyr::first(year))

    # made data long form
    id_vars <- names(df)[!grepl('Q', names(df))]
    df <- melt(df, id.vars = id_vars)
    # recode Quintiels
    df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                          ifelse(df$variable == 'Q2', 'Q2: Poor',
                                 ifelse(df$variable == 'Q3', 'Q3: Middle',
                                        ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
    # only keep data with no NAs
    df <- df[!is.na(df$value),]
    country_names <- sort(unique(df$country))
    #df <- df[complete.cases(df),]
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)

    # get min max of percent (*100)
    df <- df %>% filter(unit_of_measure=='%')
    max_per <- round(max(df$value*100), 2)
    min_per <- round(min(df$value*100), 2)

    # evaulate together
    min_value <- min(min_value, min_per)
    max_value <- max(max_value, max_per)

    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = 0
      max_value = ceiling(max_value) + 3
    }
    
    if(percent_measure) {
      fluidPage(
        fluidRow(
          p('Country'),
          selectInput(session$ns('country'),
                      label = NULL,
                      choices = country_names,
                      selected = 'Albania'),
          p('X axis range'),
          sliderInput(session$ns('value_range'),
                      label = NULL,
                      min = 0,
                      max = 100,
                      value = c(0, 100),
                      step = 1,
                      sep = '')
        )
      )
    } else {
      fluidPage(
        fluidRow(
          p('Country'),
          selectInput(session$ns('country'),
                      label = NULL,
                      choices = country_names,
                      selected = 'Albania'),
          p('X axis range'),
          sliderInput(session$ns('value_range'),
                      label = NULL,
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value),
                      sep = '')
        )
      )
    }
    

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
    # get inputs 
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    # condition for temporarily null objects from render UI
    if(is.null(value_range)){
      NULL
    } else {
      dot_list <- list()
      # Get the variable
      ind_info <- indicators %>%
        dplyr::filter(indicator_short_name %in% indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
      # subset by country and variable
      temp <- hefpi::hefpi_df %>%
        filter(country == country_names) %>%
        filter(indic %in% variable_name) %>%
        filter(year >= date_range[1],
               year <= date_range[2]) 
      # get year and keep only necessary columns
      df <- temp %>%
        group_by(country, indicator_short_name) %>%
        arrange(desc(year)) %>%
        dplyr::filter(year == dplyr::first(year)) 
      
      
      # made data long form
      id_vars <- names(df)[!grepl('Q', names(df))]
      df <- melt(df, id.vars = id_vars)
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      # only keep data with no NAs
      df <- df[!is.na(df$value),]
      #df <- df[complete.cases(df),]
      
      if(nrow(df) !=0){
        if(length(unique(df$unit_of_measure)) == 1 & unique(df$unit_of_measure) == '%'){
          # make percent 
          df$value[df$unit_of_measure == '%'] <- (df$value[df$unit_of_measure == '%'])*100
          # if the dataframe is null of empty make plot null
          value_range[2] <- value_range[2]*100
          value_range[1] <- value_range[1]*100
          df <- df %>% filter(value >= value_range[1],
                              value <= value_range[2])
        } else {
          # make percent 
          df$value[df$unit_of_measure == '%'] <- (df$value[df$unit_of_measure == '%'])*100
          df <- df %>% filter(value >= value_range[1],
                              value <= value_range[2])
        }
      }
      
      # order indicator alphabetically
      # df$indicator_short_name <- factor(df$indicator_short_name,levels= sort(unique(df$indicator_short_name), decreasing = TRUE ))
      df$indicator_short_name <- factor(df$indicator_short_name,levels = sort(unique(df$indicator_short_name), decreasing = TRUE ))
      
      if(input$only_percent_measure) {
        df <- df %>%
          filter(str_detect(unit_of_measure, '%'))
      } 
      
      
      dot_list <- list(df, unit_of_measure, indicator, date_range, value_range)
    }
    chart_data$plot_data <- dot_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("quintile_indicator_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      dot_list <- chart_data$plot_data
      if(length(dot_list)==1){
        dot_list <- hefpi::dots_indicator_default
        dot_list[[1]] <- dot_list[[1]] %>%
          filter(str_detect(unit_of_measure, '%'))
      }
      if(is.null(dot_list)){
        NULL
      } else {
        df <- dot_list[[1]]
        if(nrow(df) == 0){
          temp <- data_frame()
          
        } else {
          temp <- df
          names(temp) <- tolower(names(temp))
          names(temp)[names(temp)=='variable'] <- 'level'
          # subset by  
          temp$parameter <- 'Mean'
          # temp$level <- 'National'
          temp <- temp %>% select(region_name, country, iso3c, year,referenceid_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, ci, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid',
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2021'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
        }
        
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0("quintile_indicator_", Sys.Date(),".png"),
                                    content = function(file) {
                                      dot_list <- chart_data$plot_data
                                      if(length(dot_list)==1){
                                        dot_list <- hefpi::dots_indicator_default
                                        dot_list[[1]] <- dot_list[[1]] %>%
                                          filter(str_detect(unit_of_measure, '%'))
                                      }
                                      if(is.null(dot_list)){
                                        NULL
                                      } else {
                                        df <- dot_list[[1]]
                                        unit_of_measure <- dot_list[[2]]
                                        indicator <- dot_list[[3]]
                                        date_range <- dot_list[[4]]
                                        value_range <- dot_list[[5]]
                                        # get color graident 
                                        # col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
                                        # col_vec <- col_vec[-1]
                                        col_vec <- c("#006e38", "#75a56e","#a89fe1", "#6d60bb", "#312271")
                                        
                                        # get length of variable 
                                        col_length <- length(unique(df$variable))
                                        # make plot title 
                                        plot_title = paste0('Quintiles - Most recent value by indicator', ' - ', unique(df$country))
                                        sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
                                        y_axis_text = paste0(indicator)
                                        caption_text = 'HEFPI database, The World Bank, 2021'
                                        
                                        
                                        # number of countries
                                        plot_height <- ceiling(((length(unique(df$indicator_short_name))* 100) + 100)/3)
                                        if(plot_height < 250){
                                          plot_height <- 250
                                        }
                                        p <- ggplot(df, aes(x=indicator_short_name,
                                                            y=value,
                                                            color = variable)) +
                                          geom_point(size=rel(2), alpha = 0.7) +
                                          scale_color_manual(name = '',
                                                             values = col_vec) +
                                          geom_line(aes(group = indicator_short_name)) +
                                          scale_y_continuous(limits = c((value_range[1]), (value_range[2] +5)), 
                                                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                                                             expand = c(0,0)) +
                                          labs(title='',
                                               x = y_axis_text, 
                                               y = '',
                                               subtitle = '',
                                               caption = caption_text) +
                                          coord_flip()
                                        
                                        p <-  p + hefpi::theme_hefpi(grid_major_x = NA,
                                                             y_axis_size = rel(2/3),
                                                             x_axis_size = rel(1),
                                                             x_axis_hjust = 0.5,
                                                             y_axis_hjust = 1,
                                                             y_axis_vjust = 0.5) +
                                          theme(
                                            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            panel.grid.major.x = element_blank(),
                                            panel.grid.minor.x = element_blank(),
                                            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                                          ) +
                                          labs(title = '',
                                               subtitle ='',
                                               x = y_axis_text, 
                                               y = '')
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                    })

  
  
  # ---- GENERATE TITLE dot_ind  ---- #
  output$dots_ind_title <- renderUI({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_indicator_default
      dot_list[[1]] <- dot_list[[1]] %>%
        filter(str_detect(unit_of_measure, '%'))
    }
    if(is.null(dot_list)){
      NULL
    } else {

        df <- dot_list[[1]]
        date_range <- dot_list[[4]]
        value_range <- dot_list[[5]]

        # make plot title 
        # plot_title = paste0('Quintiles - Most recent value by indicator', ' - ', unique(df$country))
        # sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
        plot_title <- HTML(str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Quintiles </div> 
                           <div class="chart-label"> Most recent value by indicator </div>
                           <div class="chart-label"> {unique(df$country)} </div>
                          </div>
                          '))
        
      
        plot_title 
      
    }
  })
  
    
  # ---- GENERATE PLOT ---- #
  output$dots_ind <- renderPlotly({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_indicator_default
      dot_list[[1]] <- dot_list[[1]] %>%
        filter(str_detect(unit_of_measure, '%'))
      
    }
    if(is.null(dot_list)){
      NULL
    } else {
      df <- dot_list[[1]]
      if(nrow(df)==0){
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
        # save(dot_list, file = 'temp_dots_for_def.rda')
        df <- dot_list[[1]]
        unit_of_measure <- dot_list[[2]]
        indicator <- dot_list[[3]]
        date_range <- dot_list[[4]]
        value_range <- dot_list[[5]]
        
        message("Result value_range: ")
        print(value_range)
        
        if(value_range[2] == 10000) {
           value_range[2] <- 100 
        }
        # get color graident 
        # col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
        # col_vec <- col_vec[-1]
        col_vec <- c("#006e38", "#75a56e","#a89fe1", "#6d60bb", "#312271")
        # get length of variable 
        col_length <- length(unique(df$variable))
        # make plot title 
        # plot_title = paste0('Quintiles - Most recent value by indicator', ' - ', unique(df$country))
        sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
        y_axis_text = paste0(indicator)
        
        mytext <- paste(
          "Economy: ", as.character(df$country),"\n",
          "Value: ", paste0(round(df$value, digits = 2), ' (', df$unit_of_measure, ')'), "\n",
          "Year: ", as.character(df$year),"\n",
          "Data source: ", as.character(df$referenceid_list),
          sep="") %>%
          lapply(htmltools::HTML)
        
        # number of countries
        plot_height <- ceiling(((length(unique(df$indicator_short_name))* 100) + 100)/3)
        if(plot_height < 250){
          plot_height <- 250
        }
        p <- ggplot(df, aes(x=value,
                            y=indicator_short_name,
                            color = variable, 
                            text = mytext)) +
          scale_color_manual(name = '',
                             values = col_vec) +
          geom_line(aes(group = indicator_short_name), color = 'black', size = 0.25) +
          geom_point(size=rel(2), alpha = 0.7) +
          scale_x_continuous(position = 'top', 
                             limits = c((value_range[1]), (value_range[2] +5)), 
                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                             expand = c(0,0)) +
          # coord_flip() +
          labs(
               # title=plot_title, 
               x = y_axis_text, 
               y = '',
               subtitle = sub_title
               ) 
        
        
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_hjust = 0.5,
                                    y_axis_hjust = 1,
                                    y_axis_vjust = 0.5) +
                        theme(
                          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                          axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                          )
        
        fig <- ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          config(displayModeBar = F)%>%
          layout(xaxis = list(side ="top" ), margin = list(t=100))  
        fig 
        
      }
     
    }
  })
}
