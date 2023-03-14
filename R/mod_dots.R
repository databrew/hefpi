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
  ns <- shiny::NS(id)
  tagList(
    shiny::fluidPage(
      shiny::column(9,
                    shiny::uiOutput(ns('dots_country_title')),
                 tags$div(style='overflow-y: scroll; position: relative; min-height: 800px', plotly::plotlyOutput(ns('dots_country'), height = '600px', width = '100%') )
      ),
      shiny::column(3,
             class="selectizeWidth",
             #useShinyalert(),
             shiny::actionButton(ns("plot_info"), label = "Plot Info"),
             shiny::actionButton(ns('generate_chart'),label = 'Generate chart'),
             # actionButton(ns('share_chart'), 'Share chart'),
             br(), br(),
             p('Indicator'),
             shiny::selectInput(inputId = ns("indicator"),
                         label = NULL, 
                         choices = indicators_list,
                         selected = "4+ antenatal care visits (%)"),
             p('Region'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                          label = 'Select the region(s)', 
                                          status = "danger",
                                          actionButton(ns("all_regions"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;', shiny::checkboxGroupInput(inputId = ns("region"),
                         label = '', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)))),
             shiny::uiOutput(ns('ui_outputs')),
             shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER QUINTILES COUNTRY
mod_dots_country_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  shiny::observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(title = "Quintiles - Most recent value by country", 
               text = "This chart enables users to compare inequalities in HEFPI indicators by household wealth, both within and across countries. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey. For a set of countries and an indicator the user specifies, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GET UI OUTPUTS ---- #
  output$ui_outputs <- shiny::renderUI({
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
      dplyr::filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4) & !is.na(Q5)) %>%
      dplyr::select(year, country, referenceid_list,indic, Q1:Q5) 
    # made data long form
    df <- melt(df, id.vars = c('year', 'country', 'referenceid_list', 'indic'))
    max_value <- round(max(df$value), 2)
    min_value <- round(min(df$value), 2)
    if(max_value <= 1){
      min_value = 0
      max_value = 100
    } else {
      min_value = 0
      max_value = ceiling(max_value)
    }
    # create select input for country
    countries <- unique(df$country)
    # ui inputs
    shiny::fluidPage(
      shiny::fluidRow(
        p('Country'),
        shinyWidgets::dropdownButton(circle = FALSE,  
                                     label = 'Select the countries', 
                                     status = "danger",
                                     shiny::actionButton(session$ns("all_countries"), label="Select/Deselect all"),
        div(style='max-height: 30vh; overflow-y: auto;', shiny::checkboxGroupInput(inputId = session$ns("country"),
                    label = NULL, 
                    choices = countries,
                    selected = countries))),
        p('X axis range'),
        shiny::sliderInput(session$ns('value_range'),
                    label = NULL,
                    min = min_value,
                    max = max_value,
                    value = c(min_value, max_value),
                    sep = ''),
        p('Date range'),
        shiny::sliderInput(session$ns('date_range'),
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
  shiny::observe({
    all_regions <- input$all_regions
    message(all_regions)
    if(any(is.null(all_regions))){
      NULL
    } else {
      if (all_regions > 0) {
        if (all_regions %% 2 == 0){
          message(region_list$region)
          shiny::updateCheckboxGroupInput(session=session,
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = as.character(region_list$region))
          
        } else {
          shiny::updateCheckboxGroupInput(session=session,  
                                   inputId ="region",
                                   choices = as.character(region_list$region),
                                   selected = c())
          
        }}
    }
    
  })
  
  
  
  # COUNTRY
  shiny::observe({
    all_countries <- input$all_countries
    message(all_countries)
    if(any(is.null(all_countries))){
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
          dplyr::filter(!is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4) & !is.na(Q5)) %>%
          dplyr::select(year, country, referenceid_list,indic, Q1:Q5) 
       
        # create select input for country
        countries <- unique(df$country)
        if (all_countries %% 2 == 0){
          
          shiny::updateCheckboxGroupInput(session=session,
                                   "country",
                                   choices = countries,
                                   selected = countries)
          
        } else {
          shiny::updateCheckboxGroupInput(session=session, 
                                   "country",
                                   choices = countries,
                                   selected = c())
          
        }}
    }
  
  })
  
  chart_data <- shiny::reactiveValues(plot_data = 'new') 
  shiny::observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get input data
    region <- input$region
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    date_range <- input$date_range
    # condition for ui being temorarily null
    if(any(is.null(value_range))){
      NULL
    } else {
      dot_list <- list()
      # Get the variable
      ind_info <- indicators %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        dplyr::select(variable_name, unit_of_measure)
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
      # subset by country and variable
      temp <- hefpi::hefpi_df %>%
        dplyr::filter(country %in% country_names) %>%
        dplyr::filter(indic == variable_name) %>%
        dplyr::filter(year >= date_range[1],
                      year <= date_range[2]) 
      # get year and keep only necessary columns
      df <- temp %>%
        dplyr::group_by(country) %>%
        dplyr::arrange(desc(year)) %>%
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
      df <- df %>% dplyr::filter(value >= value_range[1],
                          value <= value_range[2])
      if(unit_of_measure == '%'){
        df$value <- df$value*100
        # value_range[2] <- value_range[2]*100
        # value_range[1] <- value_range[1]*100
        value_range[2] <- value_range[2]
        value_range[1] <- value_range[1]
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
  output$dl_data <- shiny::downloadHandler(
    filename = function() {
      paste0("quintile_country_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get map
      dot_list <- chart_data$plot_data
      if(length(dot_list)==1){
        dot_list <- hefpi::dots_country_default
      }
      if(any(is.null(dot_list))){
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
          temp <- temp %>% dplyr::select(region_name, country, iso3c, year,referenceid_list, indic, indicator_short_name,
                                  indicator_description, parameter, level, ci, unit_of_measure)
          names(temp) <- c('Region', 'Country_name', 'Country_iso3', 'Year', 'Referenceid', 
                           'Indicator', 'Indicator_short_name', 'Indicator_long_name', 'Parameter', 'Level', 
                           'Value', 'Unit_of_measurement')
          temp_stamp <- temp[1,]
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2022'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid<- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
        }
        write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- shiny::downloadHandler(filename = paste0("quintile_country_", Sys.Date(),".png"),
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
                                        caption_text = 'HEFPI database, The World Bank, 2022'

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
                                        p <- ggplot2::ggplot(df, ggplot2::aes(x=country,
                                                            y=value)) +
                                          ggplot2::geom_point(size=rel(2), alpha = 0.7, ggplot2::aes(color = variable)) +
                                          ggplot2::geom_line(ggplot2::aes(group = country), color = 'black', size = 0.25) +
                                          ggplot2::scale_color_manual(name = '',
                                                             values = col_vec) +
                                          ggplot2::scale_y_continuous(limits = c((value_range[1]), (value_range[2] + 5)),
                                                             breaks = seq(from = value_range[1],to = value_range[2], by = 10),
                                                             expand = c(0,0)) +
                                          ggplot2::labs(title='',
                                               subtitle = '',
                                               x = 'Country',
                                               y = 'Value',
                                               caption=caption_text) +
                                          ggplot2::coord_flip()

                                        p <- p +
                                          hefpi::theme_hefpi(grid_major_x = NA,
                                                                    y_axis_size = rel(2/3),
                                                                    x_axis_size = rel(1),
                                                                    x_axis_hjust = 0.5,
                                                                    y_axis_hjust = 1,
                                                                    y_axis_vjust = 0.5) +
                                                          ggplot2::theme(
                                                            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            panel.grid.major.x = element_blank(),
                                                            panel.grid.minor.x = element_blank(),
                                                            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                                            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                                                          ) +
                                                          ggplot2::labs(title = '',
                                                               subtitle = '',
                                                               x = 'Country',
                                                               y = 'Value'
                                                               )
                                        p
                                        ggplot2::ggsave(file, width = 8, height = 8)
                                      }
                                    })

  
  # ---- PLOT TITLE dots_country ---- #
  output$dots_country_title <- shiny::renderUI({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_country_default
    }
    if(any(is.null(dot_list))){
      NULL
    } else {
      df <- dot_list[[1]]

      unit_of_measure <- dot_list[[2]]
      indicator <- dot_list[[3]]

      # make plot title 
      # plot_title = paste0('Quintiles - Most recent value by country', ' - ', indicator)
      plot_title <- HTML(stringr::str_glue('
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
  output$dots_country <- plotly::renderPlotly({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_country_default
    }
    if(any(is.null(dot_list))){
      NULL
    } else {
      df <- dot_list[[1]]
      if(nrow(df)==0){
        empty_plot <- function(title = NULL){
          p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            plotly::layout(
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
        plot_height <- ceiling(length(unique(df$country)) * 17)
        if(plot_height < 250){
          plot_height <- 500
        }
        p <- ggplot2::ggplot(df, ggplot2::aes(x=value,
                            y = country,
                            text =mytext)) +
          ggplot2::geom_line(ggplot2::aes(group = country), color = 'black', size = 0.25) +
          ggplot2::geom_point(size=rel(2), alpha = 0.7, ggplot2::aes(color = variable)) +
          ggplot2::scale_color_manual(name = '',
                             values = col_vec) +
          ggplot2::scale_x_continuous(position = 'top',limits = c((value_range[1]), (value_range[2] + 5)), 
                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                             expand = c(0,0)) +
          ggplot2::labs(
              # title=plot_title,
              subtitle = sub_title, 
              x = y_axis_text, 
              y = 'Country'
              ) 
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_hjust = 0.5,
                                    y_axis_hjust = 1,
                                    y_axis_vjust = 0.5,
                                    # legend_position = "top",
                                    # legend_direction = "horizontal"
                                    )+
          ggplot2::theme(plot.title = element_text(margin = margin(0,0,30,0))) +
          ggplot2::theme(
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
            # legend.position = "top",
            # legend.direction = "horizontal"
          )
        
        # general_height <- 2901
        # countries_selected <- length(unique(df$country))
        # print(length(unique(df$country)))
        current_height <- ceiling(length(unique(df$country)) * 17)
        # print(current_height)
        
        fig <- plotly::ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          plotly::config(displayModeBar = F)%>%
          plotly::layout(xaxis = list(side ="top"), margin = list(t=100), height = plot_height)  
        fig 
      }
    }
  })
}

# -----------------------------------------------------------------------------------------------------------------------------
add_break_custom <- function(ind_name) {
  # if(nchar(ind_name) > 35 & nchar(ind_name) < 62) {
  #   ind_name_split <- ind_name
  #   ind_name <- as.character(str_glue('{substr(ind_name_split, 1, 35)}\n - {substr(ind_name_split, 36, nchar(ind_name_split))}'));
  #   return(ind_name)
  # } else {
  #   if(nchar(ind_name) >= 62) {
  #     ind_name_split <- ind_name
  #     ind_name <- as.character(str_glue('{substr(ind_name_split, 1, 35)}\n - {substr(ind_name_split, 36, 50)}\n - {substr(ind_name_split, 51, nchar(ind_name_split))}'));
  #     return(ind_name)
  #   } else {
  #     return(ind_name) 
  #   }
  # }
  ind_name <- paste(strwrap(ind_name, 40), collapse="\n")
  return(ind_name)
}

# UI QUINTILES INDICATOR
mod_dots_ind_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::fluidPage(
      shiny::column(9,
                    shiny::uiOutput(ns('dots_ind_title')),
             p(class='note_class', 'Chart only displays indicators for which data are available'),
             tags$div(style='overflow-y: scroll; position: relative; min-height: 2000px', 
                      plotly::plotlyOutput(ns('dots_ind'), height = '600px', width = '100%') )),
      shiny::column(3,
             class="selectizeWidth",
             #useShinyalert(),
             shiny::actionButton(ns("plot_info"), label = "Plot Info"),
             shiny::actionButton(ns('generate_chart'),label = 'Generate chart'),
             # actionButton(ns('share_chart'), 'Share chart'),

             br(), br(),
             p('Indicator'),
             shinyWidgets::dropdownButton(circle = FALSE,  
                                         label = 'Select the indicator', 
                                         status = "danger",
                                         actionButton(ns("all_inds"), label="Select/Deselect all"),
             div(style='max-height: 30vh; overflow-y: auto;', shiny::checkboxGroupInput(inputId = ns("indicator"),
                         label = NULL, 
                         choices = sort(hefpi::indicators__v2_tbl$indicator_short_name),
                         selected = sort(hefpi::indicators__v2_tbl$indicator_short_name)
                         )
                 )
             ),
             # shiny::checkboxInput(ns('only_percent_measure'), 'Show only % indicators', value = TRUE, width = NULL),
             
             p('Country'),
             shiny::selectInput(ns('country'),
                                label = NULL,
                                choices = NULL,
                                selected = NULL),
             p('X axis range'),
             shiny::sliderInput(ns('value_range'),
                                label = NULL,
                                min = 0,
                                max = 100,
                                value = c(0, 100),
                                sep = ''),
             
             # shiny::uiOutput(ns('ui_outputs')),
             p('Date range'),
             shiny::sliderInput(ns('date_range'),
                         label = NULL,
                         min = 1982,
                         max = 2021,
                         value = c(1982, 2021),
                         step = 1,
                         sep = ''),
             shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'))
    )
  )
}

# SERVER QUINTILES INDICATOR
mod_dots_ind_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  shiny::observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(title = "Quintiles - Most recent value by indicator", 
               text = "This chart allows users to explore differences in inequalities by household wealth across HEFPI indicators within a country. For instance, the chart reveals if a country achieves universal coverage of maternal and child health services while failing to enable equitable access to inpatient care. For every HEFPI indicators and country the user selects, the chart shows mean indicator values for each wealth quintile. Greater distance between the poor and rich on the chart’s horizontal axis indicates more severe inequality. How wealth is measured for a data point – by a wealth index, consumption, or income – depends on the underlying survey.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  # ---- GENERATE UI OUTPUTS ---- #
  # ---- GENERATE UI OUTPUTS ---- #
  shiny::observe({
    # shiny::req(input$indicator)
    # shiny::req(input$only_percent_measure)
    
    #date_range <- c(1982, 2018)
    #indicator_selected <- unique(hefpi::indicators$indicator_short_name)[1:2]
    #country_names = 'Albania'
    date_range <- input$date_range
    indicator_selected <- input$indicator
    # percent_measure <- input$only_percent_measure
    #country_names <- input$country
    
    # message(input$indicator[1])
    # message(date_range)
    # message(indicator[2])
    # message(percent_measure)
    
    # Get the variable
    # variable <- hefpi::indicators %>%
    #   dplyr::filter(indicator_short_name %in% indicator_selected) %>%
    #   .$variable_name
    
    # subset by country and variable
    df <- hefpi::hefpi_df %>%
      # dplyr::filter(country == country_names) %>%
      # dplyr::filter(indic %in% variable) %>%
      # Filter dataset based on the input
      dplyr::filter(indicator_short_name %in% indicator_selected) %>%
      dplyr::filter(dplyr::between(year, min(as.numeric(date_range)), max(as.numeric(date_range)))) %>%
      # get year and keep only necessary columns
      dplyr::group_by(country, indicator_short_name) %>%
      dplyr::arrange(desc(year)) %>%
      dplyr::filter(year == dplyr::first(year)) %>%
      dplyr::ungroup()
    
    
    # # get year and keep only necessary columns
    # df <- df %>%
    #   dplyr::group_by(country, indicator_short_name) %>%
    #   dplyr::arrange(desc(year)) %>%
    #   dplyr::filter(year == dplyr::first(year)) %>%
    #   ungroup()
    
    # made data long form
    id_vars <- names(df)[!grepl('Q', names(df))]
    df <- reshape2::melt(df, id.vars = id_vars)
    
    # recode Quintiels
    df <- df %>%
      dplyr::mutate(variable = ifelse(variable == 'Q1', 'Q1: Poorest', variable)) %>%
      dplyr::mutate(variable = ifelse(variable == 'Q2', 'Q2: Poor', variable)) %>%
      dplyr::mutate(variable = ifelse(variable == 'Q3', 'Q3: Middle', variable)) %>%
      dplyr::mutate(variable = ifelse(variable == 'Q4', 'Q4: Richer', variable)) %>%
      dplyr::mutate(variable = ifelse(variable == 'Q5', 'Q5: Richest', variable)) %>%
      tidyr::as_tibble() %>%
      tidyr::drop_na(value)
    
    
    # df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
    #                       ifelse(df$variable == 'Q2', 'Q2: Poor',
    #                              ifelse(df$variable == 'Q3', 'Q3: Middle',
    #                                     ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
    
    
    # only keep data with no NAs
    # df <- df[!is.na(df$value),]
    country_names <- sort(unique(df$country))
    #df <- df[complete.cases(df),]
    
    # max_value <- round(max(df$value), 2)
    # min_value <- round(min(df$value), 2)
    
    # get min max of percent (*100)
    df <- df %>% dplyr::filter(unit_of_measure == '%')
    # max_per <- round(max(df$value*100), 2)
    # min_per <- round(min(df$value*100), 2)
    
    # evaulate together
    # min_value <- min(min_value, min_per)
    # max_value <- max(max_value, max_per)
    # min_value <- 0
    # max_value <- 100
    
    # print(max_value)
    
    # if(max_value <= 1){
    #   min_value <- 0
    #   max_value <- 1
    # } else {
    #   min_value <- 0
    #   max_value <- ceiling(max_value) + 3
    # }
    
    
    # print(max_value)
    
    shiny::updateSelectInput(session, 'country', choices = country_names, selected = 'Albania')
    
    # if(percent_measure) {
      shiny::updateSliderInput(session, 
                               'value_range', 
                               min = 0, 
                               max = 100, 
                               value = c(0, 100), 
                               step = 1)
    # } else {
    #   shiny::updateSliderInput(session, 
    #                            'value_range', 
    #                            min = min_value, 
    #                            max = max_value, 
    #                            value = c(min_value, max_value), 
    #                            step = 1)
    # }
    
    
  })
  
  
  
  # ---- SELECT/DESLECT ALL BUTTONS ---- #
  # INDICATORS
  shiny::observe({
    all_inds <- input$all_inds
    message(all_inds)
    if(any(is.null(all_inds))){
      NULL
    } else {
      if (all_inds > 0) {
        if (all_inds %% 2 == 0){
          shiny::updateCheckboxGroupInput(session = session,
                                   inputId ="indicator",
                                   choices = sort(hefpi::indicators__v2_tbl$indicator_short_name),
                                   selected = sort(hefpi::indicators__v2_tbl$indicator_short_name)
          )

        } else {
          shiny::updateCheckboxGroupInput(session=session,
                                   inputId ="indicator",
                                   choices = sort(hefpi::indicators__v2_tbl$indicator_short_name),
                                   selected = c()
                                   )

        }}
    }

  })
  
  
  chart_data <- shiny::reactiveValues(plot_data = 'new') 
  shiny::observeEvent(input$generate_chart, {
    message('The "generate chart" button has been clicked on the Population Mean - Trends - National Mean tab.')
    # get inputs 
    date_range <- input$date_range
    indicator <- input$indicator
    country_names <- input$country
    value_range <- input$value_range
    # condition for temporarily null objects from render UI
    if(any(is.null(value_range))){
      NULL
    } else {
      dot_list <- list()
      # Get the variable
      ind_info <- hefpi::indicators__v2_tbl %>%
        dplyr::filter(indicator_short_name %in% indicator) %>%
        dplyr::select(variable_name, unit_of_measure)
      variable_name <- ind_info$variable_name
      unit_of_measure <- ind_info$unit_of_measure
      # subset by country and variable
      temp <- hefpi::hefpi_df %>%
        dplyr::filter(country == country_names) %>%
        dplyr::filter(indic %in% variable_name) %>%
        dplyr::filter(year >= date_range[1],
               year <= date_range[2]) 
      # get year and keep only necessary columns
      df <- temp %>%
        dplyr::group_by(country, indicator_short_name) %>%
        dplyr::arrange(desc(year)) %>%
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
          # value_range[2] <- value_range[2]*100
          # value_range[1] <- value_range[1]*100
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
      
      # Keep only % value
      # if(input$only_percent_measure) {
        df <- df %>%
          filter(str_detect(unit_of_measure, '%'))
      # } 
      
      
      dot_list <- list(df, unit_of_measure, indicator, date_range, value_range)
      # dots_indicator_default <- dot_list
      # save(dots_indicator_default, file = 'data/dots_indicator_default.rda')
    }
    chart_data$plot_data <- dot_list
  },
  
  ignoreNULL = FALSE,
  ignoreInit = TRUE)
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- shiny::downloadHandler(
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
      if(any(is.null(dot_list))){
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
          temp_stamp$Region <- 'HEFPI database, The World Bank, 2022'
          temp_stamp$Country_name <- temp_stamp$Country_iso3 <- temp_stamp$Year <- temp_stamp$Referenceid <- temp_stamp$Indicator <- temp_stamp$Indicator_short_name <- temp_stamp$Indicator_long_name <- temp_stamp$Parameter <- temp_stamp$Level <- temp_stamp$Value <- temp_stamp$Unit_of_measurement <- ''
          temp <- rbind(temp, temp_stamp)
        }

        # write.csv(temp, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- shiny::downloadHandler(filename = paste0("quintile_indicator_", Sys.Date(),".png"),
                                    content = function(file) {
                                      dot_list <- chart_data$plot_data
                                      if(length(dot_list)==1){
                                        dot_list <- hefpi::dots_indicator_default
                                        dot_list[[1]] <- dot_list[[1]] %>%
                                          filter(str_detect(unit_of_measure, '%'))
                                      }
                                      if(any(is.null(dot_list))){
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
                                        caption_text = 'HEFPI database, The World Bank, 2022'


                                        # number of countries
                                        plot_height <- ceiling(((length(unique(df$indicator_short_name))* 100) + 100)/3)
                                        if(plot_height < 250){
                                          plot_height <- 500
                                        }
                                        p <- ggplot2::ggplot(df, ggplot2::aes(x=indicator_short_name,
                                                            y=value,
                                                            color = variable)) +
                                          ggplot2::geom_point(size=rel(2), alpha = 0.7) +
                                          ggplot2::scale_color_manual(name = '',
                                                             values = col_vec) +
                                          ggplot2::geom_line(ggplot2::aes(group = indicator_short_name)) +
                                          ggplot2::scale_y_continuous(limits = c((value_range[1]), (value_range[2] +5)),
                                                             breaks = seq(from = value_range[1],to = value_range[2], by = 10),
                                                             expand = c(0,0)) +
                                          ggplot2::labs(title='',
                                               x = 'Value',
                                               y = 'Indicator',
                                               subtitle = '',
                                               caption = caption_text) +
                                          ggplot2::coord_flip()

                                        p <-  p + hefpi::theme_hefpi(grid_major_x = NA,
                                                             y_axis_size = rel(2/3),
                                                             x_axis_size = rel(1),
                                                             x_axis_hjust = 0.5,
                                                             y_axis_hjust = 1,
                                                             y_axis_vjust = 0.5) +
                                          ggplot2::theme(
                                            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            panel.grid.major.x = element_blank(),
                                            panel.grid.minor.x = element_blank(),
                                            axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                                            axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc")
                                          ) +
                                          ggplot2::labs(title = '',
                                               subtitle ='',
                                               x = 'Indicator',
                                               y = 'Value')
                                        p
                                        ggplot2::ggsave(file, width = 8, height = 8)
                                      }
                                    })

  
  
  # ---- GENERATE TITLE dot_ind  ---- #
  output$dots_ind_title <- shiny::renderUI({
    dot_list <- chart_data$plot_data
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_indicator_default
      dot_list[[1]] <- dot_list[[1]] %>%
        dplyr::filter(str_detect(unit_of_measure, '%'))
    }
    if(any(is.null(dot_list))){
      NULL
    } else {

        df <- dot_list[[1]]
        date_range <- dot_list[[4]]
        value_range <- dot_list[[5]]

        # make plot title 
        # plot_title = paste0('Quintiles - Most recent value by indicator', ' - ', unique(df$country))
        # sub_title = paste0('time period: ', date_range[1], ' - ', date_range[2])
        plot_title <- HTML(stringr::str_glue('
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
  output$dots_ind <- plotly::renderPlotly({
    dot_list <- chart_data$plot_data
    
    
    if(length(dot_list)==1){
      dot_list <- hefpi::dots_indicator_default
      dot_list[[1]] <- dot_list[[1]] %>%
        dplyr::filter(str_detect(unit_of_measure, '%'))
      
    }
    if(any(is.null(dot_list))){
      NULL
    } else {
      df <- dot_list[[1]]
      if(nrow(df)==0){
        empty_plot <- function(title = NULL){
          p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            plotly::layout(
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
        
        if(value_range[2] > 10000) {
           value_range[2] <- 100 
        }
        
        if(value_range[2] == 1000) {
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
        # y_axis_text = paste0(indicator)
        
        mytext <- paste(
          "Economy: ", as.character(df$country),"\n",
          "Value: ", paste0(round(df$value, digits = 2), ' (', df$unit_of_measure, ')'), "\n",
          "Year: ", as.character(df$year),"\n",
          "Data source: ", as.character(df$referenceid_list),
          sep="") %>%
          lapply(htmltools::HTML)
        
        # number of countries
        plot_height <- ceiling(((length(unique(df$indicator_short_name))* 100) + 100)/3)
        if(plot_height < 200){
          plot_height <- 450
          y_margin <- 1.3
        } else {
          if(plot_height < 650) {
            y_margin <- 1.37
            plot_height <- 575
          } else {
            y_margin <- 1.1
          }
        }
        
        
        
        print(plot_height)
        print(y_margin)
        
        df <- df %>%
          as_tibble() %>%
          rowwise() %>%
          mutate(indicator_short_name = add_break_custom(as.character(indicator_short_name)))
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x=value,
                            y=indicator_short_name,
                            color = variable, 
                            text = mytext)) +
          ggplot2::scale_color_manual(name = '',
                             values = col_vec) +
          ggplot2::geom_line(aes(group = indicator_short_name), color = 'black', size = 0.25) +
          ggplot2::geom_point(size=rel(2), alpha = 0.7) +
          ggplot2::scale_x_continuous(position = 'top', 
                             limits = c((value_range[1]), (value_range[2] +5)), 
                             breaks = seq(from = value_range[1],to = value_range[2], by = 10), 
                             expand = c(0,0)) +
          # ggplot2::scale_y_discrete(labels = function(x) add_break_custom(x)) +
          # coord_flip() +
          ggplot2::labs(
               # title=plot_title, 
               title = '',
               x = '',
               y = ''
               # ,
               # subtitle = sub_title
               ) 
        
        
        p <- p + hefpi::theme_hefpi(grid_major_x = NA,
                                    x_axis_hjust = 0.5,
                                    y_axis_hjust = 1,
                                    y_axis_size = 9,
                                    x_axis_size = 8,
                                    y_axis_vjust = 0.5,
                                    legend_position = 'top',
                                    legend_direction = 'horizontal',
                                    legend_text_size = 0.75,
                                    
                                    ) +
          ggplot2::theme(
                          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                          axis.ticks = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          axis.line = element_line(size = 0.5, linetype = 'solid', colour = "#cccccc"),
                          legend.position = 'top',
                          legend.direction = 'horizontal',
                          
                          )
        
        fig <- plotly::ggplotly(p, 
                        tooltip = 'text', 
                        height = plot_height) %>%
          plotly::config(displayModeBar = F)%>%
          plotly::layout(
            height = plot_height,
            xaxis = list(side ="top" ), 
            # margin = list(t=100),
            legend = list(orientation = "h",   # show entries horizontally
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,
                          y = y_margin
                          )
            )  
        fig 
        
      }
     
    }
  })
}
