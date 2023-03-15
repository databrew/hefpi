# Module recent value subnational
#' @title mod_recent_sub.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @keywords internal
#' @export 
mod_rural_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(8,
                  shiny::uiOutput(ns('map_title_ui')),
                  plotly::plotlyOutput(
                   ns('recent_sub_mean_plot'), height = 550
                 )),
    shiny::column(4,
           p('Country'),
           shiny::selectInput(ns('country'), 
                       label = NULL,
                       choices = unique(hefpi::hefpi_df$country), selected = 'Morocco'),
           # sliderInput(ns('date_range'),
           #             label = 'Year',
           #             min = 1982,
           #             max = 2021,
           #             value = c(1982, 2021),
           #             step = 1,
           #             sep = ''),
           shiny::selectInput(ns('date_range'),
                       label = 'Year',
                       choices = NULL
                       ),
           shiny::selectInput(ns('indicator'),
                       label = 'Indicator',
                       choices = NULL
                       ),
           # uiOutput(ns('ind_ui')),
           shiny::uiOutput(ns('axis_ui')),
           shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
           shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary')
           
           )
    
  )
 
}

# SERVER FOR MOST RECENT VALUE SUBNATIONAL MEAN
mod_rural_server <- function(input, output, session){
  
  # # ---- UI output for region within country ---#
  # output$ind_ui <- renderUI({
  #   #cn = 'India'
  #   #plot_years = c(1982, 2017)
  #   req(input$country)
  #   
  #   cn = input$country
  #   plot_years <- input$date_range
  #   
  #   hefpi::hefpi_sub_df
  #   
  #   ind <- hefpi::hefpi_df %>% 
  #     as_tibble() %>% 
  #     select(country, year, regioncode, indic, urb, rur) %>%
  #     filter(country == cn) %>%
  #     # filter(year == plot_years) %>%
  #     left_join(
  #       hefpi::indicators %>% select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
  #       by = c('indic' = 'variable_name')
  #     ) %>%
  #     select(indicator_short_name) %>%
  #     pull() %>%
  #     unique() %>%
  #     sort()
  # 
  #   indicator_intersect <- indicators_list
  #   indicator_intersect$`Financial Protection` <- intersect(indicators_list$`Financial Protection`, ind) %>% as.list()
  #   indicator_intersect$`Healthcare Coverage` <- intersect(indicators_list$`Healthcare Coverage`, ind) %>% as.list()
  #   indicator_intersect$`Health Outcomes` <- intersect(indicators_list$`Health Outcomes`, ind) %>% as.list()
  #   
  #   fluidPage(
  #     fluidRow(
  #       selectInput(inputId = session$ns('indicator'),
  #                   label = 'Indicator', 
  #                   choices = indicator_intersect,
  #                   selected = indicator_intersect[[1]])
  # 
  #     )
  #   )
  #  
  #   
  # })
  
  shiny::observeEvent(input$country, {
    
    shiny::req(input$country)
    
    cn = input$country
    plot_years <- input$date_range
    
    hefpi::hefpi_sub_df
    
    ind <- hefpi::hefpi_df %>% 
      tidyr::as_tibble() %>% 
      dplyr::select(country, year, regioncode, indic, urb, rur) %>%
      dplyr::filter(country == cn) %>%
      # filter(year == plot_years) %>%
      dplyr::left_join(
        hefpi::indicators %>% dplyr::select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
        by = c('indic' = 'variable_name')
      ) %>%
      dplyr::select(indicator_short_name) %>%
      dplyr::pull() %>%
      unique() %>%
      sort()
    
    indicator_intersect <- indicators_list
    indicator_intersect$`Financial Protection` <- dplyr::intersect(indicators_list$`Financial Protection`, ind) %>% as.list()
    indicator_intersect$`Healthcare Coverage` <- dplyr::intersect(indicators_list$`Healthcare Coverage`, ind) %>% as.list()
    indicator_intersect$`Health Outcomes` <- dplyr::intersect(indicators_list$`Health Outcomes`, ind) %>% as.list()
    
    shiny::updateSelectInput(session,
                      inputId = "indicator",
                      choices = indicator_intersect,
                      selected = indicator_intersect[[1]])
    
  })
  
  
  
  shiny::observeEvent(input$indicator, {
    shiny::req(input$country)
    shiny::req(input$indicator)

    ind_selected = input$indicator
    
    measure_unit <- hefpi::indicators %>%
      dplyr::select(indicator_short_name, unit_of_measure) %>%
      dplyr::filter(indicator_short_name == ind_selected) %>%
      dplyr::distinct() %>%
      dplyr::slice(1) %>%
      dplyr::select(unit_of_measure) %>%
      dplyr::pull()
    
    if(!is.null(input$country)) {

      indicator <- input$indicator
      # indicator <- 'Diastolic blood pressure (mmHg)'
      # region <- input$region
      # region <- 'Europe & Central Asia'
      cn <- input$country
      # country_name <- 'Ukraine'
      # get data
      # TEMPORARILY COMMENT OUT CODE FOR FAKE DATA BELOW
      pd <- hefpi::hefpi_df

      years <- pd %>%
        tidyr::as_tibble() %>%
        dplyr::select(country, year, regioncode, indic, urb, rur) %>%
        dplyr::filter(country == cn) %>%
        dplyr::left_join(
          hefpi::indicators %>% dplyr::select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
          by = c('indic' = 'variable_name')
        ) %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        dplyr::select(year) %>%
        dplyr::pull() %>%
        unique() %>%
        sort(decreasing = TRUE)
      
      

      shiny::updateSelectInput(session,
                        inputId = "date_range",
                        label = 'Year',
                        choices = years,
                        selected = years[1]
      )
      


    }
    
    
    if(stringr::str_detect(measure_unit, '%')) {
      
      output$axis_ui <- shiny::renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::sliderInput(inputId = session$ns('axis'),
                        label = 'Y - Axis', 
                        min = 0,
                        max = 100,
                        step = 1,
                        value = 100)
          )
        )
      })
      
    } else {
      output$axis_ui <- shiny::renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            NULL
          )
        )
      })
    }
    

  })
  


  
  # ----------- REACTIVE DATA ---------------#
  hefpi_sub_df__reactive <- shiny::reactive({
    
    shiny::req(input$country)
    shiny::req(input$indicator)
    shiny::req(input$date_range)
    #cn = 'India'
    #plot_years = c(2019)
    #indicator = "4+ antenatal care visits (%)"
    #indicator = "Height, adults (Centimeter)"
    #rn = rn[1]
    # get inputs
    # plot_years <- c(min(input$date_range):max(input$date_range))
    plot_years <- input$date_range
    indicator <- input$indicator
    cn <- input$country
    # rn <- input$region_name
    # while map (generate from reactive object) is null, plot is null
    if(any(is.null(indicator))){
      return(NULL)
    } else {
      df <- hefpi::hefpi_df %>% 
        dplyr::filter(country == cn) %>%
        tidyr::as_tibble() %>%
        dplyr::select(country, year, regioncode, indic, urb, rur) %>%
        # filter(country == cn) %>%
        dplyr::left_join(
          hefpi::indicators %>% dplyr::select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
          by = c('indic' = 'variable_name')
        ) %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        dplyr::filter(year %in% plot_years) %>%
        tidyr::pivot_longer(cols = c('urb', 'rur'), names_to = 'urb_rur') %>%
        dplyr::group_by(urb_rur) %>%
        dplyr::filter(year == max(year, na.rm = TRUE)) %>%
        dplyr::reframe(value = first(value),
                  indic = indic,
                  year = year,
                  # region_name = rn,
                  #survey_list = survey_list,
                  indicator_short_name = indicator_short_name,
                  good_or_bad = good_or_bad,
                  unit_of_measure = unit_of_measure) 
      
      return(df)
    }
      
  })
  
  
  # ---- PLOT FROM REACTIVE DATA ---- #
  hefpi_sub_plot__reactive <- shiny::reactive({
    
    shiny::req(hefpi_sub_df__reactive())
    
    plot_years <- input$date_range
    indicator <- input$indicator
    cn <- input$country
    
    
    
    if(is.null(hefpi_sub_df__reactive())){
      NULL
    } else {
      
      df <- hefpi_sub_df__reactive()
      
      # create null plot if data is empty
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
          return(p)
        } 
        p <- empty_plot("No data available for the selected inputs")
      } else {
        # get data 
        unit_of_measure <- unique(df$unit_of_measure)
        good_or_bad = unique(df$good_or_bad)
        temp <- df
        # get plot
        if(good_or_bad == 'Good'){
          bar_palette = 'Greens'
        } else {
          bar_palette = 'Reds'
        }
        # relevel factor for chart
        temp$urb_rur <- factor(temp$urb_rur, levels = unique(temp$urb_rur)[order(temp$value, decreasing = TRUE)])
        
        # get plot objects
        plot_text <- paste(
          "Indicator: ",  indicator,' (',unit_of_measure,')',"<br>",
          "Economy: ", as.character(temp$urb_rur),"<br>", 
          "Value: ", paste0(ifelse(unit_of_measure == '%', round(temp$value, digits = 2) * 100, round(temp$value, digits = 2)), ' (', unit_of_measure, ')'), "<br>",
          # 'Value: ', round(temp$value, digits = 2),' (',unit_of_measure,')',"<br>",
          # "Year: ", as.character(temp$year),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        y_axis_text = paste0(indicator)
        
        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        temp$urb_rur <- ifelse(temp$urb_rur == 'urb', 'Urban', 'Rural')
        # the selected country gets a value of NA which the palette will make black.
        # temp$value_col[temp$key == rn] <- NA
        # add higlight functionality to plot
        temp <- plotly::highlight_key(temp, key=~urb_rur)
        
        
        if(length(df$urb_rur) > 5) {
          gg <- ggplot2::ggplot(temp, ggplot2::aes(forcats::fct_rev(factor(urb_rur)), value, text = plot_text))
        } else {
          gg <- ggplot2::ggplot(temp, ggplot2::aes(urb_rur, value, text = plot_text))
        }
        
        if(nchar(y_axis_text) > 55) {
          y_axis_text_split <- y_axis_text
          y_axis_text <- as.character(str_glue('{substr(y_axis_text_split, 1, 55)}\n - {substr(y_axis_text_split, 56, nchar(y_axis_text_split))}'))
        } 
        
        # If unit_of_measure is '%'
        if(str_detect(unit_of_measure, '%')) {
          
          p <- gg +
                       ggplot2::geom_bar(stat = 'identity', ggplot2::aes(fill = value_col)) +
                       
                        ggplot2::scale_fill_distiller(palette = bar_palette, direction = 1) +
                        ggplot2::scale_y_continuous(limits = c(0, input$axis/100), labels = function(x) paste0(x*100)) +
                       # scale_y_continuous(labels = function(x) paste0(x*100)) +
                        ggplot2::labs(x = '',
                                      y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 45,
                                          x_axis_line = NA,
                                          axis_title_size = 0.51,
                                          legend_position = 'none') 
          
        } else {
          
          p <- gg +
            ggplot2::geom_bar(stat = 'identity', aes(fill = value_col)) +
                       
            ggplot2::scale_fill_distiller(palette = bar_palette, direction = 1) +
            ggplot2::labs(x='',
                            y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 45,
                                          x_axis_line = NA,
                                          legend_position = 'none') 
          
        }
        
        if(length(df$urb_rur) > 5) {
          p <- p +
            ggplot2::coord_flip()
        } 
        
        return(p)
        
      }
      
    }
    
  })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_sub_mean_plot <- plotly::renderPlotly({
    #cn = 'India'
    #plot_years = c(1982, 2017)
    #indicator = "4+ antenatal care visits (%)"
    #rn = rn[1]
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    cn <- input$country
    
    shiny::req(hefpi_sub_plot__reactive())
    # rn <- input$region_name
    # while map (generate from reactive object) is null, plot is null
    if(is.null(hefpi_sub_plot__reactive())){
      NULL
    } else {
      p <- plotly::ggplotly(
              plotly::ggplotly(
                      hefpi_sub_plot__reactive(),
                      tooltip = 'text')
              ) %>% 
               plotly::config(displayModeBar = T) %>%
                plotly::highlight(on='plotly_hover',
                                 off = 'plotly_doubleclick',   
                         persistent = FALSE,
                         color = 'black',
                         opacityDim = 0.6) %>%
                plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
    }
  })
  
  
  # ---- DOWNLOAD PLOT IMAGE ---- #
  output$dl_plot <- shiny::downloadHandler(
    filename = function() { 
      paste0("barchart_", Sys.Date(), ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggplot2::ggsave(file, plot = hefpi_sub_plot__reactive(), device = device)
    }
  )
  
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- shiny::downloadHandler(
    filename = function() {
      paste0("most_recent_value_mean_regional_barchart_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # get data
      hefpi_sub_df__reactive <- hefpi_sub_df__reactive()
      if(is.null(hefpi_sub_df__reactive)){
        NULL
      } else {
        if(is.na(hefpi_sub_df__reactive)){
          temp <- data_frame()
          write.csv(temp, file)
        } else {
          write.csv(hefpi_sub_df__reactive(), file)
        }
      }
    }
  )
  
  
  
  
  output$map_title_ui <- shiny::renderUI({
    shiny::req(input$indicator)
    
    
    indicator_name <- input$indicator
    
    shiny::fluidPage(
      shiny::fluidRow(
        HTML(stringr::str_glue('
                        <div class="chart-header-labels-row">
                           <div class="chart-label"> Urban-rural </div>
                           <div class="chart-label"> {indicator_name} </div>
                          </div>
                          '))
        
      )
    )
  })
  
  
}


## To be copied in the UI
# mod_rural_ui("rural")

## To be copied in the server
# callModule(mod_rural_server, 'rural')