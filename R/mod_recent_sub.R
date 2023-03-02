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
mod_recent_mean_sub_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(8,
                  shiny::uiOutput(ns('map_title_ui')),
                  plotly::plotlyOutput(
                   ns('recent_sub_mean_plot'), 
                   height = 650
                 )),
    shiny::column(4,
           p('Country'),
           shiny::selectInput(ns('country'), 
                       label = NULL,
                       choices = sort(unique(hefpi::hefpi_sub_df$country)), selected = 'India'),
           # sliderInput(ns('date_range'),
           #             label = NULL,
           #             min = 1982,
           #             max = 2021,
           #             value = c(1982, 2021),
           #             step = 1,
           #             sep = ''),
           shiny::selectInput(ns('date_range'),
                       label = 'Year',
                       choices = NULL
                       ),
           # uiOutput(ns('ind_ui')),
           shiny::selectInput(inputId = ns('indicator'),
                       label = 'Indicator',
                       choices = NULL,
           ),
           shiny::uiOutput(ns('axis_ui')),
           # shiny::downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
           shiny::downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
           br(), br()
           # ,
           # actionButton(ns('share_chart'), 'Share chart'),
           
           )
    
  )
 
}

# SERVER FOR MOST RECENT VALUE SUBNATIONAL MEAN
mod_recent_mean_sub_server <- function(input, output, session){
  
  # ---- UI output for region within country ---#
  # output$ind_ui <- renderUI({
  #   #cn = 'Ukraine'
  #   #plot_years = c(1982, 2017)
  #   cn = input$country
  #   # plot_years <- input$date_range
  # 
  #   df <- hefpi::hefpi_sub_df %>%
  #     filter(country == cn) %>%
  #     # filter(year %in% plot_years) %>%
  #     group_by(key) %>%
  #     # filter(year == max(year, na.rm = TRUE)) %>%
  #     summarise(value = first(value),
  #               indic = indic,
  #               year = year,
  #               region_name = region_name,
  #               #survey_list = survey_list,
  #               indicator_short_name = indicator_short_name,
  #               unit_of_measure = unit_of_measure)
  # 
  #   ind = sort(unique(df$indicator_short_name))
  #   rn = sort(unique(df$key))
  # 
  #   indicator_intersect <- indicators_list
  #   indicator_intersect$`Financial Protection` <- intersect(indicators_list$`Financial Protection`, ind) %>% as.list()
  #   indicator_intersect$`Healthcare Coverage` <- intersect(indicators_list$`Healthcare Coverage`, ind) %>% as.list()
  #   indicator_intersect$`Health Outcomes` <- intersect(indicators_list$`Health Outcomes`, ind) %>% as.list()
  # 
  # 
  #   fluidPage(
  #     fluidRow(
  #       selectInput(inputId = session$ns('indicator'),
  #                   label = 'Indicator',
  #                   choices = indicator_intersect,
  #                   selected = indicator_intersect[[1]]
  #       )
  # 
  #     )
  #   )
  # 
  # 
  # })
  
  shiny::observeEvent(input$country, {

    shiny::req(input$country)

    # print('Country triggered')

    cn = input$country
    # cn = 'Honduras'
    # cn = 'Iceland'
    # plot_years <- input$date_range

    df <- hefpi::hefpi_sub_df %>%
      dplyr::filter(country == cn) %>%
      # filter(year %in% plot_years) %>%
      dplyr::group_by(key) %>%
      dplyr::filter(year == max(year, na.rm = TRUE)) %>%
      dplyr::summarise(value = first(value),
                indic = indic,
                year = year,
                region_name = region_name,
                #survey_list = survey_list,
                indicator_short_name = indicator_short_name,
                unit_of_measure = unit_of_measure)

    ind = sort(unique(df$indicator_short_name))
    rn = sort(unique(df$key))

    indicator_intersect <- indicators_list
    indicator_intersect$`Financial Protection` <- intersect(indicators_list$`Financial Protection`, ind) %>% as.list()
    indicator_intersect$`Healthcare Coverage` <- intersect(indicators_list$`Healthcare Coverage`, ind) %>% as.list()
    indicator_intersect$`Health Outcomes` <- intersect(indicators_list$`Health Outcomes`, ind) %>% as.list()

    shiny::updateSelectInput(session,
                      inputId = "indicator",
                      choices = indicator_intersect,
                      selected = ind[1]
    )

  })
  
  shiny::observeEvent(input$indicator, {

    shiny::req(input$country)
    shiny::req(input$indicator)

      #ind <- 'BMI, women 15-49 (BMI)'
      #cn <- 'Honduras'


      ind_selected = input$indicator

      measure_unit <- hefpi::hefpi_sub_df %>%
        dplyr::filter(indicator_short_name == ind_selected) %>%
        dplyr::filter(country == input$country) %>%
        dplyr::distinct() %>%
        dplyr::slice(1) %>%
        dplyr::select(unit_of_measure) %>%
        dplyr::pull()

      if(!is.null(input$country)) {

        indicator <- input$indicator
        # indicator <- 'Diastolic blood pressure (mmHg)'
        # region <- input$region
        # region <- 'Europe & Central Asia'
        country_name <- input$country
        # country_name <- 'Ukraine'
        # get data
        # TEMPORARILY COMMENT OUT CODE FOR FAKE DATA BELOW
        pd <- hefpi::hefpi_sub_df

        years <- pd %>%
          dplyr::filter(indicator_short_name == indicator) %>%
          dplyr::filter(country == country_name) %>%
          dplyr::select(year) %>%
          dplyr::distinct() %>%
          dplyr::pull()
        
        years <- sort(years, decreasing = TRUE)

        values_range <- hefpi::hefpi_sub_df %>%
          dplyr::filter(indicator_short_name == indicator) %>%
          dplyr::filter(country == country_name) %>%
          dplyr::select(value) %>%
          dplyr::distinct() %>%
          dplyr::pull()

        values_range_slider <- c()
        values_range_slider[1] <- floor(min(values_range, na.rm = TRUE))
        values_range_slider[2] <- ceiling(max(values_range, na.rm = TRUE))


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
                          label = 'Axis',
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
              shiny::sliderInput(inputId = session$ns('axis'),
                          label = 'Axis',
                          min = 0,
                          max = (values_range_slider[2] + 5),
                          step = 1,
                          value = values_range_slider[2])
            )
          )
        })
      }
  })
  

  
  
  # ----------- REACTIVE DATA ---------------#
  hefpi_sub_df__reactive <- shiny::reactive({

    #cn = 'Honduras'
    #plot_years = c(1982, 2017)
    #indicator = "BMI, women 15-49 (BMI)"
    #rn = rn[1]
    # get inputs
    shiny::req(input$date_range)
    shiny::req(input$indicator)
    shiny::req(input$country)

    # print(input$date_range)
    # print(input$indicator)
    # print(input$country)

    plot_years <- input$date_range
    # plot_years <- c(min(input$date_range):max(input$date_range))
    indicator <- input$indicator
    cn <- input$country
    # rn <- input$region_name
    # while map (generate from reactive object) is null, plot is null
    if(is.null(indicator)){
      return(NULL)
    } else {
      df <- hefpi::hefpi_sub_df %>%
        dplyr::filter(country == cn) %>%
        dplyr::filter(indicator_short_name == indicator) %>%
        dplyr::filter(year %in% plot_years) %>%
        # filter(year >= min(plot_years),
        #        year <= max(plot_years)) %>%
        dplyr::group_by(key) %>%
        # filter(year == max(year, na.rm = TRUE)) %>%
        dplyr::summarise(value = first(value),
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

      # print(df)
      # print(nrow(df))

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
        temp$key <- factor(temp$key, levels = unique(temp$key)[order(temp$value, decreasing = TRUE)])

        # get plot objects
        plot_text <- paste(
          "Indicator: ",  indicator,' (',unit_of_measure,')',"<br>",
          "Economy: ", as.character(temp$key),"<br>",
          "Value: ", paste0(ifelse(unit_of_measure == '%', round(temp$value, digits = 2) * 100, round(temp$value, digits = 2)), ' (', unit_of_measure, ')'), "<br>",
          # 'Value: ', round(temp$value, digits = 2),' (',unit_of_measure,')',"<br>",
          # "Year: ", as.character(temp$year),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        y_axis_text = paste0(indicator)

        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        # temp$value_col[temp$key == rn] <- NA
        # add higlight functionality to plot
        temp <- highlight_key(temp, key=~key)


        if(length(df$key) > 5) {
          gg <- ggplot2::ggplot(temp, aes(forcats::fct_rev(factor(key)), value, text = plot_text))
        } else {
          gg <- ggplot2::ggplot(temp, aes(key, value, text = plot_text))
        }

        # If unit_of_measure is '%'
        if(str_detect(unit_of_measure, '%')) {

          p <- gg +
                      ggplot2::geom_bar(stat = 'identity', ggplot2::aes(fill = value_col), width = 0.75) +

                       ggplot2::scale_fill_distiller(palette = bar_palette, direction = 1) +
                       #scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                       ggplot2::scale_y_continuous(limits = c(0, input$axis/100), labels = function(x) paste0(x*100)) +
                       ggplot2::labs(x='',
                            y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 0,
                                          # x_axis_line = NA,
                                          y_axis_size = 10,
                                          legend_position = 'none')

        } else {

          p <- gg +
            ggplot2::geom_bar(stat = 'identity', aes(fill = value_col), width = 0.75) +

            ggplot2::scale_fill_distiller(palette = bar_palette, direction = 1) +
            ggplot2::scale_y_continuous(limits = c(0, input$axis), labels = function(x) paste0(x)) +
            ggplot2::labs(x='',
                            y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 0,
                                          legend_position = 'none')

        }

        if(length(df$key) > 5) {
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

    req(hefpi_sub_plot__reactive())
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
                plotly::layout(xaxis = list(fixedrange = TRUE, side ="top"), yaxis = list(fixedrange = TRUE))
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
                           <div class="chart-label"> {indicator_name} </div>
                          </div>
                          '))
        
      )
    )
  })
  
  
}


## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')