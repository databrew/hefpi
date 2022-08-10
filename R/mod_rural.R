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
  ns <- NS(id)
  fluidRow(
    column(8,
           uiOutput(ns('map_title_ui')),
           plotlyOutput(
             ns('recent_sub_mean_plot'), height = 550
           )),
    column(4,
           p('Choose country to highlight'),
           selectInput(ns('country'), 
                       label = NULL,
                       choices = unique(hefpi::hefpi_df$country), selected = 'India'),
           # sliderInput(ns('date_range'),
           #             label = NULL,
           #             min = 1982,
           #             max = 2017,
           #             value = c(1982, 2017),
           #             step = 1,
           #             sep = ''),
           selectInput(ns('date_range'),
                       label = 'Year',
                       choices = NULL
                       ),
           uiOutput(ns('ind_ui')),
           downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
           downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary')
           
           )
    
  )
 
}

# SERVER FOR MOST RECENT VALUE SUBNATIONAL MEAN
mod_rural_server <- function(input, output, session){
  
  # ---- UI output for region within country ---#
  output$ind_ui <- renderUI({
    #cn = 'India'
    #plot_years = c(1982, 2017)
    req(input$country)
    
    cn = input$country
    # plot_years <- input$date_range
    
    hefpi::hefpi_sub_df
    
    ind <- hefpi::hefpi_df %>% 
      as_tibble() %>% 
      select(country, year, regioncode, indic, urb, rur) %>%
      filter(country == cn) %>%
      left_join(
        hefpi::indicators %>% select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
        by = c('indic' = 'variable_name')
      ) %>%
      select(indicator_short_name) %>%
      pull() %>%
      unique() %>%
      sort()

    
    fluidPage(
      fluidRow(
        selectInput(inputId = session$ns('indicator'),
                    label = 'Indicator', 
                    choices = ind, 
                    selected = ind[1])
        # ,
        # selectInput(inputId =session$ns('region_name'),
        #             label = 'Choose region',
        #             choices = rn,
        #             selected = rn[1])
      )
    )
   
    
  })
  
  observe({
    req(input$country)
    req(input$indicator)
    
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
        as_tibble() %>% 
        select(country, year, regioncode, indic, urb, rur) %>%
        filter(country == cn) %>%
        left_join(
          hefpi::indicators %>% select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
          by = c('indic' = 'variable_name')
        ) %>%
        filter(indicator_short_name == indicator) %>%
        select(year) %>%
        pull() %>%
        unique() %>%
        sort(decreasing = TRUE)
      
      
      updateSelectInput(session,
                        inputId = "date_range",
                        label = 'Year',
                        choices = years,
                        selected = years[1]
      )
    }
    
  })
  
  # ----------- REACTIVE DATA ---------------#
  hefpi_sub_df__reactive <- reactive({
    
    req(input$country)
    req(input$indicator)
    req(input$date_range)
    #cn = 'India'
    #plot_years = c(2015)
    #indicator = "4+ antenatal care visits (%)"
    #rn = rn[1]
    # get inputs
    plot_years <- input$date_range
    indicator <- input$indicator
    cn <- input$country
    # rn <- input$region_name
    # while map (generate from reactive object) is null, plot is null
    if(is.null(indicator)){
      return(NULL)
    } else {
      df <- hefpi::hefpi_df %>% 
        filter(country == cn) %>%
        as_tibble() %>%
        select(country, year, regioncode, indic, urb, rur) %>%
        # filter(country == cn) %>%
        left_join(
          hefpi::indicators %>% select(good_or_bad, variable_name, indicator_short_name, unit_of_measure),
          by = c('indic' = 'variable_name')
        ) %>%
        filter(indicator_short_name == indicator) %>%
        filter(year %in% plot_years) %>%
        pivot_longer(cols = c('urb', 'rur'), names_to = 'urb_rur') %>%
        group_by(urb_rur) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        summarise(value = first(value),
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
  hefpi_sub_plot__reactive <- reactive({
    
    req(hefpi_sub_df__reactive())
    
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
          "Year: ", as.character(temp$year),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        y_axis_text = paste0(indicator)
        
        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        # temp$value_col[temp$key == rn] <- NA
        # add higlight functionality to plot
        temp <- highlight_key(temp, key=~urb_rur)
        
        
        if(length(df$urb_rur) > 5) {
          gg <- ggplot(temp, aes(forcats::fct_rev(factor(urb_rur)), value, text = plot_text))
        } else {
          gg <- ggplot(temp, aes(urb_rur, value, text = plot_text))
        }
        
        # If unit_of_measure is '%'
        if(str_detect(unit_of_measure, '%')) {
          
          p <- gg +
                       geom_bar(stat = 'identity', aes(fill = value_col)) +
                       
                       scale_fill_distiller(palette = bar_palette, direction = 1) +
                       scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                       labs(x = 'Sub national region',
                            y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 0,
                                          x_axis_line = NA,
                                          legend_position = 'none')
          
        } else {
          
          p <- gg +
                       geom_bar(stat = 'identity', aes(fill = value_col)) +
                       
                       scale_fill_distiller(palette = bar_palette, direction = 1) +
                       labs(x='Sub national region',
                            y = y_axis_text) +
                       hefpi::theme_hefpi(grid_major_x=NA,
                                          x_axis_angle = 0,
                                          x_axis_line = NA,
                                          legend_position = 'none')
          
        }
        
        if(length(df$urb_rur) > 5) {
          p <- p +
            coord_flip()
        } 
        
        return(p)
        
      }
      
    }
    
  })
  
  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_sub_mean_plot <- renderPlotly({
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
      p <- ggplotly(
             ggplotly(
                      hefpi_sub_plot__reactive(),
                      tooltip = 'text')
              ) %>% 
               config(displayModeBar = T) %>%
               highlight(on='plotly_hover',
                         persistent = FALSE,
                         color = 'black',
                         opacityDim = 0.6) %>%
               layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
    }
  })
  
  
  # ---- DOWNLOAD PLOT IMAGE ---- #
  output$dl_plot <- downloadHandler(
    filename = function() { 
      paste0("barchart_", Sys.Date(), ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = hefpi_sub_plot__reactive(), device = device)
    }
  )
  
  
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
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
  
  
  
  
  output$map_title_ui <- renderUI({
    req(input$indicator)
    
    
    indicator_name <- input$indicator
    
    fluidPage(
      fluidRow(
        HTML(str_glue('
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