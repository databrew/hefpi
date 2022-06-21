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
  ns <- NS(id)
  fluidRow(
    column(8,
           plotlyOutput(
             ns('recent_sub_mean_plot'), height = 550
           )),
    column(4,
           p('Choose country to highlight'),
           selectInput(ns('country'), 
                       label = NULL,
                       choices = country_list, selected = 'India'),
           sliderInput(ns('date_range'),
                       label = NULL,
                       min = 1982,
                       max = 2017,
                       value = c(1982, 2017),
                       step = 1,
                       sep = ''),
           uiOutput(ns('ind_ui')))
    
  )
 
}

# SERVER FOR MOST RECENT VALUE SUBNATIONAL MEAN
mod_recent_mean_sub_server <- function(input, output, session){
  
  # ---- UI output for region within country ---#
  output$ind_ui <- renderUI({
    #cn = 'India'
    #plot_years = c(1982, 2017)
    cn = input$country
    plot_years <- input$date_range
    
    df <- hefpi::hefpi_sub_df %>% 
      filter(country == cn) %>%
      filter(year >= min(plot_years),
           year <= max(plot_years)) %>%
      group_by(key) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      summarise(value = first(value),
                indic = indic,
                year = year,
                region_name = region_name,
                #survey_list = survey_list,
                indicator_short_name = indicator_short_name,
                unit_of_measure = unit_of_measure) 
    
    ind = sort(unique(df$indicator_short_name))
    rn = sort(unique(df$key))
    
    fluidPage(
      fluidRow(
        selectInput(inputId = session$ns('indicator'),
                    label = 'Indicator', 
                    choices = ind, 
                    selected = ind[1]),
        selectInput(inputId =session$ns('region_name'),
                    label = 'Choose region',
                    choices = rn,
                    selected = rn[1])
      )
    )
   
    
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
    rn <- input$region_name
    save(plot_years, indicator, cn, rn , file ='temp_sub_inputs.rda')
    # while map (generate from reactive object) is null, plot is null
    if(is.null(indicator)){
      NULL
    } else {
      df <- hefpi::hefpi_sub_df %>% 
        filter(country == cn) %>%
        filter(indicator_short_name == indicator) %>%
        filter(year >= min(plot_years),
               year <= max(plot_years)) %>%
        group_by(key) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        summarise(value = first(value),
                  indic = indic,
                  year = year,
                  region_name = rn,
                  #survey_list = survey_list,
                  indicator_short_name = indicator_short_name,
                  good_or_bad = good_or_bad,
                  unit_of_measure = unit_of_measure) 
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
        temp$key <- factor(temp$key, levels = unique(temp$key)[order(temp$value, decreasing = TRUE)])
        
        # get plot objects
        plot_text <- paste(
          "Indicator: ",  indicator,' (',unit_of_measure,')',"<br>",
          "Economy: ", as.character(temp$key),"<br>", 
          'Value: ', round(temp$value, digits = 2),' (',unit_of_measure,')',"<br>",
          "Year: ", as.character(temp$year),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        y_axis_text = paste0(indicator)
        
        # Create value_color vector, identical to value
        temp$value_col <- temp$value
        # the selected country gets a value of NA which the palette will make black.
        temp$value_col[temp$key == rn] <- NA
        # add higlight functionality to plot
        temp <- highlight_key(temp, key=~key)
        p <- ggplotly(
          ggplotly(ggplot(temp, aes(key, value, text = plot_text)) +
                     geom_bar(stat = 'identity', aes(fill = value_col)) +
                     
                     scale_fill_distiller(palette = bar_palette, direction = 1) +
                     labs(x='Sub national region',
                          y = y_axis_text) +
                     hefpi::theme_hefpi(grid_major_x=NA,
                                        x_axis_angle = 90,
                                        x_axis_line = NA,
                                        legend_position = 'none') +
                     theme(axis.text.x = element_blank(),
                           axis.ticks.x = element_blank()),
                   tooltip = 'text'))   
        p <- p %>% 
          config(displayModeBar = T) %>%
          highlight(on='plotly_hover',
                    persistent = FALSE,
                    color = 'white',
                    opacityDim = 0.6) %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
      }
    }
  })
}


## To be copied in the UI
# mod_recent_mean_sub_ui("leaf2")

## To be copied in the server
# callModule(mod_recent_mean_sub_server, 'leaf2')