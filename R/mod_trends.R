# Module Trends 

#' @title   mod_trens.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_trends_mean_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_trends_mean_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  fluidPage(
    column(8,
           plotlyOutput(
             ns('trends_mean'), height = '800px'
           )),
    column(4,
           pickerInput(ns('indicator'),
                       'Indicator',
                       choices = indicators_list,
                       selected = '4+ antenatal care visits',
                       options = list(`dropdown-align-right` = TRUE)),
          pickerInput(inputId = ns("region"),
                      label = 'Region', 
                      choices = as.character(region_list$region),
                      selected = as.character(region_list$region)[1],
                      options = list( `actions-box`=TRUE,
                                      `selected-text-format` = "count > 2",
                                      `count-selected-text` = "{0}/{1} Regions"),
                      multiple = TRUE),
           uiOutput(ns('ui_outputs')),
          sliderInput(ns('date_range'),
                                  'Date range',
                                  min = 1982,
                                  max = 2017,
                                  value = c(1982, 2017),
                                  step = 1,
                                  sep = ''),
          checkboxInput(ns('interpolate'), 'Interpolate missing values',
                        value = TRUE),
          downloadButton(ns("dl_plot"), label = 'Download image'),
          downloadButton(ns("dl_data"), label = 'Download data'),
          fluidPage(
            fluidRow(
              useShinyalert(),  # Set up shinyalert
              actionButton(ns("plot_info"), label = "Plot Info"))
          ))
  )
  )
}

# Module Server
#' @rdname mod_trends_mean_server
#' @export
#' @import tidyverse
#' @import plotly
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_mean_server <- function(input, output, session){
 
    output$ui_outputs <- renderUI({
      
      
      # Observe changes to inputs in order to generate changes to the map
      observeEvent(input$plot_info, {
        # Show a modal when the button is pressed
        shinyalert(title = "Trends - Population mean", 
                   text = "charts allow tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
                   type = "info", 
                   closeOnClickOutside = TRUE, 
                   showCancelButton = FALSE, 
                   showConfirmButton = FALSE)
      })
      
      # get inputs
      indicator <- input$indicator
      region <- input$region


      # get region code
      region_list <- hefpi::region_list
      region_code <- as.character(region_list$region_code[region_list$region == region])
        
      # Get the variable
      variable <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        .$variable_name
      
      # subset data by variable and region code
        df <- hefpi::df
        df <- df[df$indic == variable,]
        df <- df[df$regioncode %in% region_code,]
       
        max_value <- round(max(df$pop), 2)
        min_value <- round(min(df$pop), 2)
        if(max_value<1){
          min_value=0
          max_value = 1
        } else {
          min_value = 0
          max_value = ceiling(max_value)
        }
      
        countries <- unique(df$country)
        
        fluidPage(
          fluidRow(
            pickerInput(inputId = session$ns("country"),
                        label = 'Country', 
                        choices = countries,
                        selected = countries,
                        options = list( `actions-box`=TRUE,
                                        `selected-text-format` = "count > 2",
                                        `count-selected-text` = "{0}/{1} Countries"),
                        multiple = TRUE),
            sliderInput(session$ns('value_range'),
                        'Y axis range',
                        min = min_value,
                        max = max_value,
                        value = c(min_value, max_value),
                        sep = '')
          )
        )
        
      
    })
    
    get_pop_data <- reactive({
      indicator <- "Infant mortality"
      region <- region_list$region[1]
      temp <- hefpi::df_series %>% filter(region %in% region)
      country_names <- unique(temp$country_name)
      date_range <- c(1982, 2017)
      value_range <- c(0,81)
      # get inputs
      pop_list <- list()
      indicator <- input$indicator
      region <- input$region
      country_names <- input$country
      date_range <- input$date_range
      value_range <- input$value_range
      # get region code 
      region_list <- hefpi::region_list
      indicators <- hefpi::indicators
      df <- hefpi::df
      
      if(is.null(country_names) | is.null(value_range)){
        NULL
      } else {
        
        region_code <- as.character(region_list$region_code[region_list$region %in% region])
        
        # get variable
        ind_info <- indicators %>%
          filter(indicator_short_name == indicator) %>%
          select(variable_name, unit_of_measure)
        variable_name = ind_info$variable_name
        unit_of_measure = ind_info$unit_of_measure
        if(unit_of_measure != '%'){
          unit_of_measure = paste0(' (', unit_of_measure, ')')
        }
        
        # subet by variable, region code and a list of countries
        df <- df[df$indic == variable_name,]
        df <- df[df$regioncode %in% region_code,]
        pd <- df[df$country %in% country_names,]
        pd <- pd %>% filter(year >= min(date_range),
                            year <= max(date_range)) 
        pd <- pd %>% filter(pop >= min(value_range),
                            pop <= max(value_range))
        if(nrow(pd) == 0){
          NULL
        } else {
          
          # get title and subtitle
          plot_title <- paste0('Trends - Population mean')
          y_axis_text <- paste0(indicator)
          
          # condition on unit of measure
          if(unit_of_measure == '%'){
            pd$pop <- pd$pop*100
          }
          # text for plot
          mytext <- paste(
            "Indicator: ", indicator,"<br>", 
            "Economy: ", as.character(pd$country),"<br>", 
            "Value: ", round(pd$pop, digits = 2), "<br>",
            "Year: ", as.character(pd$year),"<br>",
            "Data source: ", as.character(pd$referenceid_list), "<br>",
            sep="") %>%
            lapply(htmltools::HTML)
          
          trend_palette <- colorRampPalette(brewer.pal(name = "Paired", n = 12))(length(unique(pd$country)))
          
          
          yn <- input$interpolate
          if(yn){
            
            # condition if we connect the dots
            p <- ggplotly(ggplot(data = pd, aes(year, pop, color= country, text=mytext)) +
                            geom_point() + 
                            geom_line(aes(group = country)) +
                            scale_color_manual(name = '',
                                               values = trend_palette) +
                            scale_y_continuous(labels = function(x) paste0(x, unit_of_measure))+
                            labs(x='Year',
                                 y = y_axis_text,
                                 title = plot_title) +
                            hefpi::theme_gdocs() +
                            theme(panel.grid.major.x = element_blank(),
                                  axis.ticks = element_blank()), tooltip = 'text')
            fig <- p %>% config(displayModeBar = F)
            
          } else {
            # condition if we connect the dots
            p <- ggplotly(ggplot(data = pd, aes(year, pop, color= country, text=mytext)) +
                            geom_point() +
                            scale_color_manual(name = '',
                                               values = trend_palette) +
                            scale_y_continuous(labels = function(x) paste0(x, "%"))+
                            labs(x='Year',
                                 y = y_axis_text,
                                 title = plot_title) +
                            hefpi::theme_gdocs() +
                            theme(panel.grid.major.x = element_blank(),
                                  axis.ticks = element_blank()), tooltip = 'text')
            fig <- p %>% config(displayModeBar = F)
          }
          
      
          pop_list[[1]] <- fig
          pop_list[[2]] <- pd
          pop_list[[3]] <- list(plot_title, mytext, y_axis_text, unit_of_measure, trend_palette)
          
          
          return(pop_list)
        }
        }
        
    })
    
    # ---- DOWNLOAD DATA FROM MAP ---- #
    output$dl_data <- downloadHandler(
      filename = function() {
        paste("trends_population_mean_data", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # get map
        pop_list <- get_pop_data()
        pd <- pop_list[[2]]
        
        if(is.null(pop_list)){
          NULL
        } else {
          
          write.csv(pd, file)
        }
      }
    )
    
    # ---- DOWNLOAD MAP IMAGE ---- #
    output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_trends_population_mean", ".png"),
                                      content = function(file) {
                                        
                                        pop_list <- get_pop_data()
                                        fig <- pop_list[[1]]
                                        plot_title = pop_list[[3]][[1]]
                                        mytext = pop_list[[3]][[2]]
                                        y_axis_text = pop_list[[3]][[3]]
                                        unit_of_measure = pop_list[[3]][[4]]
                                        trend_palette = pop_list[[3]][[5]]
                                        
                                        
                                        
                                        if(is.null(pop_list)){
                                          NULL
                                        } else {
                                          fig
                                          ggsave(file)
                                        }
                                        
                                       
                                      })
    
    output$trends_mean <- renderPlotly({
      
      pop_list <- get_pop_data()
      fig <- pop_list[[1]]
      pd <- pop_list[[2]]
      plot_title = pop_list[[3]][[1]]
      mytext = pop_list[[3]][[2]]
      y_axis_text = pop_list[[3]][[3]]
      unit_of_measure = pop_list[[3]][[4]]
      trend_palette = pop_list[[3]][[5]]
      
     
      
      if(is.null(pop_list)){
        NULL
      } else {
        fig
      }
        
      
      
    })
}

#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_con_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_trends_con_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('trends_con'), height = '800px'
             )),
      column(4,
             pickerInput(ns('indicator'),
                         'Indicator',
                         choices = indicators_list,
                         selected = '4+ antenatal care visits',
                         options = list(`dropdown-align-right` = TRUE)),
             pickerInput(inputId = ns("region"),
                         label = 'Region', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1],
                         options = list( `actions-box`=TRUE,
                                         `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} Regions"),
                         multiple = TRUE),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('value_range'),
                         'Y axis range',
                         min = 0,
                         max = 1,
                         value = c(0,1),
                         sep = ''),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             checkboxInput(ns('interpolate'), 'Interpolate missing values',
                           value = TRUE),
             downloadButton(ns("dl_plot"), label = 'Download image'),
             downloadButton(ns("dl_data"), label = 'Download data'),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info"))
             ))
    )
  )
}

# Module Server
#' @rdname mod_trends_cib_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_con_server <- function(input, output, session){
  
  
  output$ui_outputs <- renderUI({
    
    # Observe changes to inputs in order to generate changes to the map
    observeEvent(input$plot_info, {
      # Show a modal when the button is pressed
      shinyalert(title = "Trends - Concentration Index", 
                 text = "charts allow users to track the over-time dynamics in an indicator’s concentration index. tracking of the over-time dynamics of HEFPI indicators at the population level. Both single and multiple country trend charts are available, and users can choose whether to only show data points for years with survey data, or if trend lines should linearly interpolate over years where data are missing.", 
                 type = "info", 
                 closeOnClickOutside = TRUE, 
                 showCancelButton = FALSE, 
                 showConfirmButton = FALSE)
    })
    
    # get inputs
    indicator <- input$indicator
    region <- input$region
    
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset data by variable and region code
    df <- hefpi::df
    df <- df[df$indic == variable,]
    df <- df[df$regioncode %in% region_code,]
    
    countries <- unique(df$country)
    
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("country"),
                    label = 'Country', 
                    choices = countries,
                    selected = countries,
                    options = list( `actions-box`=TRUE,
                                    `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} Countries"),
                    multiple = TRUE),
        
      )
    )
    
    
  })
  
  get_con_data <- reactive({
    indicator <- "Infant mortality"
    region <- region_list$region[1]
    temp <- hefpi::df_series %>% filter(region %in% region)
    country_names <- unique(temp$country_name)
    date_range <- c(1982, 2017)
    value_range <- c(0,81)
    yn <- input$interpolate
    
    # get inputs
    con_list <- list()
    indicator <- input$indicator
    region <- input$region
    country_names <- input$country
    date_range <- input$date_range
    value_range <- input$value_range
    # get region code 
    region_list <- hefpi::region_list
    indicators <- hefpi::indicators
    df <- hefpi::df
    
    if(is.null(country_names)){
      NULL
    } else {
      
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      
      # get variable
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      
      # subet by variable, region code and a list of countries
      df <- df[df$indic == variable_name,]
      df <- df[df$regioncode %in% region_code,]
      pd <- df[df$country %in% country_names,]
      pd <- pd %>% filter(year >= min(date_range),
                          year <= max(date_range)) 
      pd <- pd %>% filter(CI >= min(value_range),
                          CI <= max(value_range))
      if(nrow(pd) == 0){
        NULL
      } else {
        
        # get title and subtitle
        plot_title <- paste0('Trends - Concentration index')
        y_axis_text <- paste0(indicator)
        
      
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country),"<br>", 
          "Value: ", round(pd$CI, digits = 2), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          "Data source: ", as.character(pd$referenceid_list), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        
        trend_palette <- colorRampPalette(brewer.pal(name = "Paired", n = 12))(length(unique(pd$country)))
        
        
        if(yn){
          
          # condition if we connect the dots
          p <- ggplotly(ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
                          geom_point() + 
                          geom_line(aes(group = country)) +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.ticks = element_blank()), tooltip = 'text')
          fig <- p %>% config(displayModeBar = F)
          
        } else {
          # condition if we connect the dots
          p <- ggplotly(ggplot(data = pd, aes(year, CI, color= country, text=mytext)) +
                          geom_point() +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.ticks = element_blank()), tooltip = 'text')
          fig <- p %>% config(displayModeBar = F)
        }
        
        
        con_list[[1]] <- fig
        con_list[[2]] <- pd
        con_list[[3]] <- list(plot_title, mytext, y_axis_text, trend_palette)
        
        
        return(con_list)
      }
    }
    
  })
  
  # ---- DOWNLOAD DATA FROM MAP ---- #
  output$dl_data <- downloadHandler(
    filename = function() {
      paste("trends_ci_data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # get map
      con_list <- get_con_data()
      pd <- con_list[[2]]
      
      if(is.null(con_list)){
        NULL
      } else {
        
        write.csv(pd, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_trends_ci", ".png"),
                                    content = function(file) {
                                      
                                      con_list <- get_con_data()
                                      fig <- con_list[[1]]
                                      plot_title = con_list[[3]][[1]]
                                      mytext = con_list[[3]][[2]]
                                      y_axis_text = con_list[[3]][[3]]
                                      trend_palette = con_list[[3]][[4]]
                                      
                                      
                                      
                                      if(is.null(con_list)){
                                        NULL
                                      } else {
                                        fig
                                        ggsave(file)
                                      }
                                      
                                      
                                    })
  
  output$trends_con <- renderPlotly({
    
    con_list <- get_con_data()
    fig <- con_list[[1]]
    pd <- con_list[[2]]
    plot_title = con_list[[3]][[1]]
    mytext = con_list[[3]][[2]]
    y_axis_text = con_list[[3]][[3]]
    trend_palette = con_list[[3]][[4]]
    
    
    
    if(is.null(con_list)){
      NULL
    } else {
      fig
    }
    
    
    
  })
  
  
}

#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_quin_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_trends_quin_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('trends_quin'),  height = '800px'
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list,
                         selected = 'United States'),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list,
                         selected = 'Inpatient care use, adults'),
             selectInput(ns('view_as'), 'View as',
                         choices =c('Slope chart', 'Line chart')),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             uiOutput(ns('ui_value_range')),
             useShinyalert(),  # Set up shinyalert
             actionButton(ns("plot_info"), "Plot Info"))
    )
  )
}

# Module Server
#' @rdname mod_trends_quin_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_trends_quin_server <- function(input, output, session){
  
  # Observe changes to inputs in order to generate changes to the map
  observeEvent(input$plot_info, {
    # Show a modal when the button is pressed
    shinyalert(title = "Trends - Quintile", 
               text = "charts show HEFPI health outcome and health service coverage indicator trends at the wealth quintile level, revealing if any inequalities have reduced, remained stable, or increased over time. Users can tailor the charts to their time period of interest.", 
               type = "info", 
               closeOnClickOutside = TRUE, 
               showCancelButton = FALSE, 
               showConfirmButton = FALSE)
  })
  
  output$ui_value_range <- renderUI({
    country_names <- 'United States'
    indicator <- 'Catastrophic health spending, 10%'
    # value_range <- c(0.06,0.12)
    date_range <- c(1982,2016)
    # 
    country_names <- input$country
    indicator <- input$indicator
    date_range <- input$date_range

    # Get the variable
    variable <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      .$variable_name
    
    # subset by country and variable
    df <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic == variable) %>%
      filter(year >= min(date_range),
             year <= max(date_range))  %>%
      select(year, Q1:Q5) 
    temp <- melt(df, id.vars = 'year')
    max_value <- round(max(temp$value), 2)
    min_value <- round(min(temp$value), 2)
    if(max_value<1){
      min_value=0
      max_value = 1
    } else {
      min_value = floor(min_value)
      max_value = ceiling(max_value)
    }
    sliderInput(session$ns('value_range'),
                'Y axis range',
                min = min_value,
                max = max_value,
                value = c(min_value, max_value),
                sep = '')
  })
  
  output$trends_quin <- renderPlotly({
    # country_names <- 'United States'
    # indicator <- 'Catastrophic health spending, 10%'
    # value_range <- c(0.06,0.12)
    # date_range <- c(1982,2016)
    # view_as <- 'Slope chart'
    # 
    country_names <- input$country
    indicator <- input$indicator
    date_range <- input$date_range
    view_as <- input$view_as
    value_range <- input$value_range
    
    if(is.null(value_range)){
      NULL
    } else {
      # Get the variable
      variable <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        .$variable_name
      
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country == country_names) %>%
        filter(indic == variable) %>%
        filter(year >= min(date_range),
               year <= max(date_range))  %>%
        select(year, Q1:Q5) 
      if(view_as == 'Slope chart'){
        # filter to get year_one and year_last
        year_begin = min(df$year)
        year_end = max(df$year)
        df <- df %>%
          filter(year == year_begin | year == year_end)
        # save(df, file = 'df.rda')
      }
      
      df <- melt(df, id.vars = 'year')
      
      # recode Quintiels
      df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                            ifelse(df$variable == 'Q2', 'Q2: Poor',
                                   ifelse(df$variable == 'Q3', 'Q3: Middle',
                                          ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
      
      # # get color graident 
      col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
      col_vec <- col_vec[-1]
      
      # make plot title
      plot_title = paste0('Quintile Trends - ', country_names, ' , ', indicator)
      y_axis_text = indicator
      # subset by y axis
      df <- df %>% 
        filter(value >= value_range[1],
               value <= value_range[2])
      # text for plot
      mytext <- paste(
        "Value: ", round(df$value, digits = 3), "\n",
        "Year: ", as.character(df$year),"\n",
        sep="") %>%
        lapply(htmltools::HTML)
      # condition if we connect the dots
      p <- plot_ly(data = df, x = ~year, y = ~value, color = ~variable, colors = col_vec,
                   text = mytext, hoverinfo = 'text') %>%
        add_trace(x = ~year, y = ~value, color = ~variable, colors = col_vec, mode = 'lines+markers') %>%
        layout(title = plot_title,
               xaxis= list(showline = TRUE, title = 'Year', showticklabels = TRUE),
               yaxis= list(tickformat='%',showline = TRUE, title = y_axis_text, showticklabels = TRUE))
      
      return(p)
    }
    
  })
}



## To be copied in the UI
# mod_trends_mean_ui("trends_mean1")
# mod_trends_quin_ui("trends_quin1")
# mod_trends_con_ui("trends_con1")


## To be copied in the server
# callModule(mod_trends_mean_server, 'trends_mean1')
# callModule(mod_trends_quin_server, 'trends_quin1')
# callModule(mod_trends_con_server, 'trends_con1')

