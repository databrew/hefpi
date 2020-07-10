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
             ns('trends_mean'), height = '600px'
           )),
    column(4,
           pickerInput(ns('indicator'),
                       'Indicator',
                       choices = sort(unique(indicators$indicator_short_name)),
                       selected = '4+ antenatal care visits',
                       options = list(`style` = "btn-primary")),
          pickerInput(inputId = ns("region"),
                      label = 'Region', 
                      choices = as.character(region_list$region),
                      selected = as.character(region_list$region)[1],
                      options = list( `actions-box`=TRUE,
                                      `style` = "btn-primary",
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
          downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
          downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
          br(),br(),
          fluidPage(
            fluidRow(
              useShinyalert(), 
              actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))
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
                                        `count-selected-text` = "{0}/{1} Countries",
                                        `style` = "btn-primary"),
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
      indicator <- "Inpatient care use, adults"
      region <- region_list$region
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
          y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
          
          # condition on unit of measure
          if(unit_of_measure == '%'){
            pd$pop <- pd$pop*100
            value_range[2] <- value_range[2]*100
            value_range[1] <- value_range[1]*100
            
          }
          # text for plot
          mytext <- paste(
            "Indicator: ", indicator,"<br>", 
            "Economy: ", as.character(pd$country),"<br>", 
            "Value: ", paste0(round(pd$pop, digits = 2), ' (', unit_of_measure, ')'), "<br>",
            "Year: ", as.character(pd$year),"<br>",
            "Data source: ", as.character(pd$referenceid_list), "<br>",
            sep="") %>%
            lapply(htmltools::HTML)
          
          # trend_palette <- colorRampPalette(brewer.pal(name = "Paired", n = 12))(length(unique(pd$country)))
          
          temp <- tableau_color_pal(palette = "Tableau 20")
          trend_palette <- rep(temp(n = 20), 10)
          
          yn <- input$interpolate
          if(yn){
            
            # condition if we connect the dots
            p <- ggplot(data = pd, aes(as.character(year), pop, color= country, text=mytext)) +
                            geom_point() + 
                            geom_line(aes(group = country)) +
                            scale_color_manual(name = '',
                                               values = trend_palette) +
                            scale_y_continuous(limits = c(value_range[1], value_range[2]))+
                            labs(x='Year',
                                 y = y_axis_text,
                                 title = plot_title) +
                            hefpi::theme_gdocs() +
                            theme(panel.grid.major.x = element_blank(),
                                  axis.text.x = element_text(angle = 45, hjust = 1),
                                  axis.ticks = element_blank()) 
     
              
  
            
          } else {
            # condition if we connect the dots
            p <- ggplot(data = pd, aes(as.character(year), pop, color= country, text=mytext)) +
                            geom_point() +
                            scale_color_manual(name = '',
                                               values = trend_palette) +
                            scale_y_continuous(labels = function(x) paste0(x, "%"))+
                            labs(x='Year',
                                 y = y_axis_text,
                                 title = plot_title) +
                            hefpi::theme_gdocs() +
                            theme(panel.grid.major.x = element_blank(),
                                  axis.text.x = element_text(angle = 45, hjust = 1),
                                  axis.ticks = element_blank())
            
            
          }
          
      
          pop_list[[1]] <- p
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
                                        p <- pop_list[[1]]
                                        plot_title = pop_list[[3]][[1]]
                                        mytext = pop_list[[3]][[2]]
                                        y_axis_text = pop_list[[3]][[3]]
                                        unit_of_measure = pop_list[[3]][[4]]
                                        trend_palette = pop_list[[3]][[5]]
                                        
                                        
                                        
                                        if(is.null(pop_list)){
                                          NULL
                                        } else {
                                         p =  p + theme(axis.text = element_text(size = rel(18/12))) +
                                            theme(legend.position = "top") +
                                            theme(legend.direction = "horizontal", 
                                                  legend.text=element_text(size=7)) 
                                          p
                                          ggsave(file, width = 8, height = 8)
                                        }
                                        
                                       
                                      })
    
    output$trends_mean <- renderPlotly({
      
      pop_list <- get_pop_data()
      p <- pop_list[[1]]
      pd <- pop_list[[2]]
      plot_title = pop_list[[3]][[1]]
      mytext = pop_list[[3]][[2]]
      y_axis_text = pop_list[[3]][[3]]
      unit_of_measure = pop_list[[3]][[4]]
      trend_palette = pop_list[[3]][[5]]
      
     
      
      if(is.null(pop_list)){
        NULL
      } else {
        fig <- ggplotly(p, tooltip = 'text')
        fig <- fig %>% config(displayModeBar = F)
        fig
        
      }
        
      
      
    })
}


#' @rdname mod_trends_mean_sub_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_trends_mean_sub_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotlyOutput(
               ns('trends_mean'), height = '600px'
             )),
      column(4,
             pickerInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(sub_national$indicator_short_name)),
                         selected = sort(unique(sub_national$indicator_short_name))[1],
                         options = list(`style` = "btn-primary")),
             pickerInput(inputId = ns("region"),
                         label = 'Region', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1],
                         options = list( `actions-box`=TRUE,
                                         `style` = "btn-primary",
                                         `selected-text-format` = "count > 2",
                                         `count-selected-text` = "{0}/{1} Regions"),
                         multiple = TRUE),
             uiOutput(ns('ui_outputs')),
             sliderInput(ns('value_range'),
                         'Y axis range',
                         min = 0,
                         max = 1,
                         value = c(0, 1),
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
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
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

mod_trends_mean_sub_server <- function(input, output, session){
  
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
    
    indicator <- sort(unique(sub_national$indicator_short_name))[1]
    region <- region_list$region[1]
    date_range <- c(1982, 2018)
    # get inputs
    indicator <- input$indicator
    region <- input$region
    date_range <- input$date_range
    
    # get region code
    region_list <- hefpi::region_list
    region_code <- as.character(region_list$region_code[region_list$region == region])
    
    # Get the variable from indicator input
    # ind_info <- indicators %>%
    #   filter(indicator_short_name == indicator) %>%
    #   select(good_or_bad, unit_of_measure)
    # # variable_name = ind_info$variable_name
    # good_or_bad = ind_info$good_or_bad
    # unit_of_measure = ind_info$unit_of_measure
    # 
    # Get the data to be plotted
    temp <- hefpi::sub_national[sub_national$region_code == region_code,]
    pd <- temp %>%
      filter(indicator_short_name == indicator) %>%
      group_by(ISO3 = iso3c, country,gaul_code) %>%
      filter(year >= min(date_range),
             year <= max(date_range)) 

    
    # get shape files
    shp <- hefpi::gaul
    
    # joine with data
    shp@data <- shp@data %>% dplyr::left_join(pd, by=c('ADM1_CODE'='gaul_code'))
    
    # remove polygons associated with NA - keeps only that region
    na_rows <- which(!is.na(shp@data$value))
    shp <- shp[na_rows,]
    shp@data$ADM1_NAME <- as.character(shp@data$ADM1_NAME)
    df <- shp@data
    
    
    sub_regions <- unique(df$ADM1_NAME)
    
    fluidPage(
      fluidRow(
        pickerInput(inputId = session$ns("sub_country"),
                    label = 'Subnational region', 
                    choices = sub_regions,
                    selected = sub_regions[1:10],
                    options = list( `actions-box`=TRUE,
                                    `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0}/{1} Sub-national regions",
                                    `style` = "btn-primary"),
                    multiple = TRUE)
      )
    )
    
    
  })
  
  get_pop_data <- reactive({
    indicator <- sort(unique(sub_national$indicator_short_name))[1]
    region <- region_list$region[1]
    # temp <- hefpi::df_series %>% filter(region %in% region)
    # sub_regions<- sub_regions
    date_range <- c(1982, 2017)
    value_range <- c(0,1)
    # get inputs
    pop_list <- list()
    indicator <- input$indicator
    region <- input$region
    sub_regions <- input$sub_country
    date_range <- input$date_range
    value_range <- input$value_range
    # get region code 
    region_list <- hefpi::region_list
    indicators <- hefpi::indicators
    df <- hefpi::df
    
    if(is.null(sub_regions) | is.null(value_range)){
      NULL
    } else {
      
      region_code <- as.character(region_list$region_code[region_list$region %in% region])
      
      # get variable
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      unit_of_measure = ind_info$unit_of_measure
      
      temp <- hefpi::sub_national[sub_national$region_code == region_code,]
      pd <- temp %>%
        filter(indicator_short_name == indicator) %>%
        group_by(ISO3 = iso3c, country,gaul_code) %>%
        filter(year >= min(date_range),
               year <= max(date_range)) 
        
      
      # get shape files
      shp <- hefpi::gaul
      
      # joine with data
      shp@data <- shp@data %>% dplyr::left_join(pd, by=c('ADM1_CODE'='gaul_code'))
      
      # remove polygons associated with NA - keeps only that region
      na_rows <- which(!is.na(shp@data$value))
      shp <- shp[na_rows,]
      shp@data$ADM1_NAME <- as.character(shp@data$ADM1_NAME)
      pd <- shp@data
      
      pd <- pd %>% filter(ADM1_NAME %in% sub_regions)
      
      if(nrow(pd) == 0){
        NULL
      } else {
        
        # get title and subtitle
        plot_title <- paste0('Trends - Population mean (sub national)')
        y_axis_text <- paste0(indicator, ' (', unit_of_measure, ')')
        
        # condition on unit of measure
        if(unit_of_measure == '%'){
          pd$value<- pd$value*100
          value_range[2] <- value_range[2]*100
          value_range[1] <- value_range[1]*100
          
        }
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator,"<br>", 
          "Economy: ", as.character(pd$country),"<br>", 
          "Value: ", paste0(round(pd$value, digits = 2), ' (', unit_of_measure, ')'), "<br>",
          "Year: ", as.character(pd$year),"<br>",
          "Data source: ", as.character(pd$referenceid_list), "<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        
        # trend_palette <- colorRampPalette(brewer.pal(name = "Paired", n = 12))(length(unique(pd$country)))
        
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 10)
        
        yn <- input$interpolate
        if(yn){
          
          # condition if we connect the dots
          p <-  ggplot(data = pd, aes(as.character(year), value, color= ADM1_NAME, text=mytext)) +
                          geom_point() + 
                          geom_line(aes(group = ADM1_NAME)) +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          scale_y_continuous(limits = c(value_range[1], value_range[2]))+
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.ticks = element_blank())
          
        } else {
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(as.character(year), value, color= ADM1_NAME, text=mytext)) +
                          geom_point() + 
                          # geom_line(aes(group = ADM1_NAME)) +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          scale_y_continuous(labels = function(x) paste0(x, unit_of_measure), limits = c(value_range[1], value_range[2]))+
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.ticks = element_blank())
        }
        
        
        pop_list[[1]] <- p
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
                                      p <- pop_list[[1]]
                                      plot_title = pop_list[[3]][[1]]
                                      mytext = pop_list[[3]][[2]]
                                      y_axis_text = pop_list[[3]][[3]]
                                      unit_of_measure = pop_list[[3]][[4]]
                                      trend_palette = pop_list[[3]][[5]]
                                      
                                      
                                      
                                      if(is.null(pop_list)){
                                        NULL
                                      } else {
                                        p =  p + theme(axis.text = element_text(size = rel(18/12))) +
                                          theme(legend.position = "top") +
                                          theme(legend.direction = "horizontal", 
                                                legend.text=element_text(size=7)) 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                      
                                      
                                    })
  
  output$trends_mean <- renderPlotly({
    
    pop_list <- get_pop_data()
    p <- pop_list[[1]]
    pd <- pop_list[[2]]
    plot_title = pop_list[[3]][[1]]
    mytext = pop_list[[3]][[2]]
    y_axis_text = pop_list[[3]][[3]]
    unit_of_measure = pop_list[[3]][[4]]
    trend_palette = pop_list[[3]][[5]]
    
    
    
    if(is.null(pop_list)){
      NULL
    } else {
      fig <- ggplotly(p, tooltip = 'text')
      fig <- fig %>% config(displayModeBar = F)
      fig
      
    }
    
    
    
  })
}



#-----------------------------------------------------------------------------------------------------
#' @rdname mod_trends_con_sub_ui
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
               ns('trends_con'), height = '600px'
             )),
      column(4,
             pickerInput(ns('indicator'),
                         'Indicator',
                         choices = sort(unique(indicators$indicator_short_name)),
                         selected = '4+ antenatal care visits',
                         options = list(`style` = "btn-primary")),
             pickerInput(inputId = ns("region"),
                         label = 'Region', 
                         choices = as.character(region_list$region),
                         selected = as.character(region_list$region)[1],
                         options = list( `actions-box`=TRUE,
                                         `style` = "btn-primary",
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
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
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
                                    `style` = "btn-primary",
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
        
        # trend_palette <- colorRampPalette(brewer.pal(name = "Paired", n = 12))(length(unique(pd$country)))
        
        temp <- tableau_color_pal(palette = "Tableau 20")
        trend_palette <- rep(temp(n = 20), 10)
        if(yn){
          
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(as.character(year), CI, color= country, text=mytext)) +
                          geom_point() + 
                          geom_line(aes(group = country)) +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.ticks = element_blank())

        } else {
          # condition if we connect the dots
          p <- ggplot(data = pd, aes(as.character(year), CI, color= country, text=mytext)) +
                          geom_point() +
                          scale_color_manual(name = '',
                                             values = trend_palette) +
                          labs(x='Year',
                               y = y_axis_text,
                               title = plot_title) +
                          hefpi::theme_gdocs() +
                          theme(panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.ticks = element_blank())
        }
        
        
        con_list[[1]] <- p
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
                                      p <- con_list[[1]]
                                      plot_title = con_list[[3]][[1]]
                                      mytext = con_list[[3]][[2]]
                                      y_axis_text = con_list[[3]][[3]]
                                      trend_palette = con_list[[3]][[4]]
                                      
                                      
                                      
                                      if(is.null(con_list)){
                                        NULL
                                      } else {
                                        p =  p + theme(axis.text = element_text(size = rel(18/12))) +
                                          theme(legend.position = "top") +
                                          theme(legend.direction = "horizontal", 
                                                legend.text=element_text(size=7)) 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                      
                                      
                                    })
  
  output$trends_con <- renderPlotly({
    
    con_list <- get_con_data()
    if(is.null(con_list)){
      NULL
    } else {
      p <- con_list[[1]]
      pd <- con_list[[2]]
      plot_title = con_list[[3]][[1]]
      mytext = con_list[[3]][[2]]
      y_axis_text = con_list[[3]][[3]]
      trend_palette = con_list[[3]][[4]]
      
      fig <- ggplotly(p, tooltip = 'text')
      fig <- fig %>% config(displayModeBar = F)
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
               ns('trends_quin'),  height = '600px'
             )),
      column(4,
             pickerInput(ns('country'), '
                         Country',
                         choices = country_list,
                         selected = 'United States',
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('indicator'), 'Indicator',
                         choices = sort(unique(indicators$indicator_short_name)),
                         selected = 'Inpatient care use, adults',
                         options = list(`style` = "btn-primary")),
             pickerInput(ns('view_as'), 'View as',
                         choices =c('Slope chart', 'Line chart'),
                         options = list(`style` = "btn-primary")),
             sliderInput(ns('date_range'),
                         'Date range',
                         min = 1982,
                         max = 2017,
                         value = c(1982, 2017),
                         step = 1,
                         sep = ''),
             uiOutput(ns('ui_outputs')),
             downloadButton(ns("dl_plot"), label = 'Download image', class = 'btn-primary'),
             downloadButton(ns("dl_data"), label = 'Download data', class = 'btn-primary'),
             br(),br(),
             fluidPage(
               fluidRow(
                 useShinyalert(),  # Set up shinyalert
                 actionButton(ns("plot_info"), label = "Plot Info", class = 'btn-primary'))))
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
  
  
  
  output$ui_outputs <- renderUI({
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
    country_names <- 'United States'
    indicator <- 'Inpatient care use, adults'
    # value_range <- c(0.06,0.12)
    date_range <- c(1982,2016)
    # 
    country_names <- input$country
    indicator <- input$indicator
    date_range <- input$date_range

    # get variable
    ind_info <- indicators %>%
      filter(indicator_short_name == indicator) %>%
      select(variable_name, unit_of_measure)
    variable_name = ind_info$variable_name
    unit_of_measure = ind_info$unit_of_measure
    # subset by country and variable
    df <- hefpi::df %>%
      filter(country == country_names) %>%
      filter(indic == variable_name) %>%
      filter(year >= min(date_range),
             year <= max(date_range))  %>%
      select(year, Q1:Q5) 
    temp <- melt(df, id.vars = 'year')
    max_value <- round(max(temp$value, na.rm = TRUE), 2)
    min_value <- round(min(temp$value, na.rm = TRUE), 2)
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
  
  get_quin_data <- reactive({
    country_names <- 'United States'
    indicator <- 'Inpatient care use, adults'
    value_range <- c(0,1)
    date_range <- c(1982,2016)
    view_as <- 'Slope chart'
    # 
    country_names <- input$country
    indicator <- input$indicator
    date_range <- input$date_range
    view_as <- input$view_as
    value_range <- input$value_range
    
    if(is.null(value_range)){
      NULL
    } else {
      quin_list <- list()
      
      # get variable
      ind_info <- indicators %>%
        filter(indicator_short_name == indicator) %>%
        select(variable_name, unit_of_measure)
      variable_name = ind_info$variable_name
      unit_of_measure = ind_info$unit_of_measure
      
      # subset by country and variable
      df <- hefpi::df %>%
        filter(country == country_names) %>%
        filter(indic == variable_name) %>%
        filter(year >= min(date_range),
               year <= max(date_range))  %>%
        select(year, referenceid_list, Q1:Q5)
      
      if(is.null(df)  | nrow(df) ==0){
        NULL
      } else {
        if(view_as == 'Slope chart'){
          # filter to get year_one and year_last
          year_begin = min(df$year)
          year_end = max(df$year)
          df <- df %>%
            filter(year == year_begin | year == year_end)
          # save(df, file = 'df.rda')
        }
        
        df <- melt(df, id.vars = c('referenceid_list','year'))
        
        # recode Quintiels
        df$variable <- ifelse(df$variable == 'Q1', 'Q1: Poorest',
                              ifelse(df$variable == 'Q2', 'Q2: Poor',
                                     ifelse(df$variable == 'Q3', 'Q3: Middle',
                                            ifelse(df$variable == 'Q4', 'Q4: Richer', 'Q5: Richest'))))
        
        # condition on unit of measure
        if(unit_of_measure == '%'){
          df$value <- df$value*100
          value_range[2] <- value_range[2]*100
          value_range[1] <- value_range[1]*100
          
        }
        
        # # get color graident 
        col_vec <- brewer.pal(name = 'Blues', n = length(unique(df$variable)) + 1)
        col_vec <- col_vec[-1]
        
        # make plot title
        plot_title = paste0('Quintile Trends - ', country_names)
        y_axis_text = paste0(indicator, ' (', unit_of_measure, ')')
        # subset by y axis
        
        # text for plot
        mytext <- paste(
          "Indicator: ", indicator, '\n',
          "Economy: ", country_names, '\n',
          "Value: ", paste0(round(df$value, digits = 3), ' (', unit_of_measure, ')'), "\n",
          "Year: ", as.character(df$year),"\n",
          "Data source: ", as.character(df$referenceid_list),"\n",
          sep="") %>%
          lapply(htmltools::HTML)
        
        p <- ggplot(data = df, aes(as.character(year), value, color = variable)) +
          geom_point() +
          geom_line(aes(group = as.character(variable))) +
          scale_color_manual(name = '',
                             values = col_vec) +
          scale_y_continuous(limits = c(value_range[1], value_range[2])) +
          labs(x='Year',
               y = y_axis_text,
               title = plot_title) +
          hefpi::theme_gdocs() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
        quin_list[[1]] <- p
        quin_list[[2]] <- df
        quin_list[[3]] <- list(plot_title, mytext, y_axis_text, col_vec)
        return(quin_list)
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
      quin_list <- get_quin_data()
      
      
      if(is.null(quin_list)){
        df <- quin_list[[2]]
        NULL
      } else {
        
        write.csv(df, file)
      }
    }
  )
  
  # ---- DOWNLOAD MAP IMAGE ---- #
  output$dl_plot <- downloadHandler(filename = paste0(Sys.Date(),"_trends_ci", ".png"),
                                    content = function(file) {
                                      
                                      quin_list <- get_quin_data()
                                      
                                      
                                      
                                      
                                      if(is.null(quin_list)){
                                        NULL
                                      } else {
                                        p <- quin_list[[1]]
                                        plot_title = quin_list[[3]][[1]]
                                        mytext = quin_list[[3]][[2]]
                                        y_axis_text = quin_list[[3]][[3]]
                                        col_vec = quin_list[[3]][[4]]
                                        p =  p + theme(axis.text = element_text(size = rel(18/12))) +
                                          theme(legend.position = "top") +
                                          theme(legend.direction = "horizontal", 
                                                legend.text=element_text(size=7)) 
                                        p
                                        ggsave(file, width = 8, height = 8)
                                      }
                                      
                                      
                                    })
  
  output$trends_quin <- renderPlotly({
    
    quin_list <- get_quin_data()
    
    
    if(is.null(quin_list)){
      NULL
    } else {
      p <- quin_list[[1]]
      pd <- quin_list[[2]]
      plot_title = quin_list[[3]][[1]]
      mytext = quin_list[[3]][[2]]
      y_axis_text = quin_list[[3]][[3]]
      col_vec = quin_list[[3]][[4]]
      if(is.null(p)){
        NULL
      } else {
        fig <- ggplotly(p, tooltip = 'text')
        fig <- fig %>% config(displayModeBar = F)
        fig
      }
      
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

