#' @rawNamespace import(shiny, except = runExample)
#' @import shinydashboard
#' @rawNamespace import(shinyWidgets, except = alert)
#' @rawNamespace import(shinyjs, except = show)
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import tidyr
#' @import htmltools
#' @import RColorBrewer
#' @import sp
#' @import leaflet
#' @import waiter
#' @import plotly
#' @importFrom shiny NS tagList 


app_server <- function(input, output,session) {
  suppressWarnings({
  
  w <- waiter::Waiter$new(html = spin_loader(), color = "#FFF")
  webshot::install_phantomjs()
  # Capture URL parameters
  # shinyURL.server()

  # MOST RECENT VALUE MAPS
  shiny::callModule(mod_recent_mean_server, 'recent_mean_leaf')
  shiny::callModule(mod_recent_radar_server, 'recent_radar')
  shiny::callModule(mod_recent_con_server, 'recent_con_leaf')
  shiny::callModule(mod_recent_mean_sub_server, 'recent_mean_sub_leaf')
  # callModule(mod_recent_con_sub_server, 'recent_con_sub_leaf1')
  
  # TRENDS TAB
  shiny::callModule(mod_trends_mean_server, 'trends_mean')
  shiny::callModule(mod_rural_server, 'rural')
  
  shiny::callModule(mod_trends_quin_server, 'trends_quin')
  shiny::callModule(mod_trends_con_server, 'trends_con')
  
  # QUINTILES TAB
  shiny::callModule(mod_dots_country_server, 'dots_country')
  shiny::callModule(mod_dots_ind_server, 'dots_ind')
  
  # DATA AVAILABILITY TAB
  shiny::callModule(mod_dat_country_server, 'dat_country')
  shiny::callModule(mod_dat_ind_server, 'dat_ind')
  
  ## Social
  # callModule(mod_social_server, "social_module_1")
  
  # data availability alternate tab
  # callModule(mod_dat_country_alt_server, 'dat_country_alt1')
  # callModule(mod_dat_ind_alt_server, 'dat_ind_alt1')
  
  
  
  output$style_tag <- shiny::renderUI({

    
    if(input$sidebar=='about' || input$sidebar=='docu'){
      
      return( (tags$head(tags$style(HTML('
                                       .skin-blue .main-header .navbar {
                                            background-image: url(www/hefpi_banner.png) !important;
                                            background-size: cover !important;
                                            margin: 0px;
                                            height: 450px !important;
                                            background-position-x: center;
                                        }
                                            
                                        #sidebarCollapsed {
                                            padding-top: 451px !important;
                                        }
                                      
                                        .content {
                                          margin-top: 345px !important;
                                        }
                                        
                                       ')))
     #         tags$script(HTML('
     #              $(document).ready(function() {
     #                  $( ".myClass" ).remove();
     #                  $("header").find("nav").append(\'<span class="myClass"></span>\');
     #        
     #              })
     # '))
      )
             )
      
    } else {
      return(
      (
        tags$head(tags$style(HTML('
                                       .skin-blue .main-header .navbar {
                                            background-image: none !important;
                                            background-size: cover !important;
                                            margin: 0px;
                                            height: 102px !important;
                                            background-color: #FFF !important;
                                        }
                                            
                                        #sidebarCollapsed {
                                            padding-top: 102px !important;
                                        }
                                        
                                        .content {
                                          margin-top: 0px !important;
                                        }
                                        
                                        
                                       ')))
      
     #  tags$script(HTML('
     #  
     #  $(document).ready(function() {
     #  
     #      $( ".myClass" ).remove();
     #      $("header").find("nav").append(\'<span class="myClass"> Health Equity and Financial Protection Indicators (HEFPI)</span>\');
     # 
     # 
     #  })
     #  
     # '))
      )
      )
    }
    
    # give time for wait screen to show
    Sys.sleep(3) 
    hide_waiter()
    
  })
  
  output$script_tag <- shiny::renderUI({
    if(input$sidebar=='about' || input$sidebar =='docu'){
      # $("header").find("nav").append(\'<div class="headerTitleCust"></div>\');
      return( (
        tags$script(HTML('
          $(document).ready(function() {
            $(".headerTitleCust").remove()
            $("header").find("nav").append(\'<div class="headerTitleCust headerTitleAboutDocPosition"></div>\');
          })
         '))
        )
      )
      
    } else {
      return(
        (
          tags$script(HTML('
            $(document).ready(function() {
              $(".headerTitleCust").remove()
              $("header").find("nav").append(\'<div class="headerTitleCust"></div>\');
            })
           '))
        )
      )
    }
    
    # give time for wait screen to show
    Sys.sleep(3) 
    hide_waiter()
    
  })
  
  shiny::observeEvent(input$sidebar, {
    w$show()
    Sys.sleep(3) # give time for wait screen to show
    w$hide()
    waiter_hide() # will hide *on_load waiter
  })
  

  output$plot1 <- shiny::renderPlot({
    barplot(1:10, col = grey(seq(0, 1, length = 10)),
            main = 'This is a plot',
            sub = "You can't modify it from html/css, it's an image file")
  })
  
  library(leaflet)
  output$l1 <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles('Esri.WorldImagery') %>%
      leaflet::addMarkers(data = tibble(lng = rnorm(10, sd = 40),
                               lat = rnorm(10, sd = 40)))
  })
  
  shiny::observeEvent(input$action, {
    shiny::showModal(
      shiny::modalDialog(title = 'This is a modal',
                  size = 'l',
                  easyClose = TRUE,
                  fade = TRUE,
                  shiny::fluidPage(
                    shiny::fluidRow(
                      shiny::column(6, h3('Here is some text'),
                             p('And some sub-text')),
                      shiny::column(6, h3('Here is some more text'),
                                    shiny::helpText('And some helper text'))
                    )
                  ))
    )
  })
  
  })
  
}
