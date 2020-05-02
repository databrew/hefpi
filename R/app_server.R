#' @import shiny
#' @import shinyURL
app_server <- function(input, output,session) {
  
  # Capture URL parameters
  shinyURL.server()
  
  ## Social
  callModule(mod_social_server, "social_module_1")
  
  # List the first level callModules here
  callModule(mod_leaflet_mean_server, 'leaf1')
  callModule(mod_leaflet_con_server, 'con1')
  callModule(mod_pop_mean_server, 'pop_mean1')

  output$plot1 <- renderPlot({
    barplot(1:10, col = grey(seq(0, 1, length = 10)),
            main = 'This is a plot',
            sub = "You can't modify it from html/css, it's an image file")
  })
  
  library(leaflet)
  output$l1 <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(data = tibble(lng = rnorm(10, sd = 40),
                               lat = rnorm(10, sd = 40)))
  })
  
  observeEvent(input$action, {
    showModal(
      modalDialog(title = 'This is a modal',
                  size = 'l',
                  easyClose = TRUE,
                  fade = TRUE,
                  fluidPage(
                    fluidRow(
                      column(6, h3('Here is some text'),
                             p('And some sub-text')),
                      column(6, h3('Here is some more text'),
                             helpText('And some helper text'))
                    )
                  ))
    )
  })
  
}
