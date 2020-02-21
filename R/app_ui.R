#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(
      # tags$script(src="www/script.js"),
      # tags$script(src="www/handlers.js"),
      # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
      
    ),
    # List the first level UI elements here 
    fluidPage(
      h1("hefpi")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'hefpi')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
