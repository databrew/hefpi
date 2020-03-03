#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  
  # HEADER
  header <- dashboardHeader(title = tags$a(tags$img(src='www/wbg-favicon.png',height='32', alt = 'WBG')))
  
  # SIDEBAR
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        text="Main",
        tabName="main",
        icon=icon("archway")),
      menuItem(
        text = 'About',
        tabName = 'about',
        icon = icon("cog", lib = "glyphicon"))
    )
  )
  
  # BODY
  body <- dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName="main",
      ),
      tabItem(
        tabName = 'about',
        fluidPage(
          fluidRow(
            div(img(src='www/logo.png', align = "center"), style="text-align: center;"),
            h4('Built in partnership with ',
               a(href = 'http://databrew.cc',
                 target='_blank', 'Databrew'),
               align = 'center'),
            p('Empowering research and analysis through collaborative data science.', align = 'center'),
            div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                               icon = icon("envelope", lib = "font-awesome")),
                  href="mailto:info@databrew.cc",
                  align = 'center')), 
            style = 'text-align:center;'
          )
        )
      )
    )
  )
  
  tagList(
    golem_add_external_resources(),
    # UI
    dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body, 
      skin="blue", title = 'databrew')
    
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'hefpi')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon()#,
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
