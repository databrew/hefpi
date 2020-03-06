#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  

  
  
  # HEADER
  header <- dashboardHeader(title = tags$a(tags$img(src='www/wb_logo.png', alt = 'WBG')))
  
  # SIDEBAR
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        text="Main",
        tabName="main"
        ),
      menuItem(
        text = 'About',
        tabName = 'about'
        )
    )
  )
  
  # BODY
  body <- dashboardBody(
    golem_add_external_resources(),
    tabItems(
      tabItem(
        tabName="main",
        navbarPage(title = '',
                   navbarMenu("Most recent value",
                              tabPanel("Population mean",
                                       fluidPage(column(2), 
                                                 column(5,
                                                        mod_leaflet_ui("leaf1")),
                                                 column(5))),
                              tabPanel("CI")),
                   navbarMenu('Trends',
                              tabPanel('Population mean',
                                       'Quintiles',
                                       'CI')),
                   navbarMenu('Quintile dotplots',
                              tabPanel('Countries',
                                       'Indicators')),
                   navbarMenu('Data availability',
                              tabPanel('By indicator',
                                       'By country')),
                   tabPanel(title = "CSS test page",
                            fluidRow(
                              shinydashboard::box(title = 'This is another box',
                                                  width = 6,
                                                  status = 'warning',
                                                  collapsible = TRUE,
                                                  footer = 'This is a footer',
                                                  plotOutput('plot1')),
                              column(6,
                                     textInput('text',
                                               'Test'),
                                     h1('Big heading (h1)'),
                                     h2('Less big heading (h2)'),
                                     h3('Sort of big heading (h3)'),
                                     h4('Not so big heading (h4)'),
                                     h5('Small heading (h5)'),
                                     h6('Heading w/ background (h6)'))
                            ),
                            fluidRow(
                              column(4,
                                     h4('A bunch of inputs'),
                                     p(selectInput('abc', 'Pick a place', choices = c('Home', 'Away', 'In-between')),
                                       radioButtons('xyz', 'What do you like?', choices = c('Ice cream', 'Pizza', 'Both', 'Neither', 'Ice pizza')),
                                       dateRangeInput('aslk', 'Date range', start = Sys.Date() - 20, end = Sys.Date() - 5),
                                       actionButton('action', 'This is a button', icon = icon('download')),
                                       sliderInput('lakjaasa', 'This is a slider', min = 0, max = 100, value = 25),
                                       textInput('qwer', 'This is some text input'))),
                              column(4,
                                     h4('Here is some regular text'),
                                     p('This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text,
                                       This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text'),
                                     helpText('This is "help text"')),
                              shinydashboard::box(title = 'This is another box',
                                                  width = 4,
                                                  status = 'info',
                                                  collapsible = TRUE,
                                                  footer = 'This is a footer',
                                                  leaflet::leafletOutput('l1')
                              )
                            )
                   )
                   
        ),
        # Social media share buttons
        column(12, align = 'center',
               shinydashboard::box(
          width = 12, 
          # title = "Social Buttons",
          shinydashboardPlus::socialButton(
            url = "http://databrew.cc",
            type = "twitter"
          ),
          
          shinydashboardPlus::socialButton(
            url = "http://databrew.cc",
            type = "linkedin"
          ),
          
          shinydashboardPlus::socialButton(
            url = "http://databrew.cc",
            type = "facebook"
          ),
          
          shinydashboardPlus::socialButton(
            url = "http://github.com",
            type = "github"
          ),
          
          shinydashboardPlus::socialButton(
            url = "http://databrew.cc",
            type = "email"
          )
        ),
        shinydashboard::box(width = 12,
                            # URL capture
                            shinyURL.ui(width = '20%',
                                        label = NULL)))
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
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
