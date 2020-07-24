#' @import shiny
#' @import shinydashboard
app_ui <- function() {

  
  # HEADER
  header <- dashboardHeader(title = tags$a(tags$img(src='www/wb_logo.png', alt = 'WBG')))
  
  # SIDEBAR
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        text="Population mean",
        tabName="population_mean"
      ),
      menuItem(
        text="Inequality",
        tabName="inequality"
      ),
      menuItem(
        text="Data availability",
        tabName="data"
      ),
      menuItem(
        text="CSS page",
        tabName="css_page"
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
        tabName="population_mean",
        navbarPage(title = '',
                   navbarMenu("Most recent value",
                              tabPanel("National mean",
                                       mod_recent_mean_ui("recent_mean_leaf1")),
                              tabPanel("Subnational mean",
                                       mod_recent_mean_sub_ui("recent_mean_sub_leaf1"))),
                   navbarMenu('Trends',
                              tabPanel('National mean',
                                       mod_trends_mean_ui("trends_mean1")),
                              tabPanel('Subnational mean',
                                       mod_trends_mean_sub_ui("trends_sub_mean1"))))),
      tabItem(
        tabName="inequality",
        navbarPage(title = '',
                   navbarMenu("Quintiles",
                              tabPanel("Most recent value (National)",
                                       mod_dots_country_ui('dots_country1')),
                              tabPanel("Most recent value (Indicators)",
                                       mod_dots_ind_ui('dots_ind1')),
                              tabPanel("Trends",
                                       mod_trends_quin_ui("trends_quin1"))),
                   navbarMenu('Concentration index',
                              tabPanel('Most recent value',
                                       mod_recent_con_ui("recent_con_leaf1")),
                              tabPanel('Trends',
                                       mod_trends_con_ui("trends_con1"))))),
      tabItem(
        tabName="data",
        navbarPage(title = '',
                   navbarMenu("Data Availability",
                              tabPanel("Country",
                                       mod_dat_country_ui('dat_country1')),
                              tabPanel('Indicator',
                                       mod_dat_ind_ui('dat_ind1'))))),
      tabItem(
        tabName = 'css_page',
        navbarPage(title ='Css',
                   tabPanel(title = "CSS test page",
                            fluidRow(
                              shinydashboard::box(title = 'This is another box',
                                                  width = 6,
                                                  status = 'info',
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
                                     selectInput('abc', 
                                                 'Pick a place', 
                                                 choices = c('Home', 'Away', 'In-between')),
                                     radioButtons('xyz', 
                                                  'What do you like?', 
                                                  choices = c('Ice cream', 'Pizza', 'Both', 'Neither', 'Ice pizza')),
                                     dateRangeInput('aslk', 
                                                    'Date range', 
                                                    start = Sys.Date() - 20, 
                                                    end = Sys.Date() - 5),
                                     actionButton('action', 
                                                  'This is a button', 
                                                  icon = icon('download')),
                                     sliderInput('lakjaasa', 
                                                 'This is a slider', 
                                                 min = 0,
                                                 max = 100, 
                                                 value = 25),
                                     textInput('qwer', 'This is some text input')),
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
                              )))
                   )
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
