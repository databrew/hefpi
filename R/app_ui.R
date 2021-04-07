#' @import shiny
#' @import shinydashboard
#' @import shinyURL
#' @import shinyWidgets
#' @import shinyjs
#' @import ggplot2
#' @import tidyr
#' @import htmltools
#' @import RColorBrewer
#' @import sp
#' @import leaflet
#' @import plotly
#' @import mapview
#' @import shinyalert
#' @importFrom shiny NS tagList 
app_ui <- function() {

  # HEADER
  header <- dashboardHeader(title = 'HEFPI')#tags$a(tags$img(src='www/wb_logo.png', alt = 'WBG')))
  
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
                                       mod_recent_mean_ui("recent_mean_leaf")),
                              tabPanel("Subnational mean",
                                       mod_recent_mean_sub_ui("recent_mean_sub_leaf"))),
                   navbarMenu('Trends',
                              tabPanel('National mean',
                                       mod_trends_mean_ui("trends_mean")),
                              tabPanel('Subnational mean',
                                       mod_trends_mean_sub_ui("trends_sub_mean"))))),
      tabItem(
        tabName="inequality",
        navbarPage(title = '',
                   navbarMenu('Concentration index',
                              tabPanel('Most recent value',
                                       mod_recent_con_ui("recent_con_leaf")),
                              tabPanel('Trends',
                                       mod_trends_con_ui("trends_con"))),
                   navbarMenu("Quintiles",
                              tabPanel("Most recent value by country",
                                       mod_dots_country_ui('dots_country')),
                              tabPanel("Most recent value by indicator",
                                       mod_dots_ind_ui('dots_ind')),
                              tabPanel("Trends",
                                       mod_trends_quin_ui("trends_quin")))
                  
                   )),
      tabItem(
        tabName="data",
        navbarPage(title = '',
                   navbarMenu("Data availability",
                              tabPanel("By country",
                                       mod_dat_country_ui('dat_country')),
                              tabPanel('By indicator',
                                       mod_dat_ind_ui('dat_ind'))))),
      tabItem(
        tabName = 'about',
        fluidPage(
          fluidRow(
            column(12,
                   h4('Health Equity and Financial Protection Indicators (HEFPI)'),
                   br(),
                   p('Most health systems aspire to deliver health services to people who need them, without causing financial hardship for the families involved. How close do health systems around the world come to achieving this goal of universal health coverage?'),
                   p('What percentage of children and women in different countries get key preventive health interventions such as antenatal care and cervical cancer screening? How does this percentage differ between the poor and less poor? What percentage of adults in different countries receive inpatient care, and how does this percentage compare to the WHO benchmark? What does the gradient across socioeconomic groups look like, and does it look different in low-, middle- and high-income countries? What are the gaps between the poor and the less poor in health outcomes, such as childhood stunting and adult obesity?'),
                   p('What fraction of households spend more than 10% of their income or out-of-pocket consumption on health care? Is the fraction higher among the poor? What fraction of households are impoverished by out-of-pocket health expenses? How do these numbers vary across countries? How have they changed over time?'),
                   p('The Health Equity and Financial Protection Indicators (HEFPI) dataset allows you to answer these questions. The dataset has grown over time from the first dataset published in 2000 which pulled data from 42 surveys and one type of survey, covered just 42 countries, and included just 34 indicators, which all concerned maternal and child health. In 2013, for the first time, the database included household out-of-pocket health expenditures, noncommunicable disease indicators (NCD), and data from high-income countries. The 2018 database follows this trend by employing over 1,600 surveys, covering 183 countries, and encompassing multiple years of data, richer NCD data, and more extensive data on household out-of-pocket expenditures.')
            )
       
          ),
          br(), br(),
          fluidRow(
            column(3, 
                   shiny::actionButton(inputId='link1', label="Download full data", 
                                       icon = icon("bar-chart-o"), 
                                       onclick ="window.open('https://datacatalog.worldbank.org/dataset/hefpi', '_blank')")),
            
            column(1, 
                   shiny::actionButton(inputId='link2', label="Read publication", 
                                       icon = icon("file-alt"), 
                                       onclick ="window.open('https://openknowledge.worldbank.org/handle/10986/31869', '_blank')"))),
        
          br(), br(), br(), br(), br(),br(), br(), br(),br(), br(),br(), br(), br(),
          fluidRow(
            column(1, shinyWidgets::circleButton(inputId = 'fb', icon = icon('facebook'), status = "default", size ='default', onclick ="window.open('https://www.facebook.com/worldbank', '_blank')", style="float:middle")),
            column(1,  shinyWidgets::circleButton('twitter', icon = icon('twitter'), status = "default", size = "default", onclick ="window.open('https://www.twitter.com/worldbank', '_blank')", style="float:middle")),
            column(1,  shinyWidgets::circleButton('linkedin', icon = icon('linkedin'), status = "default", size = "default", onclick ="window.open('https://www.linkedin.com/company/the-world-bank/', '_blank')", style="float:middle")),
            column(1,  shinyWidgets::circleButton('instagram', icon = icon('instagram'), status = "default", size = "default", onclick ="window.open('https://www.instagram.com/worldbank', '_blank')", style="float:middle")),
            column(1,  shinyWidgets::circleButton('youtube', icon = icon('youtube'), status = "default", size = "default", onclick ="window.open('https://www.youtube.com/channel/UCE9mrcoX-oE-2f1BL-iPPoQ', '_blank')", style="float:middle")),
            column(1,  shinyWidgets::circleButton('flickr', icon = icon('flickr'), status = "default", size = "default", onclick ="window.open('https://www.flickr.com/photos/worldbank', '_blank')", style="float:middle")),
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
