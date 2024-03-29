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
#' @import plotly
#' @import mapview
#' @import forcats
#' @import stringr
#' @import waiter
#' @rawNamespace import(shinyalert, except = runExample)
#' @importFrom shiny NS tagList 
#' @import shinycssloaders


app_ui <- function() {
  
  header <- shinydashboard::dashboardHeader(title  = '')
  
  # SIDEBAR
  sidebar <- shinydashboard::dashboardSidebar(
    width = 230,
    shinydashboard::sidebarMenu(
      id = 'sidebar',
      width = 230,
      # shinydashboard::menuItem(
      #   text = 'About HEFPI',
      #   tabName = 'about'
      # ),
      shinydashboard::menuItem(
        id = "vizualizationIDbar",
        text = 'HEFPI Visualizations',
        tabName = 'hefpi_vis',
        startExpanded = TRUE,
        shinydashboard::menuSubItem(
          text=" Country Level",
          tabName="national",
          icon = shiny::icon("angle-right")
        ),
        shinydashboard::menuSubItem(
          text="By Subnational region",
          tabName="subnational_region",
          icon = shiny::icon("angle-right")
        ),
        shinydashboard::menuSubItem(
          text="By urban-rural",
          tabName="urban_rural",
          icon = shiny::icon("angle-right")
        ),
        shinydashboard::menuSubItem(
          text="By Household wealth",
          tabName="household_wealth",
          icon = shiny::icon("angle-right")
        ),
        shinydashboard::menuSubItem(
          text="Data availability",
          tabName="data",
          icon = shiny::icon("angle-right")
        )
      )
      # shinydashboard::menuItem(
      #   text = 'Documentation', 
      #   tabName = 'docu'
      #   ),
      # shinydashboard::menuItem(
      #   text = 'Download full data', 
      #   href = 'https://datacatalog.worldbank.org/dataset/hefpi'
      # )
      
    )
  )
  
  # BODY
  body <- shinydashboard::dashboardBody(
    # waiter::use_waiter(),
    # waiter::waiter_show_on_load(html = spin_loader(), color = "#fff"),
    # waiter_show_on_load(spinner), # will show on load
    # waiter_show_on_load(html = spin_1(), color = "#333e48"),
   
  # HEADER
    tags$head(tags$style(HTML(
      ' .headerTitleCust {
          font-size: 20px;
          line-height: 100px;
          text-align: center;
          font-family: "Open Sans", sans-serif;
          padding: 0px 0px 0px 0px;
          overflow: hidden;
          color: #FFF;
          margin-right: 20%;
      }
    '))),
    shiny::uiOutput('style_tag'),
    shiny::uiOutput('script_tag'),
    tags$script(HTML('
      $(document).ready(function() {
        $("#recent_radar-generate_chart").click(function(){
          $("#sidebarCollapsed").attr( "data-collapsed", true);
                             $("body").addClass("sidebar-collapse")
        });
      })
     ')),
    golem_add_external_resources(),
    # tags$script(src = "www/script.js"),
    
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName="national",
        shiny::navbarPage(title = '',
                          shiny::navbarMenu('Most recent value',
                                            shiny::tabPanel("Map",
                                              mod_recent_mean_ui("recent_mean_leaf")
                     )
                     ,
                     shiny::tabPanel("Radar plot",
                                     mod_recent_radar_ui("recent_radar")
                     )
                     ),
                     
                     shiny::navbarMenu("Trends",
                                       shiny::tabPanel("Trends by indicator",
                                                       mod_trends_mean_ui("trends_mean")
                                                       ),
                                       shiny::tabPanel("Trends by country",
                                                       mod_trends_mean_by_country_ui('trends_mean_country')
                                                       ),
                                       )
                     
                   
                   # navbarMenu("Most recent value",
                   #            tabPanel("National mean",
                   #                     mod_recent_mean_ui("recent_mean_leaf"))),
                   # navbarMenu('Trends',
                   #            tabPanel('National mean',
                   #                     mod_trends_mean_ui("trends_mean")))
                   
                   )
        ),
      shinydashboard::tabItem(
        tabName="subnational_region",
        # navbarPage(title = '',
           mod_recent_mean_sub_ui("recent_mean_sub_leaf")
          #,
          # tabPanel("Trends",
          #          mod_trends_mean_sub_ui("trends_sub_mean")
          # )
        # )
        # navbarPage(title = '',
        #            navbarMenu("Most recent value",
        #                       tabPanel("Subnational mean",
        #                                mod_recent_mean_sub_ui("recent_mean_sub_leaf"))),
        #            navbarMenu('Trends',
        #                       tabPanel('Subnational mean',
        #                                mod_trends_mean_sub_ui("trends_sub_mean"))))
        ),
      
      shinydashboard::tabItem(
        tabName="urban_rural",
        # navbarPage(title = '',
                   # tabPanel(  "Most recent value",
                            mod_rural_ui("rural")
                   # )#,
                   # tabPanel("Trends",
                   #          mod_trends_mean_sub_ui("trends_sub_mean")
                   # )
        # )
        # navbarPage(title = '',
        #            navbarMenu("Most recent value",
        #                       tabPanel("Subnational mean",
        #                                mod_recent_mean_sub_ui("recent_mean_sub_leaf"))),
        #            navbarMenu('Trends',
        #                       tabPanel('Subnational mean',
        #                                mod_trends_mean_sub_ui("trends_sub_mean"))))
      ),
      
      shinydashboard::tabItem(
        tabName="household_wealth",
        shiny::navbarPage(title = '',
                          shiny::navbarMenu('Concentration index',
                                            shiny::tabPanel('Most recent value',
                                              mod_recent_con_ui("recent_con_leaf")),
                                            shiny::tabPanel('Trends',
                                              mod_trends_con_ui("trends_con"))),
                          shiny::navbarMenu("Quintiles",
                                            shiny::tabPanel("Most recent value by country",
                                                mod_dots_country_ui('dots_country')),
                                            shiny::tabPanel("Most recent value by indicator",
                                                mod_dots_ind_ui('dots_ind')),
                                            shiny::tabPanel("Trends",
                                                mod_trends_quin_ui("trends_quin")))
                  
                   )),
      shinydashboard::tabItem(
        tabName="data",
        shiny::navbarPage(title = '',
                          shiny::navbarMenu("Data availability",
                                            shiny::tabPanel("By country",
                                                    mod_dat_country_ui('dat_country')),
                                            shiny::tabPanel('By indicator',
                                                    mod_dat_ind_ui('dat_ind'))))),
      shinydashboard::tabItem(
        tabName = 'about', 
        shiny::fluidPage(class = "about-fluid-page",
                         shiny::fluidRow(
            # div(img(src= 'www/hefpi_banner.png', height = '300px', width = '1200px'), style = 'text-align:center;'),

            
            
            
                           shiny::column(12,
                                     br(),
                                     p('Most health systems aspire to deliver health services to people who need them, without causing financial hardship for the families involved. How close do health systems around the world come to achieving this goal of universal health coverage?'),
                                     p('What percentage of children and women in different countries get key preventive health interventions such as antenatal care and cervical cancer screening? How does this percentage differ between the poor and less poor? What percentage of adults in different countries receive inpatient care, and how does this percentage compare to the WHO benchmark? What does the gradient across socioeconomic groups look like, and does it look different in low-, middle- and high-income countries? What are the gaps between the poor and the less poor in health outcomes, such as childhood stunting and adult obesity?'),
                                     p('What fraction of households spend more than 10% of their income or out-of-pocket consumption on health care? Is the fraction higher among the poor? What fraction of households are impoverished by out-of-pocket health expenses? How do these numbers vary across countries? How have they changed over time?'),
                                     p('The Health Equity and Financial Protection Indicators (HEFPI) dataset allows you to answer these questions. The dataset has grown over time from the first dataset published in 2000 which pulled data from 42 surveys and one type of survey, covered just 42 countries, and included just 34 indicators, which all concerned maternal and child health. In 2013, for the first time, the database included household out-of-pocket health expenditures, noncommunicable disease indicators (NCD), and data from high-income countries. A major extension in 2018 and a subsequent 2019 update increased the number of household surveys included in HEFPI to over 1,700, covering 197 countries, and added richer NCD data as well as more extensive data on household out-of-pocket expenditures. The current 2022 version of the HEFPI database continues this trend, sourcing from almost 1,900 household surveys and covering 204 countries, and extending the number of indicators to 104, including new indicators of financial protection and, for the first time, indicators of effective health coverage and quality. Finally, the 2022 version of the database provides insights into regional inequities in indicators by making them available by urban and rural area of residence and by subnational regions.')
            )
       
          ),
          # tags$div(class = "row", 
          #   tags$div(class = "cards-section",
          #     column(4,
          #              box(
          #                fluidRow(
          #                  div(img(src= 'www/card_1.jpg', width = '100%'), style = 'text-align:center;')),
          #                  footer = tags$div(class="header", checked=NA,
          #                                    tags$h4("A History of the World Bank's Health Equity and Financial Protection Indicators"),
          #                                    tags$p("This project – a collaboration between the Bank’s research group, data group, and health, nutrition and population global – stretches back to 2000."),
          #                                    tags$a(href="https://datatopics.worldbank.org/health-equity-and-financial-protection/history.html", "Read more", style="color:#009FDA")
          #                  ),
          #                  width =12,
          #                
          #              )
          #         ),
          #     column(4, 
          #       box(
          #         fluidRow(
          #           div(img(src= 'www/card_2.jpg', width = '100%'), style = 'text-align:center;')),
          #         footer = tags$div(class="header", checked=NA,
          #                           tags$h4("Did the Poor Get Left Behind by the Health MDGs?"),
          #                           tags$p("One question that is often asked is whether the focus on population averages in the MDGs resulted in the poor being left behind. The HEFPI dataset allows us to answer this with some precision."),
          #                           tags$a(href="https://datatopics.worldbank.org/health-equity-and-financial-protection/left_behind.html", "Read more", style="color:#009FDA")
          #         ),
          #         width =12,
          #         
          #       )
          #     ),
          #     column(4,
          #       box(
          #         fluidRow(
          #           div(img(src= 'www/card_3.jpg', width = '100%'), style = 'text-align:center;')),
          #         footer = tags$div(class="header", checked=NA,
          #                           tags$h4("Tracking Progress Towards UHC Using the HEFPI Database"),
          #                           tags$p("The idea underlying Universal Health Coverage (UHC) is that everyone, irrespective of their means, receives the health services they need, without suffering financial hardship in the process."),
          #                           tags$a(href="https://datatopics.worldbank.org/health-equity-and-financial-protection/tracking_progress.html", "Read more", style="color:#009FDA")
          #         ),
          #         width =12,
          #         
          #       )
          #     )
          #   )
          # ),
          br(), br(),
          # fluidRow(
          #   column(6,
          #   shiny::actionButton(inputId='link1', label="Download full data", 
          #                       icon = icon("bar-chart-o"), 
          #                       onclick ="window.open('https://datacatalog.worldbank.org/dataset/hefpi', '_blank')"), align = 'center'),
          #   column(6,
          #          
          #   shiny::actionButton(inputId='link2', label="Documentation", 
          #                       icon = icon("file-alt"), 
          #                       onclick ="window.open('https://openknowledge.worldbank.org/handle/10986/31869', '_blank')"), align = 'center'),
          #          ),
        
          br(), br(),
          shiny::fluidRow(
            shiny::column(12,
            shinyWidgets::circleButton(inputId = 'fb', icon = icon('facebook'), status = "default", size ='default', onclick ="window.open('https://www.facebook.com/worldbank', '_blank')"),
            shinyWidgets::circleButton('twitter', icon = icon('twitter'), status = "default", size = "default", onclick ="window.open('https://www.twitter.com/worldbank', '_blank')"),
            shinyWidgets::circleButton('linkedin', icon = icon('linkedin'), status = "default", size = "default", onclick ="window.open('https://www.linkedin.com/company/the-world-bank/', '_blank')"),
            shinyWidgets::circleButton('instagram', icon = icon('instagram'), status = "default", size = "default", onclick ="window.open('https://www.instagram.com/worldbank', '_blank')", style="float:middle"),
            shinyWidgets::circleButton('youtube', icon = icon('youtube'), status = "default", size = "default", onclick ="window.open('https://www.youtube.com/channel/UCE9mrcoX-oE-2f1BL-iPPoQ', '_blank')"),
            shinyWidgets::circleButton('flickr', icon = icon('flickr'), status = "default", size = "default", onclick ="window.open('https://www.flickr.com/photos/worldbank', '_blank')", style="float:middle"), 
            align = 'center')
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = 'docu', 
        shiny::fluidPage(
          # fluidRow(
          #   div(img(src= 'www/hefpi_banner.png', height = '300px', width = '1200px'), style = 'text-align:center;')
          # ), 
          br(), br(),
          shiny::fluidRow(
            shiny::column(3,
                   div(img(src= 'www/working_paper_1.png', height = '400px', width = '100%'), style = 'text-align:left;') 
                   ),
            shiny::column(9,
                   tags$div(class="header", checked=NA,
                            tags$h4("The 2019 Update of the Health Equity and Financial Protection Indicators Database : An Overview",  style="color:black"),
                            tags$p("This paper outlines changes that have been made in the 2019 version of the Health Equity and Financial Protection Indicators database. On the financial protection side, the changes include an increase in the number of indicators from five to 14; revisions to several previous data points, reflecting the analysis of new surveys (or adaptations thereof); and refinements to the estimation of out-of-pocket expenditures. On the health equity side, the 2019 database includes 198 more data points than the 9,733 in the 2018 database, reflecting the addition of 535 new datapoints, and the dropping of 337 previously included data points now considered to be substandard.",  style="color:black"),
                            br(), br(), 
                            tags$p("Citation",  style="color:black;font-weight; bold;"),
                            tags$a("Wagstaff, Adam; Eozenou, Patrick; Neelsen, Sven; Smitz, Marc. 2019. The 2019 Update of the Health Equity and Financial Protection Indicators Database : An Overview. Policy Research Working Paper;No. 8879. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/31869 License: CC BY 3.0 IGO.",  style="color:black"),br(), br(), 
                            tags$p('URL'),
                            tags$a('http://hdl.handle.net/10986/31869', href = 'http://hdl.handle.net/10986/31869'),
                            br(), br(),
                            tags$p('Collection(s)'),
                            tags$a('Polic Research Working Papers', href = 'https://openknowledge.worldbank.org/handle/10986/9'),
                            br(), br(),
                            tags$p('Dataset(s)'),
                            tags$a('https://datacatalog.worldbank.org/node/142861', href = 'https://datacatalog.worldbank.org/node/142861'),
                   ) )
          ),
          hr(style = "border-top: 2px solid black;"),         
          shiny::fluidRow(
            shiny::column(3,
                   div(img(src= 'www/working_paper_2.png', height = '450px', width = '100%'), style = 'text-align:left;') 
            ),
            shiny::column(9,
                   tags$div(class="header", checked=NA,
                            # tags$h2("Working papers", style = "color:grey"),
                            tags$h4("The 2018 Health Equity and Financial Protection Indicators Database : Overview and Insights",  style="color:black"),
                            tags$p("The 2018 database on Health Equity and Financial Protection indicators provides data on equity in the delivery of health service interventions and health outcomes, and on financial protection in health. This paper provides a brief history of the database, gives an overview of the contents of the 2018 version of the database, and then gets into the details of the construction of its two sides -- the health equity side and the financial protection side. The paper also provides illustrative uses of the database, including the extent of and trends in inequity in maternal and child health intervention coverage, the extent of inequities in women's cancer screening and inpatient care utilization, and trends and inequalities in the incidence of catastrophic health expenditures.",  style="color:black"),
                            br(), br(), 
                            tags$p("Citation",  style="color:black;font-weight; bold;"),
                            tags$a("Wagstaff, Adam; Eozenou, Patrick; Neelsen, Sven; Smitz, Marc. 2018. The 2018 Health Equity and Financial Protection Indicators Database : Overview and Insights. Policy Research Working Paper;No. 8577. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/30598 License: CC BY 3.0 IGO.",  style="color:black"),br(), br(), 
                            tags$p('URL'),
                            tags$a('http://hdl.handle.net/10986/30598', href = 'http://hdl.handle.net/10986/30598'),
                            br(), br(),
                            tags$p('Collection(s)'),
                            tags$a('Polic Research Working Papers', href = 'https://openknowledge.worldbank.org/handle/10986/9'),
                            br(), br(),
                            tags$p('Dataset(s)'),
                            tags$a('https://datacatalog.worldbank.org/node/142861', href = 'https://datacatalog.worldbank.org/node/142861'),
                   ) )
          ),
          
          
        )
        # href = 'https://openknowledge.worldbank.org/handle/10986/31869'

        
      )
    )
  )
  
  tagList(
    golem_add_external_resources(),
    # UI
    shinydashboard::dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body)
  )
}

#' @rawNamespace import(shiny, except = runExample)
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'hefpi')
  )
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}


