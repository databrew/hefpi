# Mod social

#' @title mod_social_ui and mod_social_server
#' @description  Make the social section
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_social
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_social_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Social media share buttons
    column(12, align = 'center',
           shinydashboard::box(
             width = 12, 
             # title = "Social Buttons",
             shinydashboardPlus::socialButton(
               url = "https://twitter.com/intent/tweet?text=The%20World%20Bank%20HEFPI%20app&url=https://bohemia.team/hefpi/",
               type = "twitter"
             ),
             
             shinydashboardPlus::socialButton(
               url = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fwww.bohemia.team/hefpi/&title=The+World+Bank+HEFPI+web+app",
               type = "linkedin"
             ),
             
             shinydashboardPlus::socialButton(
               url = "http://www.facebook.com/sharer.php?u=https%3A%2F%2Fwww.bohemia.team/hefpi/",
               type = "facebook"
             ),
             
             shinydashboardPlus::socialButton(
               url = "https://github.com/databrew/hefpi",
               type = "github"
             ),
             a(actionButton(inputId = "email", label = "", 
                            icon = icon("envelope", lib = "font-awesome")),
               href="mailto:?subject=https%3A%2F%2Fwww.bohemia.team/hefpi/&body=https%3A%2F%2Fwww.bohemia.team/hefpi/")
           ),
           shinydashboard::box(width = 12,
                               # URL capture
                               shinyURL.ui(width = '20%',
                                           label = NULL)))
    
  )
}

# Module Server
#' @rdname mod_social_server
#' @export
#' @keywords internal

mod_social_server <- function(input, output, session){
  ns <- session$ns
  
  
  # Right now, no functionality to detect the hostname, etc.
  # And pass the parameters appropriately to the UI
  # (rather than hard-coding them into the UI, as is currently written)
  # This can be implemented later
}

## To be copied in the UI
# mod_social_ui("social_module_1")

## To be copied in the server
# callModule(mod_social_server, "social_module_1")

