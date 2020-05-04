# Module data availability

#' @title   mod_dat.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dat_country_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotOutput(
               ns('dat_country')
             )),
      column(4,
             selectInput(ns('country'), 'Country',
                         choices = country_list[[1]]))
    )
  )
}

# Module Server
#' @rdname mod_dat_country_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dat_country_server <- function(input, output, session){
  
  
  
  output$dat_country <- renderPlot({
    ggplot() + labs(title = 'In progress')
  })
}

#-------------------------------------------------------------------------------------------------------------

#' @rdname mod_dat_ind_ui
#'
#' @keywords internal
#' @export 
#' @import tidyverse
#' @import ggplot2
#' @import reshape2
#' @importFrom shiny NS tagList 
mod_dat_ind_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      column(8,
             plotOutput(
               ns('dat_ind')
             )),
      column(4,
             selectInput(ns('region'), 'Region',
                         choices = region_list[[1]]),
             selectInput(ns('indicator'), 'Indicator',
                         choices = indicators_list))
    )
  )
}

# Module Server
#' @rdname mod_dat_ind_server
#' @export
#' @import tidyverse
#' @import RColorBrewer
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import reshape2
#' @import htmltools
#' @keywords internal

mod_dat_ind_server <- function(input, output, session){
  
  
  
  output$dat_ind <- renderPlot({
    ggplot() + labs(title = 'In progress')
  })
}

## To be copied in the UI
# mod_dat_country_ui("dat_country1")
# mod_dat_country_ui("dat_ind1")


## To be copied in the server
# callModule(mod_dat_country_server, 'dat_country1')
# callModule(mod_dat_ind_server, 'dat_ind1')




