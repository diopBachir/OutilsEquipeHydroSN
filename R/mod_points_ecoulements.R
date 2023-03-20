#' points_ecoulements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_points_ecoulements_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' points_ecoulements Server Functions
#'
#' @noRd 
mod_points_ecoulements_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_points_ecoulements_ui("points_ecoulements_1")
    
## To be copied in the server
# mod_points_ecoulements_server("points_ecoulements_1")
