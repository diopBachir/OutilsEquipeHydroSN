#' nasapower_api_embedding UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nasapower_api_embedding_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, htmlOutput(ns("frame")))
    )
  )
}

#' nasapower_api_embedding Server Functions
#'
#' @noRd
mod_nasapower_api_embedding_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$frame <- renderUI({
      my_test <- tags$iframe(src=test, height=600, width=535)
      "https://power.larc.nasa.gov/data-access-viewer/"
    })

  })
}

## To be copied in the UI
# mod_nasapower_api_embedding_ui("nasapower_api_embedding_1")

## To be copied in the server
# mod_nasapower_api_embedding_server("nasapower_api_embedding_1")
