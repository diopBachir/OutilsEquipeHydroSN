#' gr1a_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr1a_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("param_choose"))
  )
}

#' gr1a_parameters Server Functions
#'
#' @noRd
mod_gr1a_parameters_server <- function(id, parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$param_choose<- renderUI({
      req(parameters)
      fluidRow(
        column(12, h4("Paramètre Pour La Simulation", style="font-family=georgia;color:blue;")),
        column(12,  numericInput(ns("x1"), div("Paramètre X1", style="font-size:85%;font-family:georgia;"), value = parameters()[1], width="100%")),

        column(12, tags$hr(style="border-color:gray;"))
      )
    })

    return(
      list(
        parametres_simulation = reactive({ c(input$x1) })
      )
    )
  })
}

## To be copied in the UI
# mod_gr1a_parameters_ui("gr1a_parameters_1")

## To be copied in the server
# mod_gr1a_parameters_server("gr1a_parameters_1")
