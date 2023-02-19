#' gr2m_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr2m_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("param_choose"))
  )
}

#' gr2m_parameters Server Functions
#'
#' @noRd
mod_gr2m_parameters_server <- function(id, parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$param_choose<- renderUI({
      req(parameters)
      fluidRow(
        column(12, h4("Paramètres Pour La Simulation", style="font-family=georgia;color:blue;")),
        column(6,  numericInput(ns("x1"), div("Paramètre X1", style="font-size:85%;font-family:georgia;"), value = parameters()[1], width="100%")),
        column(6,  numericInput(ns("x2"), div("Paramètre X2", style="font-size:85%;font-family:georgia;"), value =  parameters()[2], width="100%")),

        column(12, tags$hr(style="border-color:gray;"))
      )
    })

    return(
      list(
        parametres_simulation = reactive({ c(input$x1, input$x2, input$x3, input$x4) })
      )
    )
  })
}

## To be copied in the UI
# mod_gr2m_parameters_ui("gr2m_parameters_1")

## To be copied in the server
# mod_gr2m_parameters_server("gr2m_parameters_1")
