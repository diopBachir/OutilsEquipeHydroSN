#' gr5J_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr5J_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("param_choose"))
  )
}

#' gr5J_parameters Server Functions
#'
#' @noRd
mod_gr5J_parameters_server <- function(id, parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$param_choose<- renderUI({
      req(parameters)
      fluidRow(
        column(12, h4("Paramètres Pour La Simulation", style="font-family=georgia;color:blue;")),
        column(4,  numericInput(ns("x1"), div("Param X1", style="font-size:85%;font-family:georgia;"), value = parameters()[1], width="100%")),
        column(4,  numericInput(ns("x2"), div("Param X2", style="font-size:85%;font-family:georgia;"), value =  parameters()[2], width="100%")),
        column(4,  numericInput(ns("x3"), div("Param X3", style="font-size:85%;font-family:georgia;"), value =  parameters()[3], width="100%")),
        column(6,  numericInput(ns("x4"), div("Param X4", style="font-size:85%;font-family:georgia;"), value =  parameters()[4], width="100%")),
        column(6,  numericInput(ns("x5"), div("Param X5", style="font-size:85%;font-family:georgia;"), value =  parameters()[5], width="100%")),

        column(12, tags$hr(style="border-color:gray;"))
      )
    })

    return(
      list(
        parametres_simulation = reactive({ c(input$x1, input$x2, input$x3, input$x4, input$x5) })
      )
    )
  })
}

## To be copied in the UI
# mod_gr5J_parameters_ui("gr5J_parameters_1")

## To be copied in the server
# mod_gr5J_parameters_server("gr5J_parameters_1")
