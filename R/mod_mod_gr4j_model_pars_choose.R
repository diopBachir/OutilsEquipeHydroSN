#' mod_gr4j_model_pars_choose_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_gr4j_model_pars_choose_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("param_choose"))
  )
}

#' mod_gr4j_model_pars_choose_ui Server Functions
#'
#' @noRd
mod_mod_gr4j_model_pars_choose_server <- function(id, parameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$param_choose<- renderUI({
      req(parameters)
      fluidRow(
        column(12, h4("Paramètres Pour La Simulation", style="font-family=georgia;color:blue;")),
        column(6,  numericInput(ns("x1"), div("Paramètre X1", style="font-size:85%;font-family:georgia;"), value = parameters()[1], width="100%")),
        column(6,  numericInput(ns("x2"), div("Paramètre X2", style="font-size:85%;font-family:georgia;"), value =  parameters()[2], width="100%")),
        column(6,  numericInput(ns("x3"), div("Paramètre X3", style="font-size:85%;font-family:georgia;"), value =  parameters()[3], width="100%")),
        column(6,  numericInput(ns("x4"), div("Paramètre X4", style="font-size:85%;font-family:georgia;"), value =  parameters()[4], width="100%")),

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
# mod_mod_gr4j_model_pars_choose_ui("mod_gr4j_model_pars_choose_1")

## To be copied in the server
# mod_mod_gr4j_model_pars_choose_server("mod_gr4j_model_pars_choose_1")
