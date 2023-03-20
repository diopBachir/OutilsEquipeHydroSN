#' hbv_parameters() UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters() for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hbv_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("param_choose"))
  )
}

#' hbv_parameters() Server Functions
#'
#' @noRd
mod_hbv_parameters_server <- function(id, parameters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$param_choose<- renderUI({
      req(parameters())
      fluidRow(
        column(12, h4("Paramètres Optimisés Du Modèle", style="font-family=georgia;color:blue;")),
        column(6,  numericInput(ns("FC"), div("Param. FC", style="font-size:85%;font-family:georgia;"), value = parameters()[1], width="100%")),
        column(6,  numericInput(ns("beta"), div("Param. beta", style="font-size:85%;font-family:georgia;"), value =  parameters()[2], width="100%")),
        column(6,  numericInput(ns("LP"), div("Param. LP", style="font-size:85%;font-family:georgia;"), value =  parameters()[3], width="100%")),
        column(6,  numericInput(ns("SFCF"), div("Param. SFCF", style="font-size:85%;font-family:georgia;"), value =  parameters()[4], width="100%")),
        column(6,  numericInput(ns("TT"), div("Param. TT", style="font-size:85%;font-family:georgia;"), value =  parameters()[5], width="100%")),
        column(6,  numericInput(ns("CFCMAX"), div("Param. beta", style="font-size:85%;font-family:georgia;"), value =  parameters()[6], width="100%")),
        column(6,  numericInput(ns("k0"), div("Param. k0", style="font-size:85%;font-family:georgia;"), value =  parameters()[7], width="100%")),
        column(6,  numericInput(ns("k1"), div("Param. k1", style="font-size:85%;font-family:georgia;"), value =  parameters()[8], width="100%")),
        column(6,  numericInput(ns("k2"), div("Param. k3", style="font-size:85%;font-family:georgia;"), value =  parameters()[9], width="100%")),
        column(6,  numericInput(ns("UZL"), div("Param. UZL", style="font-size:85%;font-family:georgia;"), value =  parameters()[10], width="100%")),
        column(6,  numericInput(ns("PERC"), div("Param. PERC", style="font-size:85%;font-family:georgia;"), value =  parameters()[11], width="100%")),
        column(6,  numericInput(ns("MAXBAS"), div("Param. MAXBAS", style="font-size:85%;font-family:georgia;"), value =  parameters()[12], width="100%")),
        column(12, tags$hr(style="border-color:gray;"))
      )
    })

    return(
      list(
        parametres_simulation = reactive({
          c(input$FC, input$beta, input$LP, input$SFCF, input$TT, input$CFCMAX, input$k0, input$k1, input$k2, input$UZL, input$PERC, input$MAXBAS)
        })
      )
    )
  })
}

## To be copied in the UI
# mod_hbv_parameters()_ui("hbv_parameters()_1")

## To be copied in the server
# mod_hbv_parameters()_server("hbv_parameters()_1")
