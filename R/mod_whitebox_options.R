#' whitebox_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_whitebox_options_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, numericInput(
        ns("breaching_max_dist"), label=span("Maximum Search Distance For Breach Paths", style="font-size:85%"),
        min = 0, max = 1000, value = 5, step = 1
      )),
      column(12, numericInput(
        ns("bassin_thresold"), label=span("Seuil De Chenalisation [Flow Accumulation", style="font-size:85%"),
        min = 0, max = 1000000, value = 600, step = 5
      )),
      column(12, numericInput(
        ns("pp_snap_dist"), label=span("PourPoints Snap Distance", style="font-size:85%"),
        min = 0, max = 100, value = 50, step = 0.0001
      ))
    )
  )
}

#' whitebox_options Server Functions
#'
#' @noRd
mod_whitebox_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        breaching_max_dist = reactive({ input$breaching_max_dist }),
        bassin_thresold = reactive({ input$bassin_thresold }),
        pp_snap_dist = reactive({ input$pp_snap_dist })
      )
    )
  })
}

## To be copied in the UI
# mod_whitebox_options_ui("whitebox_options_1")

## To be copied in the server
# mod_whitebox_options_server("whitebox_options_1")
