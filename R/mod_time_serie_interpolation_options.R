#' time_serie_interpolation_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_time_serie_interpolation_options_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # Grille d'interpolation
      column(12,
             numericInput(
               ns("gridRes"), label=span("Résolution Grille [mètres]", style = "font-size:90%"), value = 7000, min = 500, max=100000, step = 500
             )
      ),
      column(12,
             selectInput(
               ns("includeNA"), label=span("Inclure NA/NaN dans la Sortie", style = "font-size:90%"),
               choices = c("Oui", "Non"), selected = "Non"
             )
      ),
    )
  )
}

#' time_serie_interpolation_options Server Functions
#'
#' @noRd
mod_time_serie_interpolation_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        gridRes = reactive({ input$gridRes }), includeNA = reactive({ input$includeNA })
      )
    )

  })
}

## To be copied in the UI
# mod_time_serie_interpolation_options_ui("time_serie_interpolation_options_1")

## To be copied in the server
# mod_time_serie_interpolation_options_server("time_serie_interpolation_options_1")
