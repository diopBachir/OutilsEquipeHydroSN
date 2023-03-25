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
      column(12, uiOutput(ns("model_options"))),
      column(12, tags$hr(style="border-color:gray;")),
      column(12, uiOutput(ns("grid_conf")))
    )
  )
}

#' time_serie_interpolation_options Server Functions
#'
#' @noRd
mod_time_serie_interpolation_options_server <- function(id, ready_data_4_timeSerieInterpolation, bassin){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # affichage de la Grille
    myModal <- function() {
      modalDialog(
        title = "Grille d'Interpolation",
        shinycssloaders::withSpinner(verbatimTextOutput(ns("show_grid_model"))),
        footer = tagList(
          actionButton(ns("fermer"), "Fermer", class = "btn btn-info")
        ),
        size = "l"
      )
    }

    # première période pour la validation croisé
    data_for_interpolation<-reactive({
      shiny::req(ready_data_4_timeSerieInterpolation)
      ready_data_4_timeSerieInterpolation
    })
    # bassin
    bv<-reactive({
      shiny::req(bassin)
      sf::st_transform(bassin, 3857)
    })

    output$model_options<- renderUI({
      req(data_for_interpolation())
      fluidRow(
        column(6,  dateInput(ns("startValidationDate"), div("Date de Début", style="font-size:80%;"), value = min(data_for_interpolation()$Date), width="100%", format = "dd/mm/yyyy")),
        column(6,  dateInput(ns("endValidationDate"), div("Date de Fin", style="font-size:80%;"), value = max(data_for_interpolation()$Date), width="100%", format = "dd/mm/yyyy")),
      )
    })

    # Définition du modèle de la grille d'interpolation spatiale
    grid_model<- reactive({
      req(bv(), input$gridRes)
      grid_def(
        bv(), input$gridRes, 1000, "+init=epsg:3857"
      )
    })

    # grid confi
    output$grid_conf<- renderUI({
      req(data_for_interpolation())
      fluidRow(align = "center",
               column(12,
                      numericInput(
                        ns("gridRes"), label=span("Résolution Grille [mètres]", style = "font-size:90%"), value = 10000, min = 50, max=1000000, step = 10
                      )
               ),
               column(12, dipsaus::actionButtonStyled(ns("showGrid"), label = "Afficher la Grille", icon = icon("th")))
      )
    })

    # Afficher la grille
    # observeEvent(ignoreInit=TRUE, ignoreNULL=TRUE, input$showGrid, {
    #   req(grid_model())
    #   showModal(myModal())
    #   output$show_grid_model<- renderPrint({
    #     ggplot2::ggplot(sf::st_as_sf(grid_model())) +
    #       geom_sf
    #   })
    # })

    return(
      list(
        gridRes = reactive({ input$gridRes }),
        startValidationDate = reactive({ input$startValidationDate }),
        endValidationDate = reactive({ input$endValidationDate }),
        modele_de_grille = reactive({ grid_model() })
      )
    )

  })
}

## To be copied in the UI
# mod_time_serie_interpolation_options_ui("time_serie_interpolation_options_1")

## To be copied in the server
# mod_time_serie_interpolation_options_server("time_serie_interpolation_options_1")
