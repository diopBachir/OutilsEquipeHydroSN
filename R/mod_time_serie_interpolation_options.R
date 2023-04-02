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
        shinycssloaders::withSpinner(plotOutput(ns("show_grid_model"))),
        footer = tagList(
          actionButton(ns("fermer"), "Fermer", class = "btn btn-info")
        ),
        size = "l", easyClose = TRUE
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
        column(6,  dateInput(ns("startDate"), div("Date de Début", style="font-size:80%;"), value = min(data_for_interpolation()$Date), width="100%", format = "dd/mm/yyyy")),
        column(6,  dateInput(ns("endDate"), div("Date de Fin", style="font-size:80%;"), value = max(data_for_interpolation()$Date), width="100%", format = "dd/mm/yyyy")),
      )
    })

    # grid confi
    output$grid_conf<- renderUI({
      req(data_for_interpolation())
      fluidRow(align = "center",
               column(12,
                      numericInput(
                        ns("gridRes"), label=span("Résolution Grille [mètres]", style = "font-size:90%"), value = 100000, min = 50, max=, step = 10
                      )
               ),
               column(12, dipsaus::actionButtonStyled(ns("showGrid"), label = "Afficher la Grille", icon = icon("th")))
      )
    })

    # Définition du modèle de la grille d'interpolation spatiale
    grid_model<- reactive({
      if(input$gridRes>10000){
        shinyFeedback::feedbackWarning(
          "gridRes", input$gridRes>10000000, "Valeur Max.(1e+05) Dépassée !"
        )
      }
      req(bv(), input$gridRes)
      tryCatch({
        grid_def(
          bv(), input$gridRes, 1000, "+init=epsg:3857"
        )},
        error = function(e) {
          shinyalert::shinyalert("Résolution Grille Trop Grossière", e$message, type = "error")
          return()
        }
      )
    })

    # Afficher la grille
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showGrid, {
      req(grid_model())
      showModal(myModal())
      output$show_grid_model<- renderPlot({
        req(grid_model())
        # ggplot2::ggplot(sf::st_as_sf(grid_model())) +
        #   geom_sf(fill=NA, color="black")
        # list(grid_model(), as(grid_model(),"sf"))
        plot(grid_model(), col = "black", lwd=2.5, main = "Grille d'Interpolation")
        plot(bv(), border="red", col=rgb(1,1,1,.05), lwd=4.5, add=TRUE)
      })
    })

    observeEvent(input$fermer, {
      shiny::removeModal()
    })

    # start date
    start_computing_date<- reactive({
      req(input$startDate, input$endDate, !anyNA(c(input$startDate, input$endDate)))
      # contrôle des périodes
      if(input$startDate >= input$endDate){
        shinyFeedback::feedbackWarning(
          "startDate", input$startDate>=input$endDate, "Date incorrecte !"
        )
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début [", input$startDate, "]",
            " soit strictement inférieure à la date de fin de période [", input$endDate, "] !"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      if(input$startDate < input$endDate){
        return(input$startDate)
      }else{
        return(NULL)
      }
    })

    # end date
    end_computing_date<- reactive({
      req(input$startDate, input$endDate, !anyNA(c(input$startDate, input$endDate)))
      # contrôle des périodes
      if(input$startDate >= input$endDate){
        shinyFeedback::feedbackWarning(
          "endDate", input$startDate>=input$endDate, "Date incorrecte !"
        )
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début [", input$startDate, "]",
            " soit strictement inférieure à la date de fin de période [", input$endDate, "] !"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      if(input$startDate < input$endDate){
        return(input$endDate)
      }else{
        return(NULL)
      }
    })


    return(
      list(
        gridRes = reactive({ input$gridRes }),
        startDate = reactive({ start_computing_date() }),
        endDate = reactive({ end_computing_date() }),
        modele_de_grille = reactive({ grid_model() })
      )
    )

  })
}

## To be copied in the UI
# mod_time_serie_interpolation_options_ui("time_serie_interpolation_options_1")

## To be copied in the server
# mod_time_serie_interpolation_options_server("time_serie_interpolation_options_1")
