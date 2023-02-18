#' ShowTemporalInterpolationResults UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ShowTemporalInterpolationResults_ui <- function(id){
  ns <- NS(id)
  tagList(

    h3( "Résultats | Visualisation", style = "color:#3474A7;text-align:center"),

    fluidRow(align = "center",
             # krigeage
             column(
               3, actionButton(ns("showKrigeResult"), label="Krigeage", icon = icon("poll"), class = "btn btn-success")
             ),
             # krigeage
             column(
               3, actionButton(ns("showIdwResult"), label="IDW", icon = icon("poll"), class = "btn btn-success")
             ),
             # krigeage
             column(
               3, actionButton(ns("showThiessenResult"), label="Thiessen", icon = icon("poll"), class = "btn btn-success")
             ),
             # krigeage
             column(
               3, actionButton(ns("showSplineResult"), label="Spline", icon = icon("poll"), class = "btn btn-success")
             )
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(3, ""),
             column(3,
                    selectInput(ns("frequency"), label = "Fréquence",
                                choices = c("Jour" = "days", "Semaine" = "weeks", "Mois" = "months", "Année" = "years"), selected = "years")
             ),
             column(3,
                    selectInput(ns("nbStep"), label = "Nombre de Pas",  choices = 1:52, selected = 4)
             ),
             column(3, "")
    ),

    # Results Plots

    #* Trend
    fluidRow(align = "center",
             column(12,  plotOutput(ns("meanMapInterpolationResult_TREND")))
    )
  )
}

#' ShowTemporalInterpolationResults Server Functions
#'
#' @noRd
mod_ShowTemporalInterpolationResults_server <- function(
    id, krigeTimeSerieResult, idwTimeSerieResult, splineTimeSerieResult, thiessenTimeSerieResult
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #--------------------------------------------------------------------------#
    #* Krigeage
    #* gridded.points.utm, grid.mod, bassin, epsg
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showKrigeResult, {
      req(krigeTimeSerieResult)

      # Notification
      id <- showNotification(
        "Traitement en cours ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # TREND
      krige_result.trend<-  reactive({
        future::value(krigeTimeSerieResult) %>%
          dplyr::mutate(Date = lubridate::ymd(Date))  %>%
          ggplot2::ggplot()+
          ggplot2::geom_line(
            ggplot2::aes(x=Date, y=KRIGEAGE, group=1), color="dodgerblue", size = .6
          ) +
          ggplot2::geom_point(ggplot2::aes(x=Date, y=KRIGEAGE), size=1.5, color="dodgerblue")+
          ggplot2::geom_smooth(ggplot2::aes(x=Date, y=KRIGEAGE), method = "lm", color="dodgerblue") +
          ggplot2::scale_x_date(date_breaks = paste0(input$nbStep, " ", input$frequency)) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle=90, size = 12, color="black", vjust = .5),
            axis.text.y = ggplot2::element_text(size = 12,  color="black"),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.text = ggplot2::element_text(size = 12)
          ) +
          ggplot2::labs(
            title = "Evolution de la Variable Interpolée sur la Période d'Etude",
            subtitle ="Méthode d'interpolation : Krigeage",
            x = NULL, y = "Valeur Moyenne", color = "Variable"
          )
      })

      # Affichage des résultats
      output$meanMapInterpolationResult_TREND <- renderPlot({
        req(krige_result.trend())
        krige_result.trend()
      })
    })

    #* IDW
    #* gridded.points.utm, grid.mod, bassin, epsg
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showIdwResult, {
      req(idwTimeSerieResult)

      # Notification
      id <- showNotification(
        "Traitement en cours ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # TREND
      idw_result.trend<-  reactive({
        future::value(idwTimeSerieResult) %>%
          dplyr::mutate(Date = lubridate::ymd(Date))  %>%
          ggplot2::ggplot()+
          ggplot2::geom_line(
            ggplot2::aes(x=Date, y=IDW, group=1), color="dodgerblue", size = .6
          ) +
          ggplot2::geom_point(ggplot2::aes(x=Date, y=IDW), size=1.5, color="dodgerblue")+
          ggplot2::geom_smooth(ggplot2::aes(x=Date, y=IDW), method = "lm", color="dodgerblue") +
          ggplot2::scale_x_date(date_breaks = paste0(input$nbStep, " ", input$frequency)) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle=90, size = 12, color="black", vjust = .5),
            axis.text.y = ggplot2::element_text(size = 12,  color="black"),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.text = ggplot2::element_text(size = 12)
          ) +
          ggplot2::labs(
            title = "Evolution de la Variable Interpolée sur la Période d'Etude",
            subtitle ="Méthode d'interpolation : IDW",
            x = NULL, y = "Valeur Moyenne", color = "Variable"
          )
      })

      # Affichage des résultats
      output$meanMapInterpolationResult_TREND <- renderPlot({
        req(idw_result.trend())
        idw_result.trend()
      })

    })

    #* THIESSEN
    #* gridded.points.utm, grid.mod, bassin, epsg
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showThiessenResult, {
      req(thiessenTimeSerieResult)

      # Notification
      id <- showNotification(
        "Traitement en cours ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # TREND
      thiessen_result.trend<-  reactive({
        future::value(thiessenTimeSerieResult) %>%
          dplyr::mutate(Date = lubridate::ymd(Date))  %>%
          ggplot2::ggplot()+
          ggplot2::geom_line(
            ggplot2::aes(x=Date, y=THIESSEN, group=1), color="dodgerblue", size = .6
          ) +
          ggplot2::geom_point(ggplot2::aes(x=Date, y=THIESSEN), size=1.5, color="dodgerblue")+
          ggplot2::geom_smooth(ggplot2::aes(x=Date, y=THIESSEN), method = "lm", color="dodgerblue") +
          ggplot2::scale_x_date(date_breaks = paste0(input$nbStep, " ", input$frequency)) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle=90, size = 12, color="black", vjust = .5),
            axis.text.y = ggplot2::element_text(size = 12,  color="black"),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.text = ggplot2::element_text(size = 12)
          ) +
          ggplot2::labs(
            title = "Evolution de la Variable Interpolée sur la Période d'Etude",
            subtitle ="Méthode d'interpolation : Polygones de Thiessen",
            x = NULL, y = "Valeur Moyenne", color = "Variable"
          )
      })

      # Affichage des résultats
      output$meanMapInterpolationResult_TREND <- renderPlot({
        req(thiessen_result.trend())
        thiessen_result.trend()
      })

    })

    #* SPLINE
    #* gridded.points.utm, grid.mod, bassin, epsg
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showSplineResult, {
      req(splineTimeSerieResult)

      # Notification
      id <- showNotification(
        "Traitement en cours ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # TREND
      spline_result.trend<-  reactive({
        future::value(splineTimeSerieResult) %>%
          dplyr::mutate(Date = lubridate::ymd(Date))  %>%
          ggplot2::ggplot()+
          ggplot2::geom_line(
            ggplot2::aes(x=Date, y=TPS, group=1), color="dodgerblue", size = .6
          ) +
          ggplot2::geom_point(ggplot2::aes(x=Date, y=TPS), size=1.5, color="dodgerblue")+
          ggplot2::geom_smooth(ggplot2::aes(x=Date, y=TPS), method = "lm", color="dodgerblue") +
          ggplot2::scale_x_date(date_breaks = paste0(input$nbStep, " ", input$frequency)) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle=90, size = 12, color="black", vjust = .5),
            axis.text.y = ggplot2::element_text(size = 12,  color="black"),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.text = ggplot2::element_text(size = 12)
          ) +
          ggplot2::labs(
            title = "Evolution de la Variable Interpolée sur la Période d'Etude",
            subtitle ="Méthode d'interpolation : Thin-Plate Spline",
            x = NULL, y = "Valeur Moyenne", color = "Variable"
          )
      })

      # Affichage des résultats
      output$meanMapInterpolationResult_TREND <- renderPlot({
        req(spline_result.trend())
        spline_result.trend()
      })

    })

  })
}

## To be copied in the UI
# mod_ShowTemporalInterpolationResults_ui("ShowTemporalInterpolationResults_1")

## To be copied in the server
# mod_ShowTemporalInterpolationResults_server("ShowTemporalInterpolationResults_1")
