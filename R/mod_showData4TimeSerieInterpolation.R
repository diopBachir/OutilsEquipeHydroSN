#' showData4TimeSerieInterpolation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_showData4TimeSerieInterpolation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="center",

             column(4,
                    actionButton(
                      ns("showStudyArea"), label="Afficher La Zone D'étude",
                      icon = icon("map")
                    )
             ),

             column(4,
                    actionButton(
                      ns("showDataset"), label="Voir Les Données Chargées",
                      icon = icon("table")
                    )
             ),
             column(4,
                    actionButton(
                      ns("showDatasetSummary"), label="Résumé Statistique",
                      icon = icon("table")
                    )
             )
    ),

    tags$hr(style="border-color:gray;"),

    dataTableOutput(ns("used_time_serie_interpolation_data"))

  )
}

#' showData4TimeSerieInterpolation Server Functions
#'
#' @noRd
mod_showData4TimeSerieInterpolation_server <- function(id, interpolation_data, bassin){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    myModal <- function() {
      modalDialog(
        title = "Zone D'étude",
        plotOutput(ns("plotStudyArea")),
        footer = tagList(
          actionButton(ns("fermer"), "Fermer", class = "btn btn-info")
        )
      )
    }

    # getting data
    interpolation_data_fn <- reactive({
      req(interpolation_data)
      dplyr::mutate(
        interpolation_data[-c(1:4),],
        dplyr::across(-1, as.numeric, .names = "{.col}")
      ) %>%
        dplyr::rename(Date = 1)
    })

    ## Summarising data
    stats_summary<-reactive({
      req(interpolation_data_fn())
      interpolation_data_fn() %>%
        dplyr::select(-Date) %>%
        tidyr::pivot_longer(
          1:ncol(interpolation_data_fn())-1, names_to = "Station", values_to = "Valeur"
        )  %>%
        dplyr::group_by(Station) %>%
        dplyr::summarise(
          Min. = min(Valeur, na.rm = T), Quart1 = quantile(Valeur, .25),
          Médianne = median(Valeur), Quart3 = quantile(Valeur, .75),
          Moyenne = mean(Valeur), Max = max(Valeur),
          "Ecart type" = sd(Valeur)
        )  %>%
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
        )
    })

    ## mean period data
    meanPeriod<- reactive({
      req(interpolation_data_fn())

      interpolation_data_fn() %>%
        dplyr::select(-Date) %>%
        tidyr::pivot_longer(
          1:ncol(interpolation_data_fn())-1, names_to = "Station", values_to = "Valeur"
        ) %>%
        dplyr::group_by(Station) %>%
        dplyr::summarise(
          Moyenne = mean(Valeur)
        ) %>%
        tidyr::pivot_wider(names_from = Station, values_from = Moyenne)
    })

    # show all time serie dataset
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showStudyArea, {
      req(interpolation_data, bassin)
      stations <- interpolation_data[c(1,2),] %>%
        dplyr::rename(Station = 1) %>%
        transpose_df() %>%
        dplyr::mutate(across(-1, as.numeric, .names = "{.col}"))

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      showModal(myModal())

      # affichage de la zone d'étude
      output$plotStudyArea <- renderPlot({
        ggplot2::ggplot(bassin) +
          ggplot2::geom_sf(linewidth = 2, color = "black", fill="lightgray") +
          ggplot2::geom_point(
            data = stations,
            ggplot2::aes(x=Longitude, y=Latitude), shape = 15, color = "blue", size = 3
          ) +
          ggthemes::theme_map()
      })

      observeEvent(input$fermer, {
        removeModal()
      })
    })

    # show all time serie dataset
    event_trigger <- reactive({list(input$showDataset, interpolation_data)}) # events trigger
    observeEvent(event_trigger(), {
      req(interpolation_data)

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_time_serie_interpolation_data <- renderDataTable({
        req(interpolation_data)
        interpolation_data
      })
    })

    # show datset summary
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$showDatasetSummary, {
      req(stats_summary())

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_time_serie_interpolation_data <- renderDataTable({
        req(stats_summary())
        stats_summary()
      })
    })

    #return
    return(
      list(
        data_for_time_serie_interpolation = reactive({ interpolation_data_fn() }),
        stations_for_time_serie_interpolation = reactive({
          req(interpolation_data)
          interpolation_data[c(1,2),] %>%
            dplyr::rename(Station = 1) %>%
            transpose_df() %>%
            dplyr::mutate(across(-1, as.numeric, .names = "{.col}"))
        }),
        mean_periode_data = reactive({ meanPeriod() })
      )
    )

  })
}

## To be copied in the UI
# mod_showData4TimeSerieInterpolation_ui("showData4TimeSerieInterpolation_1")

## To be copied in the server
# mod_showData4TimeSerieInterpolation_server("showData4TimeSerieInterpolation_1")
