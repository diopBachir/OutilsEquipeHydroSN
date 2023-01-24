#' dailyInventoryNAvalueSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dailyInventoryNAvalueSummary_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(6, actionButtonStyled(ns("graphicalStats"), "Statistiques Graphiques", class= "", type="primary")),
             column(6, actionButtonStyled(ns("numericalStats"), "Statistiques Numériques", class= "", type="primary")),
             column(12, sliderInput(ns("variableAxisSize"), label = "Taille des noms de Variables", min = 5, max = 35, step = 1, value = 8))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "left",
             column(6, h4(
               "Graphique",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(6, h4(
               "Numérique",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             ))
    ),

    fluidRow(align = "left",
             column(6, withSpinner(plotOutput(ns("graphicalStatsOut")))),
             column(6, div(dataTableOutput(ns("numericalStatsOut"))), style="font-size:85%")
    )
  )
}

#' dailyInventoryNAvalueSummary Server Functions
#'
#' @noRd
mod_dailyInventoryNAvalueSummary_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # statistiques graphiques
    graphicalSummary <- eventReactive(input$graphicalStats, {
      req(data)

      # Notification
      id <- showNotification(
        "Traitement...",
        duration = 3, closeButton = FALSE
      )
      # remove notification
      on.exit(removeNotification(id), add = TRUE)

      naniar::gg_miss_var(data[,-1], show_pct = T) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = input$variableAxisSize, color = "black"),
          axis.text.x = ggplot2::element_text(size = 12, color = "black"),
          axis.title = ggplot2::element_text(size = 12, color = "black", face = "bold"),
        ) +
        ggplot2::labs(y = "Lacunes (%)")
    })

    output$graphicalStatsOut<- renderPlot({
      req(graphicalSummary())
      graphicalSummary()
    }, height = "auto")

    # Statistiques numériques
    numericalSummury <- eventReactive(input$numericalStats, {
      req(data)

      # Notification
      id <- showNotification(
        "Traitement...",
        duration = 3, closeButton = FALSE
      )
      # remove notification
      on.exit(removeNotification(id), add = TRUE)

      naniar::miss_var_summary(data[,-1])  %>%
        dplyr::mutate(pct_miss = round(pct_miss, 3)) %>%
        dplyr::rename("Total Lacunes" = n_miss, "% Lacunes" = pct_miss)
    })

    # résumé statistiques
    output$numericalStatsOut<- renderDataTable({
      req(numericalSummury())
      numericalSummury()
    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  })
}

## To be copied in the UI
# mod_dailyInventoryNAvalueSummary_ui("dailyInventoryNAvalueSummary_1")

## To be copied in the server
# mod_dailyInventoryNAvalueSummary_server("dailyInventoryNAvalueSummary_1")
