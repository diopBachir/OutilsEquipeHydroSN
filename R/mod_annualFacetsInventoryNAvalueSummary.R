#' annualFacetsInventoryNAvalueSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annualFacetsInventoryNAvalueSummary_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12, actionButtonStyled(ns("graphicalStats"), "Résumé Inventaire", class= "", type="primary")),
             column(3, numericInput(ns("variableAxisSize"), label = "Taille Labels-Y", min = 5, max = 35, step = 1, value = 8)),
             column(3, numericInput(ns("yAxisSize"), label = "Taille Labels-Y", min = 5, max = 35, step = 1, value = 12)),
             column(3, numericInput(ns("yAxisTitleSize"), label = "Taille Titre Axe-Y", min = 5, max = 35, step = 1, value = 12)),
             column(3, numericInput(ns("nrowGGarrange"), label = "Nb. lignes ggarrange", min = 1, max = 10, step = 1, value = 1))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(12, h4(
               "Statistiques Graphiques",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(12, withSpinner(plotOutput(ns("graphicalStatsOut"))))
    )
  )
}

#' annualFacetsInventoryNAvalueSummary Server Functions
#'
#' @noRd
mod_annualFacetsInventoryNAvalueSummary_server <- function(id, data){
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

      # Création des graphes avec facets
      plot_list <- purrr::imap(data, ~{
        naniar::gg_miss_var(.x[, -c(1,2)], show_pct = T)  +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
          ggplot2::theme_bw() +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = input$variableAxisSize, color = "black"),
            axis.text.x = ggplot2::element_text(size = input$yAxisSize, color = "black"),
            axis.title = ggplot2::element_text(size = input$yAxisTitleSize, color = "black", face = "bold"),
            panel.grid.major = ggplot2::element_line(linewidth = .3, linetype = "11", color = "gray"),
            panel.grid.minor = ggplot2::element_line(linewidth = .3, linetype = "11", color = "gray")
          ) +
          ggplot2::labs(y = "Lacunes (%)", x = NULL) +
          ggplot2::ggtitle(.x$Facet[1])
      })

      ggpubr::ggarrange(plotlist=plot_list, nrow = input$nrowGGarrange)
    })

    output$graphicalStatsOut<- renderPlot({
      req(graphicalSummary())
      graphicalSummary()
    })


    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  })
}

## To be copied in the UI
# mod_annualFacetsInventoryNAvalueSummary_ui("annualFacetsInventoryNAvalueSummary_1")

## To be copied in the server
# mod_annualFacetsInventoryNAvalueSummary_server("annualFacetsInventoryNAvalueSummary_1")
