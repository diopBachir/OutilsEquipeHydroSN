#' boxplotUnivariateDataPreparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_boxplotUnivariateDataPreparation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(6, actionButtonStyled(ns("clean"), "Nettoyer", class= "", type="primary")),
             column(6, actionButtonStyled(ns("summary"), "Résumé Stat.", class= "", type="primary"))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "left",
             column(6, h4(
               "Données Nettoyées",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               ),
               div(verbatimTextOutput(ns("data_univar_plot")), style="font-size:85%;"), width = "100%"
             )),
             column(6, h4(
               "Résumé Statique",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               ),
               div(verbatimTextOutput(ns("statsSum")), style="font-size:85%;"), width = "100%"
             ))
    )
  )
}

#' boxplotUnivariateDataPreparation Server Functions
#'
#' @noRd
mod_boxplotUnivariateDataPreparation_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # formattage
    # affichage des données formattées
    dataTidy <- eventReactive(input$clean, {
      shiny::req(data)
      data %>%
        tidyr::pivot_longer(
          cols = 1:ncol(data),
          names_to = "variable",
          values_to = "valeur"
        ) %>%
        dplyr::select(variable, valeur) %>%
        stats::na.omit()
    })


    # données formattées
    output$data_univar_plot<- renderPrint({
      req(dataTidy())
      dataTidy()
    })

    dataTidySummury <- eventReactive(input$summary, {
      shiny::req(dataTidy())
      summary(data)
    })

    # résumé statistiques
    output$statsSum<- renderPrint({
      shiny::req(dataTidySummury())
      dataTidySummury()
    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    return(list(dataUnivariateBoxplotCleaned = reactive({ dataTidy() })))
  })
}

## To be copied in the UI
# mod_boxplotUnivariateDataPreparation_ui("boxplotUnivariateDataPreparation_1")

## To be copied in the server
# mod_boxplotUnivariateDataPreparation_server("boxplotUnivariateDataPreparation_1")
