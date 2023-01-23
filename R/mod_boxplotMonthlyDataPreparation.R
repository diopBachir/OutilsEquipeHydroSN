#' boxplotMonthlyDataPreparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_boxplotMonthlyDataPreparation_ui <- function(id, label){
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

#' boxplotMonthlyDataPreparation Server Functions
#'
#' @noRd
mod_boxplotMonthlyDataPreparation_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # formattage
    # affichage des données forattées
    dataTidy <- eventReactive(input$clean, {
      req(data)
      data %>%
        tidyr::pivot_longer(
          cols = 2:ncol(data),
          names_to = "variable",
          values_to = "valeur"
        ) %>%
        dplyr::mutate(
          mois = lubridate::month(lubridate::ymd(Date)),
          mois = case_when(
            lubridate::month(Date) == 1 ~ "Janvier", lubridate::month(Date) == 2 ~ "Février",
            lubridate::month(Date) == 3 ~ "Mars", lubridate::month(Date) == 4 ~ "Avril",
            lubridate::month(Date) == 5 ~ "Mai", lubridate::month(Date) == 6 ~ "Juin",
            lubridate::month(Date) == 7 ~ "Juillet", lubridate::month(Date) == 8 ~ "Août",
            lubridate::month(Date) == 9 ~ "Septembre", lubridate::month(Date) == 10 ~ "Octobre",
            lubridate::month(Date) == 11~ "Novembre", TRUE ~ "Décembre",
          ),
          mois = factor(
            mois, levels = c(
              "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
              "Août", "Septembre", "Octobre", "Novembre", "Décembre"
            )
          )
        )  %>%
        dplyr::select(Date, mois, variable, valeur) %>%
        stats::na.omit()

    })


    # données formattées
    output$data_univar_plot<- renderPrint({
      req(dataTidy())
      dataTidy()
    })

    dataTidySummury <- eventReactive(input$summary, {
      req(dataTidy())
      summary(dataTidy() %>% dplyr::select(Date, valeur))
    })

    # résumé statistiques
    output$statsSum<- renderPrint({
      req(dataTidySummury())
      dataTidySummury()
    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    return(list(dataMonthlyBoxplotCleaned = reactive({ dataTidy() })))
  })
}

## To be copied in the UI
# mod_boxplotMonthlyDataPreparation_ui("boxplotMonthlyDataPreparation_1")

## To be copied in the server
# mod_boxplotMonthlyDataPreparation_server("boxplotMonthlyDataPreparation_1")
