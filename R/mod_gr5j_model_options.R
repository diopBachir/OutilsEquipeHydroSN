#' gr5j_model_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr5j_model_options_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      uiOutput(ns("model_options"))
    )
  )
}

#' gr5j_model_options Server Functions
#'
#' @noRd
mod_gr5j_model_options_server <- function(id, ready_data_4_gr5j_application){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # première période pour la validation croisé
    dataObs1<-reactive({
      shiny::req(ready_data_4_gr5j_application)
      ready_data_4_gr5j_application %>%
        slice(1:round(nrow(ready_data_4_gr5j_application)/2))
    })
    # seconde période pour la validation croisé
    dataObs2<- reactive({
      req(ready_data_4_gr5j_application)
      ready_data_4_gr5j_application %>%
        slice(round(nrow(ready_data_4_gr5j_application)/2+.5):nrow(ready_data_4_gr5j_application))
    })

    output$model_options<- renderUI({
      req(ready_data_4_gr5j_application)
      fluidRow(
        column(12, h4("Options Du Calage/Validation Croisé", style="font-family=georgia;color:blue;")),
        column(6,  dateInput(ns("startValidationDate1"), div("Début Première Période", style="font-size:80%;"), value = min(dataObs1()$date), width="100%", format = "dd/mm/yyyy")),
        column(6,  dateInput(ns("endValidationDate1"), div("Fin Première Période", style="font-size:80%;"), value = max(dataObs1()$date), width="100%", format = "dd/mm/yyyy")),
        column(6,  dateInput(ns("startValidationDate2"), div("Début Seconde Période", style="font-size:80%;"), value = min(dataObs2()$date), width="100%", format = "dd/mm/yyyy")),
        column(6,  dateInput(ns("endValidationDate2"), div("Fin Seconde Période", style="font-size:80%;"), value = max(dataObs2()$date), width="100%", format = "dd/mm/yyyy")),

        column(12, tags$hr(style="border-color:gray;")),

        column(12, h4("Options || Configuration", style="font-family=georgia;color:blue;")),
        column(12,  numericInput(ns("nbWarmUpYear"), div("Période D'échauffement [Nombre de jours]", style="font-size:85%;"),
                                 min=10, max = round(nrow(ready_data_4_gr5j_application)*50/100), value = 730,
                                step = 1, width="100%")),
        column(12,  selectInput(
          ns("calibrationType"), div("Type De Calibration", style="font-size:85%;"),
          choices = c("Fonction Objective Unique [KGE[Q]]", "Critère Composite [KGE[Q], KGE[sqrt(Q)]]"), selected = "Fonction Objective Unique [KGE[Q]]", width="100%"
        )),
        column(12, tags$hr(style="border-color:gray;"))
      )
    })

    return(
      list(
        startValidationDate1 = reactive({ input$startValidationDate1 }), endValidationDate1 = reactive({ input$endValidationDate1 }),
        startValidationDate2 = reactive({ input$startValidationDate2 }), endValidationDate2 = reactive({ input$endValidationDate2 }),
        nbWarmUpYear = reactive({ input$nbWarmUpYear }), calibrationType = reactive({ input$calibrationType })
      )
    )

  })
}

## To be copied in the UI
# mod_gr5j_model_options_ui("gr5j_model_options_1")

## To be copied in the server
# mod_gr5j_model_options_server("gr5j_model_options_1")
