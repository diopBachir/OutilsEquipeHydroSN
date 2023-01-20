#' data4UnivariateBoxplotFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4UnivariateBoxplotFile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="center",
             column(6,
                    fileInput(
                      ns("dataInput"), label = "",
                      # label = div("Données", style = "color:#gray;family:Georgia;font-size:120%"),
                      accept = c(".csv", ".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "fichier CSV ou Excel"
                    )
             ),
             column(3,
                    div("Serie", style = "color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("showDataset"), label="",
                      icon = icon("table")
                    )
             ),
             column(3,
                    div("Résumé Stat.", style = "color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("showDatasetSummary"), label="",
                      icon = icon("table")
                    )
             )
    ),

    dataTableOutput(ns("used_data"))
  )
}

#' data4UnivariateBoxplotFile Server Functions
#'
#' @noRd
mod_data4UnivariateBoxplotFile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    userFile <- reactive({
      # If no file is selected, don't do anything
      shiny::req(input$dataInput)
      input$dataInput
    })

    ##### vérification de l'extention du fichier
    extension_fichier<- reactive({
      shiny::req(userFile())
      tools::file_ext(userFile()$datapath) %in% c("csv", "xlsx", "xls")
    })

    # loadingData
    data_for_univariate_boxplots<- reactive({

      if(!extension_fichier()){
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Veillez choisir un fichier {.csv|.xlsx|xls} !"
          )
        )
        # shinyalert
        if(!extension_fichier()){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez choisir un fichier avec l'extension {.csv}, {.xlsx} ou {xlsx}."
            )
          )
        }
      }

      # load data
      data_loaded<-reactive({
        shiny::req(extension_fichier())
        import_data(userFile()$datapath) %>%
          dplyr::mutate(
            dplyr::across(-1, as.numeric, .names = "{.col}"),
            dplyr::across(tidyselect::where(is.numeric), ~tidyr::replace_na(., median(., na.rm=TRUE))),
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )%>%
          purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=50)
      })

      data_type_columns<- sum(sapply(data_loaded()[-1], is.numeric))==ncol(data_loaded())-1
      # alert
      if(!data_type_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !data_type_columns,
          paste0(
            "Toutes les colonnes ne sont pas numériques !"
          )
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Toutes les colonnes {à partir de la deuxième colonne} ne sont pas numériques. ",
            "Veillez convertir ces colonnes en nombres avant de continuer !"
          )
        )
      }

      # return
      if(data_type_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackSuccess(
          "dataInput",
          paste0(
            "Ficher Chargé Avec Succès ", shiny::icon("check")
          )
        )
        return(data_loaded()[,-1])
      }else{
        return(NULL)
      }

    })

    ## Summarising data
    stats_summary<-reactive({
      shiny::req(data_for_univariate_boxplots())
      data_for_univariate_boxplots() %>%
        dplyr::select(-1) %>%
        tidyr::pivot_longer(
          1:ncol(data_for_univariate_boxplots())-1, names_to = "Station", values_to = "Valeur"
        )  %>%
        dplyr::group_by(Station) %>%
        dplyr::summarise(
          Min. = min(Valeur, na.rm = T), Quart1 = quantile(Valeur, .25, na.rm = T),
          Médianne = median(Valeur, na.rm = T), Quart3 = quantile(Valeur, .75, na.rm = T),
          Moyenne = mean(Valeur, na.rm = T), Max = max(Valeur, na.rm = T),
          "Ecart type" = sd(Valeur, na.rm = T)
        ) %>%
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
        )
    })

    # show all time serie dataset
    output$used_data <- renderDataTable({
      shiny::req(data_for_univariate_boxplots())
      data_for_univariate_boxplots()
    })

    observeEvent(input$showDataset, {
      shiny::req(data_for_univariate_boxplots())

      # shinyFeedback::hideFeedback("dataInput")

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        shiny::req(data_for_univariate_boxplots())
        data_for_univariate_boxplots()
      })
    })
    # show datset summary
    observeEvent(input$showDatasetSummary, {
      shiny::req(stats_summary())

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        shiny::req(stats_summary())
        stats_summary()
      })
    })

    #return
    return(
      list(
        data_for_univariateBoxplot = reactive({data_for_univariate_boxplots()}),
        stats_summary = reactive({stats_summary()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4UnivariateBoxplotFile_ui("data4UnivariateBoxplotFile_1")

## To be copied in the server
# mod_data4UnivariateBoxplotFile_server("data4UnivariateBoxplotFile_1")
