#' data4MonthlyBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4MonthlyBoxplot_ui <- function(id){
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

    tags$hr(style="border-color:gray;"),

    dataTableOutput(ns("used_data"))
  )
}

#' data4MonthlyBoxplot Server Functions
#'
#' @noRd
mod_data4MonthlyBoxplot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    userFile <- reactive({
      # If no file is selected, don't do anything
      req(input$dataInput)
      input$dataInput
    })

    ##### vérification de l'extention du fichier
    extension_fichier<- reactive({
      req(userFile())
      tools::file_ext(userFile()$datapath) %in% c("csv", "xlsx", "xls")
    })

    # loadingData
    data_for_monthly_boxplots<- reactive({

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
        req(extension_fichier())
        import_data(userFile()$datapath) %>%
          dplyr::mutate(
            dplyr::across(-1, as.numeric, .names = "{.col}"),
            dplyr::across(tidyselect::where(is.numeric), ~tidyr::replace_na(., median(., na.rm=TRUE))),
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )%>%
          purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=50)
      })

      # validation du nom de la première colonne
      colonne_date<- names(data_loaded())[1] == "Date"
      shinyFeedback::feedbackWarning(
        "dataInput", !colonne_date,
        paste0(
          "La première colonne est-elle nommée {Date} ?"
        )
      )
      # alert
      if(!colonne_date){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "La première colonne du fichier doit obligatoirement se nommer {Date}"
          )
        )
      }

      # validation du format de la colonne {Date}
      req(colonne_date)
      format_date<- sum(grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", data_loaded()$Date)) == nrow(data_loaded()) |
        sum(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", data_loaded()$Date)) == nrow(data_loaded())
      shinyFeedback::feedbackWarning(
        "dataInput", !format_date,
        paste0(
          "Vérifiez le format de la {Date} !"
        )
      )

      # alert
      if(!format_date){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
            "doit être de la forme { [YYYY/MM/DD] } !"
          )
        )
      }

      # validation du numéro du mois
      req(format_date)
      test_mois<- sum(as.numeric(stringr::str_sub(data_loaded()$Date, 6, 7)) <=12) == nrow(data_loaded())
      shinyFeedback::feedbackWarning(
        "dataInput", !test_mois,
        paste0(
          "Le numéro du mois ne peut être supérieur à 12 !"
        )
      )
      # alert
      if(!test_mois){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
            "doit être de la forme : YYYY/MM/DD. Avez-vous inverser la position du mois ou de la date ? ",
            "Le numéro du mois ne peut être supérieur à 12 !"
          )
        )
      }


      # Validation du type des colonnes
      req(test_mois)
      data_type_columns<- sum(sapply(data_loaded()[-1], is.numeric))==ncol(data_loaded())-1
      shinyFeedback::feedbackWarning(
        "dataInput", !data_type_columns,
        paste0(
          "Toutes les colonnes ne sont pas numériques !"
        )
      )
      # alert
      if(!data_type_columns){
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
        return(
          data_loaded()
        )
      }else{
        return(NULL)
      }

    })

    ## Summarising data
    stats_summary<-reactive({
      req(data_for_monthly_boxplots())
      data_for_monthly_boxplots() %>%
        dplyr::select(-Date) %>%
        tidyr::pivot_longer(
          1:ncol(data_for_monthly_boxplots())-1, names_to = "Station", values_to = "Valeur"
        )  %>%
        dplyr::group_by(Station) %>%
        dplyr::summarise(
          Min. = min(Valeur, na.rm = T), Quart1 = quantile(Valeur, .25, na.rm = T),
          Médianne = median(Valeur, na.rm = T), Quart3 = quantile(Valeur, .75, na.rm = T),
          Moyenne = mean(Valeur, na.rm = T), Max = max(Valeur, na.rm = T),
          "Ecart type" = sd(Valeur, na.rm = T)
        )  %>%
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
        )
    })

    # show all time serie dataset
    output$used_data <- renderDataTable({
      req(data_for_monthly_boxplots())
      data_for_monthly_boxplots()
    })

    observeEvent(input$showDataset, {
      req(data_for_monthly_boxplots())

      shinyFeedback::hideFeedback("dataInput")

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        req(data_for_monthly_boxplots())
        data_for_monthly_boxplots()
      })
    })
    # show datset summary
    observeEvent(input$showDatasetSummary, {
      req(stats_summary())

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        req(stats_summary())
        stats_summary()
      })
    })


    #return
    return(
      list(
        data_for_monthlyBoxplot = reactive({data_for_monthly_boxplots()}),
        stats_summary = reactive({stats_summary()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4MonthlyBoxplot_ui("data4MonthlyBoxplot_1")

## To be copied in the server
# mod_data4MonthlyBoxplot_server("data4MonthlyBoxplot_1")
