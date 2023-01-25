#' data4AnnualInventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4AnnualInventory_ui <- function(id){
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

    DT::dataTableOutput(ns("used_data"))
  )
}

#' data4AnnualInventory Server Functions
#'
#' @noRd
mod_data4AnnualInventory_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Customizing how DataTables displays missing values in Shiny
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(151,151,151)', 'font-style': 'normal', 'font-family': 'georgia'});",
      "    }",
      "  }",
      "}"
    )

    # to receive data
    dataLoad<- reactiveVal()

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
    data_for_annualInventoryGraph<- reactive({

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
        test<- import_data(userFile()$datapath) %>%
          dplyr::mutate(
            dplyr::across(-1, as.numeric, .names = "{.col}"),
            # dplyr::across(tidyselect::where(is.numeric), ~replace_na(., median(., na.rm=TRUE))),
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )
        # purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=50)
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
      format_date<- sum(grepl("[0-9]{4}", data_loaded()$Date)) == nrow(data_loaded())
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
            "doit être de la forme { [YYYY] } !"
          )
        )
      }

      # validation du numéro du mois
      # req(format_date)
      # test_mois<- sum(as.numeric(str_sub(data_loaded()$Date, 6, 7)) <=12) == nrow(data_loaded())
      # shinyFeedback::feedbackWarning(
      #   "dataInput", !test_mois,
      #   paste0(
      #     "Le numéro du mois ne peut être supérieur à 12 !"
      #   )
      # )
      # # alert
      # if(!test_mois){
      #   shinyalert::shinyalert(
      #     "Erreur Chargement !!",
      #     paste(
      #       "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
      #       "doit être de la forme : YYYY/MM/DD ou YYYY-MM-DD. Avez-vous inverser la position du mois ou de la date ? ",
      #       "Le numéro du mois ne peut être supérieur à 12 !"
      #     )
      #   )
      # }
      #

      # Validation du type des colonnes
      req(format_date)
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

      # Validation du nombre de stations
      req(data_type_columns)
      nbre_colonnes<- ncol(data_loaded()) <= 81
      shinyFeedback::feedbackWarning(
        "dataInput", !nbre_colonnes,
        paste0(
          "Trop de Variables à Inventorier !"
        )
      )
      # alert
      if(!nbre_colonnes){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous avez dépasser la limite du nombre de variables autorisé. Le fichier chargé doit contenir 81 colonnes ",
            "au maximum, la première colonne représentant la date et nommée obligatoirement { Date } !"
          )
        )
      }

      # return
      if(nbre_colonnes){
        return(
          data_loaded()
        )
      }else{
        return(NULL)
      }

    })

    output$used_data <- DT::renderDataTable({
      req(data_for_annualInventoryGraph())
      data_for_annualInventoryGraph()
    }, options = list(rowCallback = JS(rowCallback)))

    # show all time serie dataset
    observeEvent(input$showDataset, {
      req(data_for_annualInventoryGraph())

      shinyFeedback::hideFeedback("dataInput")

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      dataLoad(data_for_annualInventoryGraph())

      output$used_data <- DT::renderDataTable({
        req(dataLoad())
        dataLoad()
      }, options = list(rowCallback = JS(rowCallback)))

    })

    # show datset summary
    observeEvent(input$showDatasetSummary, {
      # req(stats_summary())

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      ## Summarising data
      stats_summary<-reactive({
        req(data_for_annualInventoryGraph())

        naniar::miss_var_summary(data_for_annualInventoryGraph()[,-1])  %>%
          dplyr::mutate(pct_miss = round(pct_miss, 3)) %>%
          dplyr::rename("Total Lacunes" = n_miss, "% Lacunes" = pct_miss)

      })

      dataLoad(stats_summary())

      output$used_data <- DT::renderDataTable({
        req(dataLoad())
        dataLoad()
      }, options = list(rowCallback = JS(rowCallback)))

    })

    #return
    return(
      list(
        data_for_annualInventoryGraph = reactive({data_for_annualInventoryGraph()}),
        stats_summary = reactive({stats_summary()})
      )
    )
  })
}

## To be copied in the UI
# mod_data4AnnualInventory_ui("data4AnnualInventory_1")

## To be copied in the server
# mod_data4AnnualInventory_server("data4AnnualInventory_1")
