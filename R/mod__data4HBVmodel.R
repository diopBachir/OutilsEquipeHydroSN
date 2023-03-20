#' _data4HBVmodel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod__data4HBVmodel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="left",
             column(12,
                    fileInput(
                      ns("dataInput"),
                      label = div("Chargés Les Données Nécessaire : Pluies (mm), ETP (mm), Température (°C), Débits (mm)", style = "color:#gray;family:Georgia;font-size:95%"),
                      accept = c(".csv", ".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "fichier CSV ou Excel", width = "100%"
                    )
             )
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(
      column(8,  div(DT::dataTableOutput(ns("used_data")), style="font-size:80%")),
      column(4,  verbatimTextOutput(ns("used_data_summary"))),
    )
  )
}

#' _data4HBVmodel Server Functions
#'
#' @noRd
mod__data4HBVmodel_server <- function(id){
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
    data_for_dailyInventoryGraph<- reactive({

      if(!extension_fichier()){
        shinyFeedback::hideFeedback("dataInput")
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

      req(extension_fichier())

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
      colonne_names<- test_match_order(names(data_loaded()), c("date", "PmmObs", "ETP", "Temp", "Qobs"))
      # alert
      if(!colonne_names){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !colonne_names,
          paste0(
            "Codage Des Noms de Colonnes Incorrect !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Les colonnes du fichier importé doivent se nommer dans cet ordre : [date, PmmObs, ETP, Temp, Qobs] ",
            "pour, respectivement, la date (date), les pluies observés (PmmObs), l'évapotranspiration (ETP) ",
            "la température (Temp) et les débits observés (Qobs)."
          )
        )
      }

      # validation du format de la colonne {Date}
      req(colonne_names)
      format_date<- anyNA(as.Date(data_loaded()$date, format="%d/%m/%Y"))
      # format_date<- sum(grepl("[0-9]{2}/[0-9]{2}/[0-9]{4}", data_loaded()$date)) == nrow(data_loaded()) |
      #   sum(grepl("[0-9]{2}-[0-9]{2}-[0-9]{4}", data_loaded()$date)) == nrow(data_loaded())
      # alert
      if(format_date){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", format_date,
          paste0(
            "Vérifiez le format de la {Date} !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
            "doit être de la forme { [DD/MM/YYYY] } ou { [DD/MM/YYYY] } !"
          )
        )
      }

      # validation du numéro du mois
      # req(format_date)
      # test_mois<- sum(as.numeric(stringr::str_sub(data_loaded()$date, 4, 5)) <=12) == nrow(data_loaded())
      # # alert
      # if(!test_mois){
      #   shinyFeedback::hideFeedback("dataInput")
      #   shinyFeedback::feedbackWarning(
      #     "dataInput", !test_mois,
      #     paste0(
      #       "Le numéro du mois ne peut être supérieur à 12 !"
      #     )
      #   )
      #
      #   shinyalert::shinyalert(
      #     "Erreur Chargement !!",
      #     paste(
      #       "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
      #       "doit être de la forme : DD/MM/YYYY ou DD-MM-YYYY. Avez-vous inverser la position du mois ou de la date ? ",
      #       "Le numéro du mois ne peut être supérieur à 12 !"
      #     )
      #   )
      # }


      # Validation du type des colonnes
      req(!format_date)
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
        shinyFeedback::feedbackSuccess("dataInput", data_type_columns,
                                       paste0(
                                         "Fichier Chargé Avec Succès ", icon("check"), icon("check"), icon("check")
                                       )
        )
        return(
          data_loaded() %>%
            dplyr::mutate(date = lubridate::ymd(as.Date(data_loaded()$date, format="%d/%m/%Y"), tz="UTC"))
        )
      }else{
        return(NULL)
      }

    })

    # show data
    output$used_data <- DT::renderDataTable({
      req(data_for_dailyInventoryGraph())
      data_for_dailyInventoryGraph()
    }, options = list(rowCallback = htmlwidgets::JS(rowCallback)))

    # show data summary
    output$used_data_summary <- renderPrint({
      req(data_for_dailyInventoryGraph())
      summary(data_for_dailyInventoryGraph())
    })

    #return
    return(
      list(
        data_for_HBV_modelisiation = reactive({data_for_dailyInventoryGraph()})
      )
    )
  })
}

## To be copied in the UI
# mod__data4HBVmodel_ui("data4HBVmodel_1")

## To be copied in the server
# mod__data4HBVmodel_server("data4HBVmodel_1")
