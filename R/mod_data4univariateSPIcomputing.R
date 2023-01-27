#' data4univariateSPIcomputing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4univariateSPIcomputing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="left",
             column(12,
                    fileInput(
                      ns("dataInput"), label="",
                      # label = div("Données", style = "color:#gray;family:Georgia;font-size:120%"),
                      accept = c(".csv", ".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "fichier CSV ou Excel", width = "100%"
                    )
             )
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(
      column(6, dataTableOutput(ns("used_data_timeserie"))),
      column(6, verbatimTextOutput(ns("used_data_summary")))
    )
  )
}

#' data4univariateSPIcomputing Server Functions
#'
#' @noRd
mod_data4univariateSPIcomputing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Customizing how DataTables displays missing values in Shiny

    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
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
    data_for_SPIcomputing<- reactive({

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
            "doit être de la forme {[YYYY/MM/DD]} !"
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
      nbre_colonnes<- ncol(data_loaded()) == 2
      shinyFeedback::feedbackWarning(
        "dataInput", !nbre_colonnes,
        paste0(
          "Nombre de Variables Incorrect !"
        )
      )
      # alert
      if(!nbre_colonnes){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous avez dépasser la limite du nombre de variables autorisé. Le fichier chargé doit contenir exactement deux colonnes ",
            "la première colonne représentant la date et nommée obligatoirement { Date } !"
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

    # show all time serie dataset
    output$used_data_timeserie <- renderDataTable({
      req(data_for_SPIcomputing())
      data_for_SPIcomputing()
    })

    # show datset summary
    ## Summarising data
    stats_summary<-reactive({
      req(data_for_SPIcomputing())

      summary(data_for_SPIcomputing())

    })

    output$used_data_summary <- renderPrint({
      req(stats_summary())
      stats_summary()
    })

    #return
    return(
      list(
        data_for_SPIcomputing = reactive({data_for_SPIcomputing()}),
        stats_summary = reactive({stats_summary()})
      )
    )
  })
}

## To be copied in the UI
# mod_data4univariateSPIcomputing_ui("data4univariateSPIcomputing_1")

## To be copied in the server
# mod_data4univariateSPIcomputing_server("data4univariateSPIcomputing_1")
