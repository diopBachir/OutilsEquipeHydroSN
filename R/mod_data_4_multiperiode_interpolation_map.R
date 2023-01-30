#' data_4_multiperiode_interpolation_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'  @import dplyr
#'
mod_data_4_multiperiode_interpolation_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="left",
             column(12,
                    fileInput(
                      ns("dataInput"), label = div("Données d'Interpolation", style="font-size:95%;"),
                      accept = c(".csv", ".xlsx", ".xls"),  buttonLabel = "Charger",
                      placeholder = "fichier CSV ou Excel"
                    )
             )
    ),
    verbatimTextOutput(ns("test"))
  )
}

#' data_4_multiperiode_interpolation_map Server Functions
#'
#' @noRd
mod_data_4_multiperiode_interpolation_map_server <- function(id){
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
    data_for_interpolation<- reactive({

      if(!extension_fichier()){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Veillez choisir un fichier {.csv|.xlsx|.xls} !"
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
        import_data(userFile()$datapath)
      })

      # validation du nom de la première colonne
      req(data_loaded())
      colonne_name_test<- test_match_order(names(data_loaded())[1:3], c("Station", "Longitude", "Latitude"))
      # alert
      if(!colonne_name_test){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !colonne_name_test,
          paste0(
            "Codage des noms de colonne incorrect !"
          )
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Les trois premières colonnes doivent se nommer {Station, Longitude, Latitude}. La première ",
            "colonne porte obligatoirement le nom {Station} avec un respect strict de la casse."
          )
        )
      }


      # Validation du type des colonnes
      req(colonne_name_test)
      data_type_columns<- sum(sapply(data_loaded()[-1], is.numeric))==ncol(data_loaded())-1
      # alert
      if(!data_type_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !data_type_columns,
          paste0(
            "Colonnes Non Numériques Détectées !"
          )
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Toutes les colonnes {à partir de la deuxième colonne} ne sont pas numériques. ",
            "Veillez convertir ces colonnes en nombres avant de continuer."
          )
        )
      }

      # vérification du nombre de colonnes
      req(data_type_columns)
      nb_columns<- ncol(data_loaded()) >= 4
      # alert
      if(!nb_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !nb_columns,
          paste0(
            "Aucune Donnée A Interpoler!"
          )
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Aucune donnée à interpoler n'a été trouvée dans le fichier importé !"
          )
        )
      }

      # # tester les noms de stations
      # req(data_type_columns, data_loaded(), stations_db)
      # test_id_stations<- sum(names(data_loaded())[-1] %in% stations_db$Station) == nrow(stations_db)
      # shinyFeedback::feedbackWarning(
      #   "dataInput", !test_id_stations,
      #   paste0(
      #     "Problème avec les noms de stations !"
      #   )
      # )
      # # alert
      # if(!test_id_stations){
      #   shinyalert::shinyalert(
      #     "Erreur Chargement !!",
      #     paste(
      #       "Certains noms de stations dans le fichier des stations et celui des données d'interpolation ",
      #       "ne sont pas identiques. Cela risque de biaiser les résultats de l'interpolation. Essayer d'identifier ",
      #       "et de recoder ces noms. Si les noms contiennent des caractètes comme des tirets ou des espaces vides, ",
      #       "veillez les remplacer par le caratère {.} ou { _ } ! Faites également attention aux accents dans  les noms de colonnes !"
      #     )
      #   )
      # }

      # return
      if(nb_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackSuccess(
          "dataInput", nb_columns,
          paste0(
            "Ficher Chargé Avec Succès ", icon("check")
          )
        )
        return(data_loaded()[!duplicated(data_loaded()$Station),])
      }else{
        return(NULL)
      }

    })

    ## Summarising data
    # stats_summary<-reactive({
    #   req(extension_fichier(), data_for_interpolation())
    #   data_for_interpolation() %>%
    #     dplyr::select(-c(Station, Longitude, Latitude)) %>%
    #     pivot_longer(
    #       1:(ncol(data_for_interpolation())-3), names_to = "Variable", values_to = "Valeur"
    #     ) %>%
    #     group_by(Variable) %>%
    #     summarise(
    #       Min. = min(Valeur, na.rm = T), Quart1 = quantile(Valeur, .25),
    #       Médianne = median(Valeur), Quart3 = quantile(Valeur, .75),
    #       Moyenne = mean(Valeur), Max = max(Valeur),
    #       "Ecart type" = sd(Valeur)
    #     ) %>%
    #     mutate(
    #       across(where(is.numeric), ~round(., 2), .names = "{.col}")
    #     )
    # })

    #return
    return(
      list(
        data_for_time_serie_interpolation = reactive({
          req(data_for_interpolation())
          data_for_interpolation()
        })
      )
    )

  })
}

## To be copied in the UI
# mod_data_4_multiperiode_interpolation_map_ui("data_4_multiperiode_interpolation_map_1")

## To be copied in the server
# mod_data_4_multiperiode_interpolation_map_server("data_4_multiperiode_interpolation_map_1")
