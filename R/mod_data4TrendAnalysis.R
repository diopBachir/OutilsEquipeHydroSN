#' data4TrendAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4TrendAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="left",
             column(12,
                    fileInput(
                      ns("dataInput"), label = div("Données [Serie.s Temporelle.s]", style="font-size:90%;"),
                      accept = c(".csv", ".xlsx", ".xls"),  buttonLabel = "Charger",
                      placeholder = "fichier CSV ou Excel"
                    )
             )
    )
  )
}

#' data4TrendAnalysis Server Functions
#'
#' @noRd
mod_data4TrendAnalysis_server <- function(id){
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
    data_for_interpolation<- reactive({

      if(!extension_fichier()){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Type De Fichier Incorrect !"
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
        tryCatch(
          import_data(userFile()$datapath),
          error = function(err) { data.frame(NULL) }
        )
      })

      # s'assurer que le fichier importer n'est pas vide
      testNotEmptySheet<- reactive({
        req(data_loaded())
        nrow(data_loaded()) != 0
      })
      # shinyalert
      if(!testNotEmptySheet()){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !testNotEmptySheet(),
          paste0(
            "Fichier Incorrect !!"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Le fichier importé est probablement vide : [ {nrow(data)=0} ]"
          )
        )
      }

      # validation du nom de la première colonne
      req(testNotEmptySheet())
      colonne_1st<- names(data_loaded())[1] == "Station_ID"
      # alert
      if(!colonne_1st){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !colonne_1st,
          paste0(
            "Codage Fichier Incorrect !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "La première colonne du fichier doit obligatoirement se nommer [Station_ID].",
            "Si vous importez un fichier excel, il s'agit de la cellule [A1] !"
          )
        )
      }

      # validation de la correspondance entre les noms de stations des 1ère et 4ème lignes
      req(colonne_1st)
      first_4th_cols_test<- test_match_order(names(data_loaded())[-1], as.vector(t(data_loaded()[4,][-1])))
      # alert
      if(!first_4th_cols_test){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !first_4th_cols_test,
          paste0(
            "Codage Fichier Incorrect !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Les lignes 1 et 4 du fichier doivent obligatoirement contenir les mêmes noms de stations !"
          )
        )
      }

      # validation des noms des 3 premières lignes
      req(first_4th_cols_test)
      lonlat_test<- test_match_order(data_loaded()[[1]][c(1,2)], c("Longitude", "Latitude"))
      # alert
      if(!lonlat_test){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !lonlat_test,
          paste0(
            "Codage Fichier Incorrect !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Les deux premières lignes du fichier doivent obligatoirement se nommer ",
            "dans cet ordre [Longitude, Latitude]. Si vous importez un fichier excel, ",
            "il s'agit des cellules [A2 et A3] !"
          )
        )
      }

      # validation des noms des 3 premières lignes
      req(lonlat_test)
      colonne_date<- data_loaded()[[1]][4] == "Date"
      # alert
      if(!colonne_date){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !colonne_date,
          paste0(
            "Codage Fichier Incorrect !"
          )
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "La quatrième ligne du fichier doit obligatoirement se nommer [Date].",
            "Si vous importez un fichier excel, il s'agit de la cellule [A5] !"
          )
        )
      }

      # validation du format de la colonne {Date}
      req(colonne_date)
      format_date<- reactive({
        anyNA(lubridate::ymd(data_loaded()$Station_ID[-c(1:4)]))
      })
      # alert
      if(format_date()){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", format_date(),
          paste0(
            "Format Date Incorrect !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
            "doit être de la forme : { [YYYY/MM/DD | YYYY-MM-DD] } !"
          )
        )
      }

      # Validation du type des colonnes
      req(!format_date())
      data_test_date <- dplyr::mutate(data_loaded()[-c(3,4),-1], across(1:ncol(data_loaded())-1, as.numeric, .names = "{.col}"))
      data_type_columns<- sum(sapply(data_test_date, is.numeric))==ncol(data_loaded())-1
      # alert
      if(!data_type_columns){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !data_type_columns,
          paste0(
            "Colonnes Non Numériques !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Toutes les colonnes {à partir de la deuxième colonne et cinquième ligne} ne sont pas numériques. ",
            "Veillez convertir ces colonnes en nombres avant de continuer. Si vous importez un fichier excel, ",
            "les cellules à convertir commencent à partir de la cellules [B6] !"
          )
        )
      }

      # tester le nombre de lignes
      req(data_type_columns)
      test_nb_row<- nrow(data_loaded()) <= 1000000
      # alert
      if(!test_nb_row){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackWarning(
          "dataInput", !test_nb_row,
          paste0(
            "Limite (1000000 lignes) dépassée !"
          )
        )

        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Par précaution, il est impossible d'utiliser cet outil avec un fichier dépassant 1000000 lignes.",
            "Ce comportement est délibéré pour ne pas bousiller le système utilisé ! Si vous avez un fichier ",
            "dépassant cette limite, veillez diviser le fichier en plusieurs parties pour les importer et les ",
            "traiter un à un !"
          )
        )
      }

      # return
      if(test_nb_row){
        shinyFeedback::hideFeedback("dataInput")
        shinyFeedback::feedbackSuccess(
          "dataInput", test_nb_row,
          paste0(
            "Ficher Chargé Avec Succès ", icon("check")
          )
        )
        return(data_loaded())
      }else{
        return(NULL)
      }

    })

    #return
    return(
      list(
        data_for_time_serie_trend_analysis = reactive({
          req(data_for_interpolation())
          tmp.tb <- data_for_interpolation()
          # conversion des dates
          tmp.tb$Station_ID <- c("Longitude", "Latitude", NA, "Date", tmp.tb$Station_ID[-c(1:4)])
          tmp.tb
        })
      )
    )

  })
}

## To be copied in the UI
# mod_data4TrendAnalysis_ui("data4TrendAnalysis_1")

## To be copied in the server
# mod_data4TrendAnalysis_server("data4TrendAnalysis_1")
