#' data4TimeSerieInterpolation_shinyFiles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4TimeSerieInterpolation_shinyFiles_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyFilesButton(
                      id = ns("datatable_file"), label = "Données d'Interpolation",
                      title = "Sélectionner Le Fichier Excel ou CSV contenant les données d'Interpolation !",
                      multiple = FALSE, icon = icon("file-import"), buttonType = "info", width = "100%"
                    )
             ),
             column(12, verbatimTextOutput(ns("load_datatable_file_confirm")))
    )
  )
}

#' data4TimeSerieInterpolation_shinyFiles Server Functions
#'
#' @noRd
mod_data4TimeSerieInterpolation_shinyFiles_server <- function(id, workingDirectory){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_tabular_data <- reactiveVal()

    observeEvent(input$datatable_file, {
      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      req(volumes_default_dir())
      shinyFiles::shinyFileChoose(input, "datatable_file", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      # chargement du fichier
      fichier_tabulaire <- reactive({
        req(file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$datatable_file)$datapath))
        shinyFiles::parseFilePaths(volumes_default_dir(), input$datatable_file)$datapath
      })

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(fichier_tabulaire())
        if(!tools::file_ext(fichier_tabulaire()) %in% c("csv", "xlsx", "xls")){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Le type de fichier importé n'est pas celui attendu. Veuillez importer un fichier Excel ou CSV !"
            )
          )
          return(FALSE)
        }else{
          return(TRUE)
        }
      })

      # load data
      data_loaded<-reactive({
        req(extension_fichier(), fichier_tabulaire())
        tryCatch(
          import_data(fichier_tabulaire()),
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
      if(!colonne_1st){shinyalert::shinyalert(
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
        anyNA(as.Date(
          as.character(
            janitor::excel_numeric_to_date(
              as.numeric(as.character(data_loaded()$Station_ID[-c(1:4)])), date_system = "modern"
            )
          )
        ))
      })
      # alert
      if(format_date()==TRUE){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
            "doit être de la forme : { [DD/MM/YYYY] } !"
          )
        )
      }

      # Validation du type des colonnes
      req(format_date()==FALSE)
      data_test_date<- dplyr::mutate(data_loaded()[-c(3,4),-1], across(1:ncol(data_loaded())-1, as.numeric, .names = "{.col}"))
      data_type_columns<- sum(sapply(data_test_date, is.numeric))==ncol(data_loaded())-1
      # alert
      if(!data_type_columns){
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
      test_nb_row<- nrow(data_loaded()) <= 36500
      # alert
      if(!test_nb_row){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Par précaution, il est impossible d'utiliser cet outil avec un fichier dépassant 36500 lignes.",
            "Ce comportement est délibéré pour ne pas bousiller le système utilisé ! Si vous avez un fichier ",
            "dépassant cette limite, veillez diviser le fichier en plusieurs parties pour les importer et les ",
            "traiter un à un !"
          )
        )
      }

      # return
      if(test_nb_row){
        output$load_datatable_file_confirm<- renderPrint({
          req(test_nb_row==TRUE)
          cat(fichier_tabulaire())
        })
        load_tabular_data(data_loaded())
      }else{
        return(NULL)
      }

    })

    #return
    return(
      list(
        data_for_time_serie_interpolation = reactive({
          req(load_tabular_data())
          tmp.tb <- load_tabular_data()
          # conversion des dates
          tmp.tb$Station_ID <- c(
            "Longitude", "Latitude", NA, "Date",
            ### conversion des dates
            stringr::str_replace_all(
              as.character(
                janitor::excel_numeric_to_date(
                  as.numeric(as.character(tmp.tb$Station_ID[-c(1:4)])), date_system = "modern"
                )
              ), "-", "/"
            )
          )
          tmp.tb
        })
      )
    )

  })
}

## To be copied in the UI
# mod_data4TimeSerieInterpolation_shinyFiles_ui("data4TimeSerieInterpolation_shinyFiles_1")

## To be copied in the server
# mod_data4TimeSerieInterpolation_shinyFiles_server("data4TimeSerieInterpolation_shinyFiles_1")
