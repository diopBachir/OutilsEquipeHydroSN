#' data4ETOcomputing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4ETOcomputing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="left",
             column(12,
                    fileInput(
                      ns("dataInput"), label = div("Importer le.s Fichier.s contenant les données météorologiques",
                                                   style = "color:#gray;family:Georgia;font-size:120%"),
                      accept = c(".csv", ".xls", ".xlsx"),  buttonLabel = "Charger...",
                      placeholder = "fichiers CSV", multiple = T, width = "100%"
                    )
             )
    ),

    div(dataTableOutput(ns("used_data")), style="font-size:80%;")
  )
}

#' data4ETOcomputing Server Functions
#'
#' @noRd
mod_data4ETOcomputing_server <- function(id){
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
      sum(tools::file_ext(userFile()$datapath)  %in% c("csv", "xlsx", "xls")) == length(userFile()$datapath)
    })

    # loadingData
    data_for_dailyET0computing<- reactive({

      req(userFile())
      if(!extension_fichier()){
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Veillez choisir un ou des fichiers CSV ou EXCEL !"
          )
        )
        # shinyalert
        if(!extension_fichier()){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez choisir un ou des fichiers CSV {.csv} ou EXECL {.xlsx}"
            )
          )
        }
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      # Tester si les noms de colonnes requis sont présents dans tous les fichiers avec le même ordre
      testColReq<- reactive({
        req(extension_fichier())
        testColReq<- TRUE
        for(file in userFile()$datapath){
          temp.df.col.req.test <-  import_data(file)
          if(
            !test_match_order(
              names(temp.df.col.req.test),
              c("nom", "longitude", "latitude", "altitude", "date","Rs", "Ra", "Tmean", "HRmean", "U2", "Tmax", "Tmin")
            )){
            testColReq<- FALSE
            break
          }
        }
        testColReq
      })

      if(!testColReq()){
        shinyFeedback::feedbackWarning(
          "dataInput", !testColReq(),
          paste0(
            "Problèmes avec les noms de colonnes !"
          )
        )
        # shinyalert
        if(!testColReq()){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Les Noms de Colonnes doivent être les mêmes dans tous les Fichiers Chargés. Vous devez vous assurez que les Fichiers ",
              "sont formattés de la même manière, qu'ils contiennent les mêmes noms et nombres de  Colonnes, ordonnées de la même manière. ",
              "Chaque colonne doit contenir les colonnes suivantes dans l'ordre : ",
              "[nom, longitude, latitude, altitude, date,Rs, Ra, Tmean, HRmean, U2, Tmax, Tmin] !"
            )
          )
        }
      }



      # Tester les si y'a des fichiers vides sans données
      testEmptyFile<- reactive({
        req(testColReq())
        testEmptyFilebool<- FALSE
        for(file in userFile()$datapath){
          temp.df.empty<- tryCatch(
            import_data(file),
            error = function(err) { data.frame(NULL)  }
          )
          if(nrow(temp.df.empty)==0){
            testEmptyFilebool<- TRUE
            break
          }
        }
        testEmptyFilebool
      })

      if(!(testEmptyFile()==FALSE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testEmptyFile()==FALSE),
          paste0(
            "Fichier.s Vide.s Suspecté.s  !"
          )
        )
        # shinyalert
        if(!(testEmptyFile()==FALSE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Certains Fichiers sont probablement Vides [nrow(data)=0]. Essayez de les identifier ",
              "et de les supprimer avant de continuer !"
            )
          )
        }
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      # Test du format de la Date
      testDateFormat<- reactive({
        req(!(testEmptyFile()==TRUE))
        testDateFormatbool<- TRUE
        for(file in userFile()$datapath){
          temp.date.parse<- lubridate::ymd(import_data(file)[["date"]])
          if(anyNA(temp.date.parse)){
            testDateFormatbool<- FALSE
            break
          }
        }
        testDateFormatbool
      })

      if(!(testDateFormat()==TRUE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testDateFormat()==TRUE),
          paste0(
            "Erreur Format Date"
          )
        )
        # shinyalert
        if(!(testDateFormat()==TRUE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
              "doit être de la forme { [YYYY/MM/DD] } ou { [YYYY-MM-DD] } !"
            )
          )
        }
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      # load data
      data_loaded<-reactive({
        req(testDateFormat())

        userFile()

      })

      return(data_loaded())
    })

    # output
    output$used_data <- renderDataTable({
      req(data_for_dailyET0computing())
      data_for_dailyET0computing()
    })

    # return
    return(
      list(
        data_files_path_for_dailyET0computing = reactive({data_for_dailyET0computing()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4ETOcomputing_ui("data4ETOcomputing_1")

## To be copied in the server
# mod_data4ETOcomputing_server("data4ETOcomputing_1")
