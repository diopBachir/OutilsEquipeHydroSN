#' data4DailyFacetInventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4DailyFacetInventory_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(
      ns("dataInput"), label = div("Données à Inventorier", style = "color:#gray;family:Georgia;font-size:120%"),
      accept = c(".xlsx", ".xls"),  buttonLabel = "Charger...",
      placeholder = "Classeur Excel", width = "100%"
    ),
    tags$hr(style="border-color:gray;"),

    withSpinner(verbatimTextOutput(ns("used_data")), type=5)
  )
}

#' data4DailyFacetInventory Server Functions
#'
#' @noRd
mod_data4DailyFacetInventory_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Customizing how DataTables displays missing values in Shiny
    # rowCallback <- c(
    #   "function(row, data){",
    #   "  for(var i=0; i<data.length; i++){",
    #   "    if(data[i] === null){",
    #   "      $('td:eq('+i+')', row).html('NA')",
    #   "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
    #   "    }",
    #   "  }",
    #   "}"
    # )

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
      tools::file_ext(userFile()$datapath) %in% c("xlsx", "xls")
    })

    # loadingData
    data_for_dailyFacetsInventoryGraph<- reactive({

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
              "Vous devez choisir un fichier avec l'extension {.xlsx} ou {xlsx}."
            )
          )
        }
      }


      # load data
      data_loaded<-reactive({
        req(extension_fichier())

        # tester le nombre de feuilles
        testSheet <- length(readxl::excel_sheets(userFile()$datapath)) > 1
        shinyFeedback::feedbackWarning(
          "dataInput", !testSheet,
          paste0(
            "Nombre de feuilles incorrect !"
          )
        )
        if(!testSheet){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Le fichier excel chargé doit contenir au moins deux (2) feuilles !"
            )
          )
        }

        # Tester les si y'a des feuilles vides sans données
        testEmptySheet<- reactive({
          req(testSheet)
          emptySheet<-FALSE
          for(feuille in readxl::excel_sheets(userFile()$datapath)){
            df.temp<- tryCatch(
              readxl::read_excel(userFile()$datapath,feuille,col_names = T),
              error = function(err) { data.frame(NULL)  }
            )
            if(nrow(df.temp)==0){
              emptySheet<-TRUE
              break
            }
          }

          emptySheet
        })

        if(!(testEmptySheet()==FALSE)){
          shinyFeedback::feedbackWarning(
            "dataInput", !(testEmptySheet()==FALSE),
            paste0(
              "Feuille Vide Suspectée  !"
            )
          )
          # shinyalert
          if(!(testEmptySheet()==FALSE)){
            shinyalert::shinyalert(
              "Erreur Chargement !!",
              paste(
                "Le Classeur chargé contient probablement une ou plusieurs feuille Vide. Essayez de les supprimer ",
                "ou de les corriger avant de continuer"
              )
            )
          }
        }else{
          shinyFeedback::hideFeedback("dataInput")
        }

        # Tester le nom de première colonne dans tous les feuilles
        testFirstColName<- reactive({
          req(testEmptySheet()==FALSE)
          firstColOK<-TRUE
          for(feuille in readxl::excel_sheets(userFile()$datapath)){
            df.tempFirstCol<- tryCatch(
              readxl::read_excel(userFile()$datapath,feuille),
              error = function(err) { data.frame(NULL)  }
            )
            if(nrow(df.tempFirstCol)==0 | names(df.tempFirstCol)[1] != "Date"){
              firstColOK<-FALSE
              break
            }
          }
          firstColOK
        })

        if(!(testFirstColName())){
          shinyFeedback::feedbackWarning(
            "dataInput", !(testFirstColName()),
            paste0(
              "Nom de La Première Colonne Incorrect"
            )
          )
          # shinyalert
          if(!(testFirstColName())){
            shinyalert::shinyalert(
              "Erreur Chargement !!",
              paste(
                "Assurez-vous que la première colonne est nommé { Date } dans Chaque Feuille !"
              )
            )
          }
        }

        # Valider le format de la date dans chaque feuille
        testDateFormat<- reactive({
          req(testFirstColName())
          dateFormatOK<-TRUE
          for(feuille in readxl::excel_sheets(userFile()$datapath)){
            df.temp<- tryCatch(
              readxl::read_excel(userFile()$datapath,feuille),
              error = function(err) { data.frame(NULL)  }
            )
            if(
              sum(grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", as.character(df.temp$Date))) != nrow(df.temp) &
              sum(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", as.character(df.temp$Date))) != nrow(df.temp)
            ){
              dateFormatOK<-FALSE
              break
            }
          }

          dateFormatOK
        })

        if(!testDateFormat()){
          shinyFeedback::feedbackWarning(
            "dataInput", !(testDateFormat()),
            paste0(
              "Format Date Invalide !"
            )
          )
          # shinyalert
          if(!testDateFormat()){
            shinyalert::shinyalert(
              "Erreur Chargement !!",
              paste(
                "Une ou plusieurs enregistrement ont un format de date incorrect ou une cellule vide dans la colonne Date. ",
                "Le format de la date doit être de la forme { [YYYY/MM/DD] } ou { [YYYY-MM-DD] } !"
              )
            )
          }
        }

        # Valider la position des mois dans la date
        testMonthPos<- reactive({
          req(testDateFormat())
          MonthPos<-TRUE
          for(feuille in readxl::excel_sheets(userFile()$datapath)){
            df.temp<- tryCatch(
              readxl::read_excel(userFile()$datapath,feuille),
              error = function(err) { data.frame(NULL)  }
            )
            if(
              nrow(df.temp)==0 | sum(as.numeric(stringr::str_sub(df.temp$Date, 6, 7)) <=12) != nrow(df.temp)
            ){
              MonthPos<-FALSE
              break
            }
          }

          MonthPos
        })

        if(!testMonthPos()){
          shinyFeedback::feedbackWarning(
            "dataInput", !(testMonthPos()==TRUE),
            paste0(
              "Format Date Invalide !"
            )
          )
          # shinyalert
          if(!testMonthPos()){
            shinyalert::shinyalert(
              "Erreur Chargement !!",
              paste(
                "Une ou plusieurs enregistrement ont un format de date incorrect. Le format de la date ",
                "doit être de la forme : YYYY/MM/DD ou YYYY-MM-DD. Avez-vous inverser la position du mois ou de la date ? ",
                "Le numéro du mois ne peut être supérieur à 12 !"
              )
            )
          }
        }

        # Valider le nombre de colonne autorisé
        testNbreColonnes<- reactive({
          req(testMonthPos())
          nbColOK<-TRUE
          for(feuille in readxl::excel_sheets(userFile()$datapath)){
            df.temp<- tryCatch(
              readxl::read_excel(userFile()$datapath,feuille),
              error = function(err) { data.frame(NULL)  }
            )
            if(
              ncol(df.temp)==0 | ncol(df.temp) > 81
            ){
              nbColOK<-FALSE
              break
            }
          }

          nbColOK
        })

        if(!(testNbreColonnes()==TRUE)){
          shinyFeedback::feedbackWarning(
            "dataInput", !(testNbreColonnes()==TRUE),
            paste0(
              "Trop de Variables à Inventorier !"
            )
          )
          # shinyalert
          if(!(testNbreColonnes()==TRUE)){
            shinyalert::shinyalert(
              "Erreur Chargement !!",
              paste(
                "Vous avez dépasser la limite du nombre de variables autorisé. Le fichier chargé doit contenir 81 colonnes ",
                "au maximum, la première colonne représentant la date et nommée obligatoirement { Date } !"
              )
            )
          }
        }

        # return
        req(testNbreColonnes())
        df_list <- list()
        for(i in 1:length(readxl::excel_sheets(userFile()$datapath))){
          tempDF <- readxl::read_excel(userFile()$datapath, readxl::excel_sheets(userFile()$datapath)[i])  %>%
            dplyr::mutate(Facet = readxl::excel_sheets(userFile()$datapath)[i])
          tempDF <- dplyr::select(tempDF, Facet, Date, 1:(ncol(tempDF)-2)) %>%
            dplyr::mutate(
              dplyr::across(c(-Date, -Facet), as.numeric, .names = "{.col}"),
              # dplyr::across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))),
              dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
            ) %>%
            dplyr::mutate(Date = lubridate::ymd(as.character(Date)), Date = as.numeric(format(Date, "%Y")))

          df_list[[i]] <- tempDF
        }

        df_list
      })



      data_loaded()

    })

    output$used_data <- renderPrint({
      req(data_for_dailyFacetsInventoryGraph())
      data_for_dailyFacetsInventoryGraph()
    })

    #return
    return(
      list(
        data_for_dailyFacetsInventoryGraph = reactive({data_for_dailyFacetsInventoryGraph()}),
        stats_summary = reactive({ stats_summary() })
      )
    )
  })
}

## To be copied in the UI
# mod_data4DailyFacetInventory_ui("data4DailyFacetInventory_1")

## To be copied in the server
# mod_data4DailyFacetInventory_server("data4DailyFacetInventory_1")
