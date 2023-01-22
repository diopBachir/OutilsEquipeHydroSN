#' data4FacetsUnivariateBoxplotFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4FacetsUnivariateBoxplotFile_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(align="center",
             column(6,
                    fileInput(
                      ns("dataInput"), label = "",
                      accept = c(".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "fichier Excel", multiple = F
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

    dataTableOutput(ns("used_data")),

    shinyBS::bsModal(
      id = "dataSummarising",
      title = "Résumé Statistque",
      trigger = ns("showDatasetSummary"),
      size = "large",
      dataTableOutput(ns("dataSumm"))
    )

  )
}

#' data4FacetsUnivariateBoxplotFile Server Functions
#'
#' @noRd
mod_data4FacetsUnivariateBoxplotFile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    userFile <- reactive({
      # If no file is selected, don't do anything
      req(input$dataInput)
      input$dataInput
    })

    # # test du nombre de Classeur
    # nbFiles <- reactive({
    #   req(userFile())
    #   length(userFile()$datapath) > 1
    # })

    ##### vérification de l'extention du fichier
    extension_fichier<- reactive({
      req(userFile())
      tools::file_ext(userFile()$datapath)  %in% c("xlsx", "xls")
    })

    # loadingData
    data_for_univariate_facets_boxplots<- reactive({

      # # validation du nombre de classeur
      # if(!nbFiles()){
      #   shinyFeedback::feedbackWarning(
      #     "dataInput", !nbFiles(),
      #     paste0(
      #       "Nombre de fichiers insuffisant !"
      #     )
      #   )
      #   # shinyalert
      #   if(!nbFiles()){
      #     shinyalert::shinyalert(
      #       "Erreur Chargement !!",
      #       paste(
      #         "Vous devez charger au moins deux fichier excels !."
      #       )
      #     )
      #   }
      # }else{
      #   shinyFeedback::hideFeedback("dataInput")
      # }

      # req(nbFiles())
      if(!extension_fichier()){
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Veillez choisir des fichiers excels {.xlsx|xls} !"
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
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      # Tester l'égalité des nombres de feuilles
      # testEqualSheet<- reactive({
      #   req(extension_fichier())
      #   sheet.init<- readxl::excel_sheets(userFile()$datapath[1])
      #   for(worksheet in userFile()$datapath[-1]){
      #     sheet.tmp<- readxl::excel_sheets(worksheet)
      #     if(length(sheet.init)==length(sheet.tmp)){
      #       egal<- TRUE
      #     }else{
      #       egal<- FALSE
      #     }
      #   }
      #   egal
      # })

      # if(!(testEqualSheet()==TRUE)){
      #   shinyFeedback::feedbackWarning(
      #     "dataInput", !(testEqualSheet()==TRUE),
      #     paste0(
      #       "Nombre de Groups Par Classeur Non Correct !"
      #     )
      #   )
      #   # shinyalert
      #   if(!(testEqualSheet()==TRUE)){
      #     shinyalert::shinyalert(
      #       "Erreur Chargement !!",
      #       paste(
      #         "Tous les Classeurs doivent contenir le même nombre de Groups !"
      #       )
      #     )
      #   }
      # }

      # # Tester si les noms des feuilles sont identiques dans tous les classeurs
      # testNameSheet<- reactive({
      #   req(testEqualSheet())
      #   sheet.init<- readxl::excel_sheets(userFile()$datapath[1])
      #   for(worksheet in userFile()$datapath[-1]){
      #     sheet.tmp<- readxl::excel_sheets(worksheet)
      #     if(sum(sheet.init==sheet.tmp)==length(sheet.init)){
      #       egalSheetName<- TRUE
      #     }else{
      #       egalSheetName<- FALSE
      #     }
      #   }
      #
      #   egalSheetName
      # })
      #
      # if(!(testNameSheet()==TRUE)){
      #   shinyFeedback::feedbackWarning(
      #     "dataInput", !(testNameSheet()==TRUE),
      #     paste0(
      #       "Noms de Groups Non Identiques !"
      #     )
      #   )
      #   # shinyalert
      #   if(!(testNameSheet()==TRUE)){
      #     shinyalert::shinyalert(
      #       "Erreur Chargement !!",
      #       paste(
      #         "Les Noms de Groups doivent être les mêmes dans tous les Classeurs. Vous devez vous assurez que les classeurs ",
      #         "sont formattés de la même manière, qu'ils contiennent les mêmes noms et nombres de  feuilles, ordonnées de la même ",
      #         "manière (optionnel pour les noms de feuilles), et contiennent le même nombre de lignes par colonne !"
      #       )
      #     )
      #   }
      # }

      # Tester si les noms des 2 premières colonne dans tous les fichier
      testFirstColName<- reactive({
        req(extension_fichier())
        firstColOK<-TRUE
        for(feuille in readxl::excel_sheets(userFile()$datapath)){
          df.temp<- tryCatch(
            readxl::read_excel(userFile()$datapath,feuille),
            error = function(err) { data.frame(NULL)  }
          )
          if(nrow(df.temp)==0 | names(df.temp)[1] != "Seuil"){
            firstColOK<-FALSE
            break
          }
        }

        firstColOK
      })

      if(!(testFirstColName()==TRUE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testFirstColName()==TRUE),
          paste0(
            "Nom de la Première Colonne Incorrect"
          )
        )
        # shinyalert
        if(!(testFirstColName()==TRUE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Assurez-vous que la première colonne de chaque feuille est nommé { Seuil } avec un respect strict de la casse. ",
              "La colonne seuil va servir à tracer la valeur Seuil dans Chaque Facet. Vous devez également vous assurer qu'aucune ",
              "feuille est vide :"
            )
          )
        }
      }

      # Tester les si y'a des feuilles vides sans données
      testEmptySheet<- reactive({
        req(testFirstColName()==TRUE)
        emptySheet<-FALSE
        for(feuille in readxl::excel_sheets(userFile()$datapath)){
          df.temp<- tryCatch(
            readxl::read_excel(userFile()$datapath,feuille,col_names = T),
            error = function(err) { data.frame(NULL) }
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
            "Group Vide Suspectée  !"
          )
        )
        # shinyalert
        if(!(testEmptySheet()==FALSE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Certains Classeurs contiennent probablement une ou plusieurs feuille Vide. Essayez de les supprimer ",
              "ou de les corriger avant de continuer"
            )
          )
        }
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      # valider les noms de colonnes
      testColNames<- reactive({
        req(!(testEmptySheet()==TRUE))
        # init table
        ini.df<- dplyr::bind_cols(
          import_list(userFile()$datapath, setclass = "tbl"), .id = "Group"
        )  %>%
          dplyr::select(-tidyr::contains(c("Seuil"))[-1]) %>%
          dplyr::rename(Facet = 1, Seuil=2)

        ini.df
      })

      if(anyNA(testColNames())){
        shinyFeedback::feedbackWarning(
          "dataInput", anyNA(testColNames()),
          paste0(
            "Erreur Dans les Colonnes [Noms ou Nbre de lignes]"
          )
        )
        # shinyalert
        if(anyNA(testColNames())){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Assurez vous que toutes les colonnes ",
              "contiennent le même nombre de lignes. Si cette contrainte n'est pas respectée, l'algorithme ne ",
              "fonctionnera pas, et aucune garantie (de résultats cohérents) n'est fournie dans le cas contraire !"
            )
          )
        }
      }


      # load data
      data_loaded<-reactive({
        req(!anyNA(testColNames()))

        tbl.init <- readxl::read_excel(userFile()$datapath, sheet = readxl::excel_sheets(userFile()$datapath)[1]) %>%
          dplyr::mutate(Group = readxl::excel_sheets(userFile()$datapath)[1]) %>%
          tidyr::pivot_longer(
            -c(Group,  Seuil),
            names_to = "Variable",
            values_to = "Valeur"
          )
        for(feuille in readxl::excel_sheets(userFile()$datapath)[-1]){
          df.temp<-  readxl::read_excel(userFile()$datapath,feuille,col_names = T) %>%
            dplyr::mutate(Group = feuille) %>%
            tidyr::pivot_longer(
              -c(Group,  Seuil),
              names_to = "Variable",
              values_to = "Valeur"
            )
          tbl.init <- dplyr::bind_rows(tbl.init, df.temp)
        }

        tbl.init  %>%
          dplyr::mutate(Variable = factor(Variable), Group = factor(Group))

      })

      return(data_loaded())
    })

    # show all time serie dataset
    output$used_data <- renderDataTable({
      req(data_for_univariate_facets_boxplots())
      data_for_univariate_facets_boxplots()
    })

    observeEvent(input$showDataset, {
      req(userFile())

      # shinyFeedback::hideFeedback("dataInput")

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        req(data_for_univariate_facets_boxplots())
        data_for_univariate_facets_boxplots()
      })
    })

    # show datset summary
    observeEvent(input$showDatasetSummary, {
      req(data_for_univariate_facets_boxplots())

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({

        data_for_univariate_facets_boxplots() %>%
          dplyr::group_by( Group, Variable) %>%
          dplyr::summarise(
            Min. = min(Valeur, na.rm = T), Q1 = quantile(Valeur, .25),
            Médianne = median(Valeur), Q3 = quantile(Valeur, .75),
            Moyenne = mean(Valeur), Max = max(Valeur),
            "Ec. type" = sd(Valeur)
          )  %>%
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )

      })

    })

    # return
    return(
      list(
        data_for_facetsUnivariateBoxplot = reactive({data_for_univariate_facets_boxplots()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4FacetsUnivariateBoxplotFile_ui("data4FacetsUnivariateBoxplotFile_1")

## To be copied in the server
# mod_data4FacetsUnivariateBoxplotFile_server("data4FacetsUnivariateBoxplotFile_1")
