#' data4MatrixFacetsMultivariateBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4MatrixFacetsMultivariateBoxplot_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    fluidRow(align="center",
             column(6,
                    fileInput(
                      ns("dataInput"), label = "",
                      # label = div("Données", style = "color:#gray;family:Georgia;font-size:120%"),
                      accept = c(".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "fichier Excel", multiple = T
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

#' data4MatrixFacetsMultivariateBoxplot Server Functions
#'
#' @noRd
mod_data4MatrixFacetsMultivariateBoxplot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    userFile <- reactive({
      # If no file is selected, don't do anything
      req(input$dataInput)
      input$dataInput
    })

    # test du nombre de Classeur
    nbFiles <- reactive({
      req(userFile())
      length(userFile()$datapath) > 1
    })

    ##### vérification de l'extention du fichier
    extension_fichier<- reactive({
      req(nbFiles())
      sum(tools::file_ext(userFile()$datapath)  %in% c("xlsx", "xls")) == length(userFile()$datapath)
    })

    # loadingData
    data_for_multivariate_facets_matrix_boxplots<- reactive({

      # validation du nombre de classeur
      if(!nbFiles()){
        shinyFeedback::feedbackWarning(
          "dataInput", !nbFiles(),
          paste0(
            "Nombre de fichiers insuffisant !"
          )
        )
        # shinyalert
        if(!nbFiles()){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez charger au moins deux fichiers excels !."
            )
          )
        }
      }else{
        shinyFeedback::hideFeedback("dataInput")
      }

      req(nbFiles())
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
      testEqualSheet<- reactive({
        req(extension_fichier())
        sheet.init<- readxl::excel_sheets(userFile()$datapath[1])
        for(worksheet in userFile()$datapath[-1]){
          sheet.tmp<- readxl::excel_sheets(worksheet)
          if(length(sheet.init)==length(sheet.tmp)){
            egal<- TRUE
          }else{
            egal<- FALSE
          }
        }
        egal
      })

      if(!(testEqualSheet()==TRUE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testEqualSheet()==TRUE),
          paste0(
            "Nombre de Feuilles Par Classeur Non Correct !"
          )
        )
        # shinyalert
        if(!(testEqualSheet()==TRUE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Tous les Classeurs doivent contenir le même nombre de Feuilles !"
            )
          )
        }
      }

      # Tester si les noms des feuilles sont identiques dans tous les classeurs
      testNameSheet<- reactive({
        req(testEqualSheet())
        sheet.init<- readxl::excel_sheets(userFile()$datapath[1])
        for(worksheet in userFile()$datapath[-1]){
          sheet.tmp<- readxl::excel_sheets(worksheet)
          if(sum(sheet.init==sheet.tmp)==length(sheet.init)){
            egalSheetName<- TRUE
          }else{
            egalSheetName<- FALSE
          }
        }

        egalSheetName
      })

      if(!(testNameSheet()==TRUE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testNameSheet()==TRUE),
          paste0(
            "Noms de Feuilles Non Identiques !"
          )
        )
        # shinyalert
        if(!(testNameSheet()==TRUE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Les Noms de Feuilles doivent être les mêmes dans tous les Classeurs. Vous devez vous assurez que les classeurs ",
              "sont formattés de la même manière, qu'ils contiennent les mêmes noms et nombres de  feuilles, ordonnées de la même ",
              "manière (optionnel pour les noms de feuilles), et contiennent le même nombre de lignes par colonne !"
            )
          )
        }
      }

      # Tester si les noms des 2 premières colonne dans tous les fichier
      testFirstColName<- reactive({
        req(testNameSheet()==TRUE)
        firstColOK<-TRUE
        for(worksheet in userFile()$datapath){
          for(feuille in readxl::excel_sheets(worksheet)){
            df.temp<- tryCatch(
              readxl::read_excel(worksheet,feuille),
              error = function(err) { data.frame(NULL)  }
            )
            if(nrow(df.temp)==0 | sum(names(df.temp)[1:3] %in% c("FacetX", "FacetY",  "Seuil"))!=3){
              firstColOK<-FALSE
              break
            }
          }
        }

        firstColOK
      })

      if(!(testFirstColName()==TRUE)){
        shinyFeedback::feedbackWarning(
          "dataInput", !(testFirstColName()==TRUE),
          paste0(
            "Nom des 3 Premières Colonnes Incorrects"
          )
        )
        # shinyalert
        if(!(testFirstColName()==TRUE)){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Assurez-vous que les trois (3) premières colonnes sont nommées { FacetX, FacetY, Seuil } dans l'ordre ",
              "avec un respect strict de la casse. Les valeurs des colonnes { FacetX, FacetY } sont utilisées dans ggplot2::facet_grid(). ",
              "La colonne seuil va servir à tracer la valeur Seuil dans Chaque Facet !"
            )
          )
        }
      }

      # Tester les si y'a des feuilles vides sans données
      testEmptySheet<- reactive({
        req(testFirstColName()==TRUE)
        emptySheet<-FALSE
        for(worksheet in userFile()$datapath){
          for(feuille in readxl::excel_sheets(worksheet)){
            df.temp<- tryCatch(
              readxl::read_excel(worksheet,feuille,col_names = T),
              error = function(err) { data.frame(NULL)  }
            )
            if(nrow(df.temp)==0){
              emptySheet<-TRUE
              break
            }
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
          import_list(userFile()$datapath[1], setclass = "tbl"), .id = "Group"
        )  %>%
          dplyr::select(-tidyselect::contains(c("FacetX"))[-1]) %>%
          dplyr::select(-tidyselect::contains(c("FacetY"))[-1]) %>%
          dplyr::select(-tidyselect::contains(c("Seuil"))[-1]) %>%
          dplyr::rename(FacetX = 1, FacetY = 2, Seuil=3)

        # binding table
        for (filePath in userFile()$datapath[-1]) {
          temp.df <- dplyr::bind_cols(
            import_list(filePath, setclass = "tbl"), .id = "Group"
          )  %>%
            dplyr::select(-tidyselect::contains(c("FacetX"))[-1]) %>%
            dplyr::select(-tidyselect::contains(c("FacetY"))[-1]) %>%
            dplyr::select(-tidyselect::contains(c("Seuil"))[-1]) %>%
            dplyr::rename(FacetX = 1, FacetY = 2, Seuil=3)

          ini.df <- dplyr::bind_rows(ini.df, temp.df)
        }

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
              "Assurez-vous que les mêmes noms de colonnes se retrouvent dans tous les Classeurs et ",
              "ordonnés de la même manière puis réessayez. Assurez vous également que toutes les colonnes ",
              "contiennent le même nombre de lignes. Si ces contraintes ne sont pas respectées, l'algorithme ne ",
              "fonctionnera pas, et aucune garantie (de résultats cohérents) n'est fournie dans le cas contraire !"
            )
          )
        }
      }


      # load data
      data_loaded<-reactive({
        req(!anyNA(testColNames()))

        tbl.init <- readxl::read_excel(userFile()$datapath[1], sheet = readxl::excel_sheets(userFile()$datapath[1])[1]) %>%
          dplyr::mutate(Group = readxl::excel_sheets(userFile()$datapath[1])[1]) %>%
          tidyr::pivot_longer(
            -c(Group, FacetX, FacetY,Seuil),
            names_to = "Variable",
            values_to = "Valeur"
          )
        for(feuille in readxl::excel_sheets(userFile()$datapath[1])[-1]){
          df.temp<-  readxl::read_excel(userFile()$datapath[1],feuille,col_names = T) %>%
            dplyr::mutate(Group = feuille) %>%
            tidyr::pivot_longer(
              -c(Group, FacetX, FacetY,Seuil),
              names_to = "Variable",
              values_to = "Valeur"
            )
          tbl.init <- dplyr::bind_rows(tbl.init, df.temp)
        }

        for(worksheet in userFile()$datapath[-1]){
          for(feuille in readxl::excel_sheets(worksheet)){
            df.temp2 <- readxl::read_excel(worksheet, feuille) %>%
              dplyr::mutate(Group = feuille) %>%
              tidyr::pivot_longer(
                -c(Group, FacetX, FacetY,Seuil),
                names_to = "Variable",
                values_to = "Valeur"
              )

            tbl.init <- dplyr::bind_rows(tbl.init, df.temp2)
          }
        }

        tbl.init  %>%
          dplyr::mutate(Variable = factor(Variable), FacetX = factor(FacetX), FacetY = factor(FacetY), Group = factor(Group))

      })

      return(data_loaded())
    })

    # show all time serie dataset
    output$used_data <- renderDataTable({
      req(data_for_multivariate_facets_matrix_boxplots())
      data_for_multivariate_facets_matrix_boxplots()
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
        req(data_for_multivariate_facets_matrix_boxplots())
        data_for_multivariate_facets_matrix_boxplots()
      })
    })

    # show datset summary
    observeEvent(input$showDatasetSummary, {
      req(data_for_multivariate_facets_matrix_boxplots())

      output$used_data <- renderDataTable({

        data_for_multivariate_facets_matrix_boxplots() %>%
          dplyr::group_by(FacetX, FacetY, Group, Variable) %>%
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
        data_for_MatrixFacetsMultivariateBoxplot = reactive({data_for_multivariate_facets_matrix_boxplots()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4MatrixFacetsMultivariateBoxplot_ui("data4MatrixFacetsMultivariateBoxplot_1")

## To be copied in the server
# mod_data4MatrixFacetsMultivariateBoxplot_server("data4MatrixFacetsMultivariateBoxplot_1")
