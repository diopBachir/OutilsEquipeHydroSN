#' data4MultivariateBoxplotFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data4MultivariateBoxplotFile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="center",
             column(6,
                    fileInput(
                      ns("dataInput"), label = "",
                      # label = div("Données", style = "color:#gray;family:Georgia;font-size:120%"),
                      accept = c(".xlsx", ".xls"),  buttonLabel = "Charger...",
                      placeholder = "data.xlsx|xls"
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

#' data4MultivariateBoxplotFile Server Functions
#'
#' @noRd
mod_data4MultivariateBoxplotFile_server <- function(id){
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
      tools::file_ext(userFile()$datapath) %in% c("xlsx", "xls")
    })

    # loadingData
    data_for_multivariate_boxplots<- reactive({

      if(!extension_fichier()){
        shinyFeedback::feedbackWarning(
          "dataInput", !extension_fichier(),
          paste0(
            "Veillez choisir un fichier {.xlsx|xls} !"
          )
        )
        # shinyalert
        if(!extension_fichier()){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez choisir un fichier avec l'extension {.xlsx} ou {xlsx} !"
            )
          )
        }
      }


      # load data
      data_loaded<-reactive({
        req(extension_fichier())

        testSheet <- length(excel_sheets(userFile()$datapath)) > 1
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
          for(feuille in excel_sheets(userFile()$datapath)){
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

        req(!(testEmptySheet()==TRUE))
        tbl.init <- readxl::read_excel(userFile()$datapath, sheet = readxl::excel_sheets(userFile()$datapath)[1]) %>%
          dplyr::mutate(Group = readxl::excel_sheets(userFile()$datapath)[1]) %>%
          tidyr::pivot_longer(
            1:ncol(readxl::read_excel(userFile()$datapath, sheet = readxl::excel_sheets(userFile()$datapath)[1])),
            names_to = "Variable",
            values_to = "Valeur"
          )

        for (feuille in readxl::excel_sheets(userFile()$datapath)[-1]) {
          tbl.temp <- readxl::read_excel(userFile()$datapath, sheet = feuille) %>%
            dplyr::mutate(Group = feuille) %>%
            tidyr::pivot_longer(
              1:ncol(readxl::read_excel(userFile()$datapath, sheet = feuille)),
              names_to = "Variable",
              values_to = "Valeur"
            )

          tbl.init <- dplyr::bind_rows(tbl.init, tbl.temp)
        }

        tbl.init
      })

      return(data_loaded())
    })

    # show all time serie dataset
    output$used_data <- renderDataTable({
      req(data_for_multivariate_boxplots())
      data_for_multivariate_boxplots()
    })

    observeEvent(input$showDataset, {
      req(data_for_multivariate_boxplots())

      shinyFeedback::hideFeedback("dataInput")

      # Notification
      id <- showNotification(
        "Traitement ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      output$used_data <- renderDataTable({
        req(data_for_multivariate_boxplots())
        data_for_multivariate_boxplots()
      })

    })

    # show datset summary
    observeEvent(input$showDatasetSummary, {
      req(data_for_multivariate_boxplots())

      output$used_data <- renderDataTable({
        req(data_for_multivariate_boxplots())
        data_for_multivariate_boxplots() %>%
          dplyr::select(-Group) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Min. = min(Valeur, na.rm = T), Quart1 = quantile(Valeur, .25),
            Médianne = median(Valeur), Quart3 = quantile(Valeur, .75),
            Moyenne = mean(Valeur), Max = max(Valeur),
            "Ecart Group" = sd(Valeur)
          )  %>%
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )
      })

    })

    #return
    return(
      list(
        data_for_multivariateBoxplot = reactive({data_for_multivariate_boxplots()})
      )
    )

  })
}

## To be copied in the UI
# mod_data4MultivariateBoxplotFile_ui("data4MultivariateBoxplotFile_1")

## To be copied in the server
# mod_data4MultivariateBoxplotFile_server("data4MultivariateBoxplotFile_1")
