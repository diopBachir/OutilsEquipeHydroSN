#' computingDailyET0 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_computingDailyET0_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align="center",
             column(6,
                    div("Estimation des Paramètres Météorologiques", style = "color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("paramEstim"), label="Estimer",
                      icon = icon("microchip"), width="100%"
                    )
             ),
             column(6,
                    div("Calcul de l'ETP Journalière [21 Méthodes]", style = "color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("et0camputing"), label="Calculer",
                      icon = icon("microchip"), width="100%"
                    )
             )
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align="center",
             column(4,
                    div("Valeurs Journalières", style = "font-face:bold;color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("dailyEToExcel"), label="Exporter",
                      icon = icon("file-excel")
                    )
             ),
             column(4,
                    div("Cumuls Mensuels", style = "font-face:bold;color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("monthlyEToExcel"), label="Exporter",
                      icon = icon("file-excel")
                    )
             ),
             column(4,
                    div("Cumuls Annuels", style = "color:#gray;family:Georgia;font-size:110%"),
                    actionButton(
                      ns("annualEToExcel"), label="Exporter",
                      icon = icon("file-excel")
                    )
             ),
    ),

    tags$hr(style="border-color:gray;"),

    verbatimTextOutput(ns("verboseProcessing"))
  )
}

#' computingDailyET0 Server Functions
#'
#' @noRd
mod_computingDailyET0_server <- function(id, filesPathList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    loadedData <- reactiveVal()
    ET0_result <- reactiveVal()

    # estimation des paramètres
    observeEvent(ignoreInit = T, ignoreNULL = T, input$paramEstim, {
      req(filesPathList)

      # # initiation
      init.table <- EstParamMeteoET0(import_data(filesPathList$datapath[1]))[0,]
      # estimation des paramètres
      for(i in 1:length(filesPathList$datapath)){

        temp.estimation.table <- EstParamMeteoET0(import_data(filesPathList$datapath[i]))

        # binding table
        init.table <- dplyr::bind_rows(init.table, temp.estimation.table)

        # rendering
        output$verboseProcessing  <-  renderPrint({
          req(init.table)
          paste0("Estimation Des Paramètres Météo ::: Station : ", unique(init.table$nom), "::: ----------------------- OK ! ")
        })

        # return
        loadedData(init.table)
      }

      # return
      return(
        list(
          data_binded_for_dailyET0computing = reactive({loadedData()})
        )
      )

    })

    # calcul de l'ET0
    observeEvent(ignoreInit = T, ignoreNULL = T, input$et0camputing, {
      req(loadedData())
      eto_result<-  ET0(loadedData())

      output$verboseProcessing  <-  renderPrint({
        req(eto_result)
        paste0("Calcul ET0 ::: Station : ", unique(eto_result$nom), "::: -------------------- OK! ")
      })

      ET0_result(eto_result)
    })

    # Données journalière vers excel
    observeEvent(ignoreInit=T, ignoreNULL=T, input$dailyEToExcel, {
      req(ET0_result())
      # exportation
      init.df.list <- list()
      for(station in unique(ET0_result()$nom)){

        # extract station
        temp.df <- ET0_result() %>%
          dplyr::filter(nom == station) %>%
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric), ~round(.x, 3), .names = "{.col}")
          )

        # add extracted table to list
        init.df.list[[station]] <- temp.df

        # rendering
        output$verboseProcessing  <-  renderPrint({
          req(init.df.list)
          paste0("Envoi ET0 Journalière vers Excel ::: Station : ", names(init.df.list), "::: ----------------------- OK ! ")
        })

      }

      ### Envoyer vers Excel
      show_in_excel(init.df.list)

    })

    # Cumul Mensuel vers excel
    observeEvent(ignoreInit=T, ignoreNULL=T, input$monthlyEToExcel, {
      req(ET0_result())
      # exportation
      init.df.list <- list()
      for(station in unique(ET0_result()$nom)){

        # extract station
        temp.df <- ET0_result() %>%
          dplyr::filter(nom == station) %>%
          dplyr::mutate(
            date = lubridate::ymd(date), annee = lubridate::year(date), mois = lubridate::month(date)
          )  %>%
          dplyr::select(nom, annee, mois, tidyselect::starts_with("ETo"))  %>%
          dplyr::group_by(nom, annee, mois) %>%
          dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum, .names = "{.col}"))

        # add extracted table to list
        init.df.list[[station]] <- temp.df

        # rendering
        output$verboseProcessing  <-  renderPrint({
          req(init.df.list)
          paste0("Envoi Cumul Mensuel ETO vers Excel ::: Station : ", names(init.df.list), "::: ----------------------- OK ! ")
        })

      }

      ### Envoyer vers Excel
      show_in_excel(init.df.list)

    })

    # Cumul Annuel vers excel
    observeEvent(ignoreInit=T, ignoreNULL=T, input$annualEToExcel, {
      req(ET0_result())
      # exportation
      init.df.list <- list()
      for(station in unique(ET0_result()$nom)){

        # extract station
        temp.df <- ET0_result() %>%
          dplyr::filter(nom == station) %>%
          dplyr::mutate(
            date = lubridate::ymd(date), annee = lubridate::year(date)
          )  %>%
          dplyr::select(nom, annee, tidyselect::starts_with("ETo"))  %>%
          dplyr::group_by(nom, annee) %>%
          dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum, .names = "{.col}"))

        # add extracted table to list
        init.df.list[[station]] <- temp.df

        # rendering
        output$verboseProcessing  <-  renderPrint({
          req(init.df.list)
          paste0("Envoi Cumul Annuel ETO vers Excel ::: Station : ", names(init.df.list), "::: ----------------------- OK ! ")
        })

      }

      ### Envoyer vers Excel
      show_in_excel(init.df.list)

    })

    # return
    return(
      eto_data_list <- list(
        dailyET0_table = reactive({ET0_result()}),
        monthlyET0_table = reactive({
          ET0_result() %>%
            dplyr::filter(nom == station) %>%
            dplyr::mutate(
              date = lubridate::ymd(date), annee = lubridate::year(date), mois = lubridate::month(date)
            )  %>%
            dplyr::select(nom, annee, mois, tidyselect::starts_with("ETo"))  %>%
            dplyr::group_by(nom, annee, mois) %>%
            dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum, .names = "{.col}"))
        }),
        annualET0_table = reactive({
          ET0_result() %>%
            dplyr::filter(nom == station) %>%
            dplyr::mutate(
              date = lubridate::ymd(date), annee = lubridate::year(date)
            )  %>%
            dplyr::select(nom, annee, tidyselect::starts_with("ETo"))  %>%
            dplyr::group_by(nom, annee) %>%
            dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum, .names = "{.col}"))
        })
      )
    )

  })
}

## To be copied in the UI
# mod_computingDailyET0_ui("computingDailyET0_1")

## To be copied in the server
# mod_computingDailyET0_server("computingDailyET0_1")
