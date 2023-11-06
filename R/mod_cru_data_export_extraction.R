#' cru_data_export_extraction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cru_data_export_extraction_ui <- function(id, cru_extracted_table){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(4,
                    dipsaus::actionButtonStyled(
                      ns("tabular_data"), span("Données Tabulaire", id=ns("tabularAnimate"), style="font-face:bold;font-family:Georgia;font-size:105%"), class= "",
                      type="primary", icon = icon("table")
                    )
             ),
             column(4,
                    dipsaus::actionButtonStyled(
                      ns("graph"), span("Visualisation Graphique", id=ns("graphAnimate"), style="font-face:bold;font-family:Georgia;font-size:105%"), class= "",
                      type="primary", icon = icon("plot")
                    )
             ),
             column(4,
                    downloadButton(ns("to_excel"), label="Exporter [Excel", icon = icon("file-excel"), class= "butt", width = "100%")
             ),
             column(12, uiOutput(ns("show_result")))
    )
  )
}

#' cru_data_export_extraction Server Functions
#'
#' @noRd
mod_cru_data_export_extraction_server <- function(id, cru_extracted_table, variable_extraite){
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

    # données
    donnees<- reactive({
      req(cru_extracted_table())
      # extraction des données pour toutes les stations
      tryCatch({
        data.frame(cru_extracted_table()) %>%
          tibble::rownames_to_column(var = "Date") %>%
          dplyr::mutate(
            Date = stringr::str_replace(Date, stringr::fixed("X"), ""),
            Date = stringr::str_replace(Date, stringr::fixed("."), "-"),
            Date = lubridate::ymd(Date),
            dplyr::across(tidyselect::where(is.numeric), ~round(.x, 4), .names = "{.col}")
          )
      }, error = function(e) {
          shinyalert("Error !", e$message, type = "error")
          return()
        }, warning = function(w) {
          shinyalert("Warning !", w$message, type = "warning")
          return()
        }
      )
    })

    # données
    donnees_cycle_saisonnier<- reactive({
      req(donnees())
      # extraction des données pour toutes les stations
      donnees() %>%
        dplyr::mutate(
          Date = lubridate::month(Date)
        ) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), ~round(mean(.x, na.rm = TRUE), 4), .names = "{.col}"))
    })

    output$show_result<- renderUI({
      fluidRow(
        column(12, div(DT::dataTableOutput(ns("resultats_tabulaires")), style = "font-size=75%"))
      )
    })

    output$resultats_tabulaires<- DT::renderDT({
      req(donnees())
      donnees()
    }, options = list(rowCallback = JS(rowCallback), pageLength = 12))

    #--------------------------------------------------------------------------------------------#

    observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$tabular_data, {
      req(donnees())

      output$show_result<- renderUI({
        fluidRow(
          column(12, div(DT::dataTableOutput(ns("resultats_tabulaires")), style = "font-size=75%"))
        )
      })

      output$resultats_tabulaires<- DT::renderDT({
        req(donnees())
        donnees()
      }, options = list(rowCallback = JS(rowCallback), pageLength = 12))

    })

    #--------------------------------------------------------------------------------------------#

    observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$graph, {
      req(donnees_cycle_saisonnier())

      output$show_result<- renderUI({
        fluidRow(
          column(12, div(plotly::plotlyOutput(ns("resultats_graphiques")), style = "font-size=75%"))
        )
      })

      output$resultats_graphiques<- plotly::renderPlotly({
        req(donnees_cycle_saisonnier())

        data_plt<-
        ## Define a blank plot with the desired layout (don't add any traces yet)
        p <- plot_ly()%>%
          layout(
            title = "Visualisation du Cycle Saisonnier des Series Chronologiques Extraites",
            xaxis = list(title = "Mois"),
            yaxis = list (title = stringr::str_to_upper(variable_extraite()))
          )

        ## Make sure our list of columns to add doesnt include the Month Considered
        ToAdd <- setdiff(colnames(donnees_cycle_saisonnier()),"Date")

        ## Add the traces one at a time
        for(i in ToAdd){
          p <- p %>% add_trace(
            x = donnees_cycle_saisonnier()[["Date"]], y = donnees_cycle_saisonnier()[[i]], name = i,
            type = 'scatter', mode = 'lines', line = list(color = sample(colors(), 1), width = 2.5)
          )
        }

        # return
        p

      })

    })

    #--------------------------------------------------------------------------------------------#

    #----------------------------------------------------------------------------------------------#
    ###  Exportation vers excel

    output$to_excel <-  downloadHandler(
      filename = function() {
        paste("Extraction-Données-NetCDF-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(donnees(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_cru_data_export_extraction_ui("cru_data_export_extraction_1")

## To be copied in the server
# mod_cru_data_export_extraction_server("cru_data_export_extraction_1")
