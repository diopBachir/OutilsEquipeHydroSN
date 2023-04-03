#' time_serie_interpolation_result_exportation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_time_serie_interpolation_result_exportation_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("test")),

    h4("Exporter Résultats KRIGEAGE", style="text-align:center"),
    fluidRow(align = "center",
             column(
               3, downloadButton(ns("krigeEXCEL"), label="EXCEL", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("krigeCSV"), label="CSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("krigeTSV"), label="TSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("krigeRDS"), label="RDS", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    h4("Exporter Résultats IDW", style="text-align:center"),
    fluidRow(align = "center",
             column(
               3, downloadButton(ns("idwEXCEL"), label="EXCEL", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("idwCSV"), label="CSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("idwTSV"), label="TSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("idwRDS"), label="RDS", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    h4("Exporter Résultats THIESSEN", style="text-align:center"),
    fluidRow(align = "center",
             column(
               3, downloadButton(ns("thiessenEXCEL"), label="EXCEL", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("thiessenCSV"), label="CSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("thiessenTSV"), label="TSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("thiessenRDS"), label="RDS", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    h4("Exporter Résultats Thine-Plate SPLINE", style="text-align:center"),

    fluidRow(align = "center",
             column(
               3, downloadButton(ns("splineEXCEL"), label="EXCEL", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("splineCSV"), label="CSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("splineTSV"), label="TSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("splineRDS"), label="RDS", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;")
  )
}

#' time_serie_interpolation_result_exportation Server Functions
#'
#' @noRd
mod_time_serie_interpolation_result_exportation_server <- function(
    id, resultKrigeage, resultIDW, resultSpline, resultThiessen, interpolationData
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # time serie date range for joining
    date_range<- reactive({
      req(interpolationData)
      interpolationData %>%
        dplyr::select(Date) %>%
        dplyr::mutate(Date = lubridate::ymd(Date)) %>%
        dplyr::rename(date = Date)
    })

    result_krige<- reactive({
      req(resultKrigeage)
      krige_temp<- resultKrigeage %>%
        dplyr::mutate(Date = lubridate::ymd(Date))
      date_range()  %>%
        dplyr::left_join(krige_temp, by = c("date" = "Date")) %>%
        dplyr::rename(Date = date)
    })

    result_idw<- reactive({
      req(resultIDW, date_range())
      idw_tmp<- resultIDW %>%
        dplyr::mutate(Date = lubridate::ymd(Date))
      date_range()  %>%
        dplyr::left_join(idw_tmp, by = c("date" = "Date")) %>%
        dplyr::rename(Date = date)
    })

    result_thiessen<- reactive({
      req(resultThiessen)
      thiessen_tmp<- resultThiessen %>%
        dplyr::mutate(Date = lubridate::ymd(Date))
      date_range()  %>%
        dplyr::left_join(thiessen_tmp, by = c("date" = "Date")) %>%
        dplyr::rename(Date = date)
    })

    result_spline<- reactive({
      req(resultSpline)
      spline_tmp<- resultSpline %>%
        dplyr::mutate(Date = lubridate::ymd(Date))
      date_range()  %>%
        dplyr::left_join(spline_tmp, by = c("date" = "Date")) %>%
        dplyr::rename(Date = date)
    })

    ### KRIGEAGE---------------------------------------------------------------#
    #* excel
    output$krigeEXCEL <-  downloadHandler(
      filename = function() {
        paste("Résultats-KRIGEAGE-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(result_krige(), file)
      }
    )
    # csv
    output$krigeCSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-KRIGEAGE-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv2(result_krige(), file)
      }
    )
    # tsv
    output$krigeTSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-KRIGEAGE-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        vroom::vroom_write(result_krige(), file)
      }
    )
    # rds
    output$krigeRDS <-  downloadHandler(
      filename = function() {
        paste("Résultats-KRIGEAGE-", Sys.Date(), ".RDS")
      },
      content = function(file) {
        saveRDS(result_krige(), file)
      }
    )

    ### IDW--------------------------------------------------------------------#
    #* excel
    output$idwEXCEL <-  downloadHandler(
      filename = function() {
        paste("Résultats-IDW-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(result_idw(), file)
      }
    )
    # csv
    output$idwCSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-IDW-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv2(result_idw(), file)
      }
    )
    # tsv
    output$idwTSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-IDW-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        vroom::vroom_write(result_idw(), file)
      }
    )
    # rds
    output$idwRDS <-  downloadHandler(
      filename = function() {
        paste("Résultats-IDW-", Sys.Date(), ".RDS")
      },
      content = function(file) {
        saveRDS(result_idw(), file)
      }
    )

    ### THIESSEN--------------------------------------------------------------------#
    #* excel
    output$thiessenEXCEL <-  downloadHandler(
      filename = function() {
        paste("Résultats-THIESSEN-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(result_thiessen(), file)
      }
    )
    # csv
    output$thiessenCSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-THIESSEN-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv2(result_thiessen(), file)
      }
    )
    # tsv
    output$thiessenTSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-THIESSEN-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        vroom::vroom_write(result_thiessen(), file)
      }
    )
    # rds
    output$thiessenRDS <-  downloadHandler(
      filename = function() {
        paste("Résultats-THIESSEN-", Sys.Date(), ".RDS")
      },
      content = function(file) {
        saveRDS(result_thiessen(), file)
      }
    )

    ### SPLINE--------------------------------------------------------------------#
    #* excel
    output$splineEXCEL <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPLINE-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(result_spline(), file)
      }
    )
    # csv
    output$splineCSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPLINE-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv2(result_spline(), file)
      }
    )
    # tsv
    output$splineTSV <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPLINE-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        vroom::vroom_write(result_spline(), file)
      }
    )
    # rds
    output$splineRDS <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPLINE-", Sys.Date(), ".RDS")
      },
      content = function(file) {
        saveRDS(result_spline(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_time_serie_interpolation_result_exportation_ui("time_serie_interpolation_result_exportation_1")

## To be copied in the server
# mod_time_serie_interpolation_result_exportation_server("time_serie_interpolation_result_exportation_1")
