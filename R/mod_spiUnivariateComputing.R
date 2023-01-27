#' spiUnivariateComputing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spiUnivariateComputing_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
                margin-top: -0.3em;
                line-height: 1.5em;
                vertical-align: text-bottom;
                box-sizing: border-box;
            }
            .loading.dots::after {
                text-rendering: geometricPrecision;
                content: "⠋\\A⠙\\A⠹\\A⠸\\A⠼\\A⠴\\A⠦\\A⠧\\A⠇\\A⠏";
                animation: spin10 1s steps(10) infinite;
                animation-duration: 1s;
                animation-timing-function: steps(10);
                animation-delay: 0s;
                animation-iteration-count: infinite;
                animation-direction: normal;
                animation-fill-mode: none;
                animation-play-state: running;
                animation-name: spin10;
            }
            .loading::after {
                display: inline-table;
                white-space: pre;
                text-align: left;
            }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')),

    br(),
    fluidRow(align="center",
             column(
               3, actionButton(ns("spi"), span("SPI", id=ns("spiAnimate")), icon = icon("table"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("excelOutput"), label="EXCEL", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("csvOutput"), label="CSV", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("tsvOutput"), label="TSV", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    DT::dataTableOutput(ns("spi_result"))
  )
}

#' spiUnivariateComputing Server Functions
#'
#' @noRd
mod_spiUnivariateComputing_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # calcul du SPI
    spiResult <- reactiveVal()

    observeEvent(input$spi, {
      req(data)

      # setting buttons with shinyjs
      shinyjs::addClass(id = "spiAnimate", class = "loading dots")
      shinyjs::disable("spi")

      #------------------------------------------------------------------------------#
      ## Calcul du SPI
      spi<- data %>%
        dplyr::mutate(
          dplyr::across(
            -Date, ~round((.x - mean(.x, na.rm=T))/sd(.x, na.rm=T), 4), .names = "{.col}"
          )
        ) %>%
        dplyr::rename(SPI = 2)

      req(spi)
      spiResult(spi)
      #------------------------------------------------------------------------------#
      # rendering
      output$spi_result<- DT::renderDataTable({
        req(spi)
        spi
      }, options = list(pageLength=10, align="right"))

      shinyjs::enable("spi")
      shinyjs::removeClass(id = "spiAnimate", class = "loading dots")

    })

    ### Exporting result---------------------------------------------------------------#
    #* excel
    output$excelOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(spiResult(), file)
      }
    )
    # csv
    output$csvOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".csv")
      },
      content = function(file) {
        vroom::vroom_write(spiResult(), file, delim = ";")
      }
    )
    # tsv
    output$tsvOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".tsv")
      },
      content = function(file) {
        vroom::vroom_write(spiResult(), file)
      }
    )

    # return
    return(list(spi_result_cleaned = reactive(spiResult())))
  })
}

## To be copied in the UI
# mod_spiUnivariateComputing_ui("spiUnivariateComputing_1")

## To be copied in the server
# mod_spiUnivariateComputing_server("spiUnivariateComputing_1")
