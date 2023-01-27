#' spiBarPlotGraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spiBarPlotGraph_ui <- function(id){
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

    fluidRow(align="center",
             column(
               4, actionButton(ns("spiBarplot"), span("SPI-BARPLOT", id=ns("spiBarplotAnimate")), icon = icon("map"))
             ),
             column(
               4, downloadButton(ns("jpegOutput"), label="JPEG", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               4, downloadButton(ns("svgOutput"), label="SVG", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    plotOutput(ns("spi_heatmap"))
    # verbatimTextOutput(ns("test"))
  )
}

#' spiBarPlotGraph Server Functions
#'
#' @noRd
mod_spiBarPlotGraph_server <- function(id, spi_result, spiBarPlotOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    spi_plot_result<- reactiveVal()

    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$spiBarplot, {
      req(spi_result)

      # setting buttons with shinyjs
      shinyjs::addClass(id = "spiBarplotAnimate", class = "loading dots")
      shinyjs::disable("spiBarplot")
#
#       #------------------------------------------------------------------------------#
#       # Le graphique SPI
      dataPLT<- spi_result %>%
        dplyr::mutate(
          Date = as.numeric(Date),
          signe = case_when(SPI > 0 ~ "positif", SPI < 0 ~ "negatif", TRUE ~ "neutre")
        )

      plotSPI_barplot<- function(){

        dataPLT %>%
          dplyr::mutate(
            signe = case_when(SPI > 0 ~ "positif", SPI < 0 ~ "negatif", TRUE ~ "neutre")
          )  %>%
          ggplot2::ggplot(ggplot2::aes(x = Date, y = SPI)) +
          ggplot2::geom_bar(
            stat = "identity", show.legend = F, position = "dodge",
            color="black", fill="black", width = spiBarPlotOptions$barWidth()
          ) +
          ggplot2::geom_hline(ggplot2::aes(yintercept=0), size=.5, color="black") +
          # ggplot2::scale_fill_manual(values = c("red", "black")) +

          # ggplot2::scale_color_manual(values = c("red", "black")) +

          ggplot2::scale_x_continuous(
            limits = c(min(spi_result$Date), max(spi_result$Date)),
            breaks = seq(
              min(spi_result$Date), max(spi_result$Date), as.numeric(spiBarPlotOptions$xAxisBreakStep())
            ), expand = c(.01, .01)
          ) +

          ggplot2::scale_y_continuous(
            breaks = round(seq(min(spi_result$SPI), max(spi_result$SPI), 1)+.5),
            labels = round(seq(min(spi_result$SPI), max(spi_result$SPI), 1)+.5),
            expand = c(.015, .015)
          ) +

          ggplot2::theme_bw() +

          ggplot2:: theme(
            axis.text.x = ggplot2::element_text(
              family = "Times", size=as.numeric(spiBarPlotOptions$xAxisTextSize()), color = spiBarPlotOptions$axisTextColor(),
              hjust = as.numeric(spiBarPlotOptions$xAxisTextHjust()), vjust = as.numeric(spiBarPlotOptions$xAxisTextVjust()),
              angle = as.numeric(spiBarPlotOptions$xAxisTextAngle()), margin = ggplot2::margin(t = .1, unit = "cm")
            ),
            axis.text.y = ggplot2::element_text(
              family = "Times", size=as.numeric(spiBarPlotOptions$yAxisTextSize())
            ),
            axis.title.y = ggplot2::element_text(
              family = "Times", size=12, face = "bold"
            ),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_line(
              size = as.numeric(spiBarPlotOptions$axisTicksSize()), color = spiBarPlotOptions$axisTicksColor()
            ),
            axis.ticks.length=grid::unit(.15, "cm"),

            panel.grid.major.y = ggplot2::element_line(linewidth = .4, color = "gray", linetype = "12"),
            panel.grid.minor.y = ggplot2::element_line(linewidth = .4, color = "gray", linetype = "12"),
            panel.grid.major.x = ggplot2::element_line(linewidth = .4, color = "gray", linetype = "12"),
            panel.grid.minor.x = ggplot2::element_line(linewidth = .4, color = "gray", linetype = "12"),

            plot.background = ggplot2::element_rect(
              color="white", fill="white", size=.1
            ),
            panel.border = ggplot2::element_rect(
              color=spiBarPlotOptions$panelBackgroundLineColor(), fill=NA,
              size=as.numeric(spiBarPlotOptions$panelBackgroundLineSize())
            ),
            plot.margin = ggplot2::margin(c(.3, .3, .3, .3), unit = "cm")
          ) +

          ggplot2::labs(x=NULL, y="SPI")

      }

      spi_plot_result(plotSPI_barplot())

      # return(plotSPI_barplot())
      output$spi_heatmap<- renderPlot({
        req(spi_plot_result())
        spi_plot_result()
      })

      shinyjs::enable("spiBarplot")
      shinyjs::removeClass(id = "spiBarplotAnimate", class = "loading dots")

    })



    ### Exporting result---------------------------------------------------------------#
    #* jpeg
    output$jpegOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-BarPlot",stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = spi_plot_result(), filename = file,width = 13.3, height = 7.05)
      }
    )
    # csv
    output$svgOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-BarPlot", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = spi_plot_result(), filename = file, width = 13.3, height = 7.05)
      }
    )
  })
}

## To be copied in the UI
# mod_spiBarPlotGraph_ui("spiBarPlotGraph_1")

## To be copied in the server
# mod_spiBarPlotGraph_server("spiBarPlotGraph_1")
