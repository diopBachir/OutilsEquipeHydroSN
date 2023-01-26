#' makingDailyFacetsInventoryHeatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingDailyFacetsInventoryHeatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '

            .shiny-input-container {
                color: gray;
            }

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
            .form-group { margin-bottom: 0 !important; }
            .form-label-top { margin-bottom: 0 !important; }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')),

    br(),

    fluidRow(align = "center",
             column(4, actionButtonStyled(ns("heatmap"), span("HeatMap", id=ns("boxplotAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("dailyInventoryHeatmapOut")), type=5)
  )
}

#' makingDailyFacetsInventoryHeatmap Server Functions
#'
#' @noRd
mod_makingDailyFacetsInventoryHeatmap_server <- function(id, cleanedData, dailyInventoryHeatmapOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataPLT <- reactiveVal()

    observeEvent(cleanedData, {
      req(cleanedData)
      dataPLT(cleanedData)
    })

    dailyInventoryHeatmapGraph <- eventReactive(input$heatmap, {
      req(dataPLT(), dailyInventoryHeatmapOptions)

      # pivoter le graphique
      coordFlip <- function(){
        if(dailyInventoryHeatmapOptions$coordFlip() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # plot
      dailyInventoryHeatmap <- purrr::imap(dataPLT(), ~{
        ggmissfct(.x[-1], order_missing = T) %>%
          ggplot2::ggplot(ggplot2::aes(Date, variable, fill = pct_miss)) +
          ggplot2::geom_tile(height = 1.5, width = 1.5) +
          ggplot2::ggtitle(.x$Facet[1]) +
          ggplot2::scale_x_continuous(
            breaks = seq(
              min(.x$Date), max(.x$Date), as.numeric(dailyInventoryHeatmapOptions$xAxisBreakStep())
            ),
            expand = c(0, 0)
          ) +

          ggplot2::scale_y_discrete(expand = c(0, 0)) +

          # guide_color
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              barwidth=as.numeric(dailyInventoryHeatmapOptions$barWidth()), barheight= as.numeric(dailyInventoryHeatmapOptions$barHeight()),
              title.position = dailyInventoryHeatmapOptions$barTitleLoc(), ticks.colour = dailyInventoryHeatmapOptions$barTicksColor(),
              ticks.linewidth = as.numeric(dailyInventoryHeatmapOptions$barTicksLineWidth()), title = dailyInventoryHeatmapOptions$legTitleName(),
              title.hjust = as.numeric(dailyInventoryHeatmapOptions$barTitleHjust()), title.vjust = as.numeric(dailyInventoryHeatmapOptions$barTitleVjust()),
              title.theme = ggplot2::element_text(
                family = "Times", size=as.numeric(dailyInventoryHeatmapOptions$barTitleSize()), color = "black",
                margin = ggplot2::margin(
                  b=as.numeric(dailyInventoryHeatmapOptions$barTitleMarginB()), l=as.numeric(dailyInventoryHeatmapOptions$barTitleMarginL()),
                  t=as.numeric(dailyInventoryHeatmapOptions$barTitleMarginT()), r=as.numeric(dailyInventoryHeatmapOptions$barTitleMarginR()),
                  unit = "cm"
                )
              ),
              frame.colour = "black"
            )
          ) +

          ggplot2::scale_fill_distiller(
            palette = "Spectral",
            direction = -1,
            breaks = seq(0, 100, 10),
            labels = seq(0, 100, 10)
          ) +

          ggplot2::scale_color_distiller(
            palette = "Spectral",
            direction = -1,
            breaks = seq(0, 100, 10),
            labels = seq(0, 100, 10)
          ) +

          ggplot2::theme_bw()+
          # personnalisation des éléments du graphique
          ggplot2::theme(
            # axes
            axis.text.x = ggplot2::element_text(
              family = "Times", size=as.numeric(dailyInventoryHeatmapOptions$xAxisTextSize()),
              color = dailyInventoryHeatmapOptions$axisTextColor(),
              angle=as.numeric(dailyInventoryHeatmapOptions$xAxisTextAngle()),
              hjust = as.numeric(dailyInventoryHeatmapOptions$xAxisTextHjust()),
              vjust = as.numeric(dailyInventoryHeatmapOptions$xAxisTextVjust()),
              margin = ggplot2::margin(t = .1, unit = "cm")
            ),
            axis.ticks = ggplot2::element_line(
              colour=dailyInventoryHeatmapOptions$axisTicksColor(),
              size=as.numeric(dailyInventoryHeatmapOptions$axisTicksSize())
            ),
            axis.title = ggplot2::element_blank(),
            # les grilles
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # texte de l'axe des y
            axis.text.y= ggplot2::element_text(
              family = "Times", color =  dailyInventoryHeatmapOptions$axisTextColor(),
              size=as.numeric(dailyInventoryHeatmapOptions$yAxisTextSize())
            ),
            # légende
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.margin = ggplot2::margin(
              b=as.numeric(dailyInventoryHeatmapOptions$legMarginB()),
              l=as.numeric(dailyInventoryHeatmapOptions$legMarginL()),
              t=as.numeric(dailyInventoryHeatmapOptions$legMarginT()),
              r=as.numeric(dailyInventoryHeatmapOptions$legMarginR()),
              unit = "cm"
            ),
            legend.text = ggplot2::element_text(
              family = "Times", size=as.numeric(dailyInventoryHeatmapOptions$LegTextSize()),
              color =  dailyInventoryHeatmapOptions$LegTextColor(),
              margin = ggplot2::margin(
                b=as.numeric(dailyInventoryHeatmapOptions$legTextMarginB()),
                l=as.numeric(dailyInventoryHeatmapOptions$legTextMarginL()),
                t=as.numeric(dailyInventoryHeatmapOptions$legTextMarginT()),
                r=as.numeric(dailyInventoryHeatmapOptions$legTextMarginR()),
                unit = "cm"
              )
            ),

            plot.title = ggplot2::element_text(
              hjust = as.numeric(dailyInventoryHeatmapOptions$plotTitleAlign()),
              size = as.numeric(dailyInventoryHeatmapOptions$plotTitleSize()),
              face = "bold", margin = ggplot2::margin(b=.1, unit = "cm")
            ),

            plot.margin = ggplot2::margin(c(.1, .1, .1, .1), unit = "cm"),
            plot.background = ggplot2::element_rect(fill="white", color = "white", size=.0001),
            # contours
            panel.border = ggplot2::element_rect(
              color=dailyInventoryHeatmapOptions$panelBackgroundLineColor(), fill=NA,
              size=as.numeric(dailyInventoryHeatmapOptions$panelBackgroundLineSize())
            )
          )
      })

      return(ggpubr::ggarrange(plotlist=dailyInventoryHeatmap, nrow = as.numeric(dailyInventoryHeatmapOptions$facetNrow()), legend = "bottom", common.legend = T))
    })

    output$dailyInventoryHeatmapOut <- renderPlot({
      req(dailyInventoryHeatmapGraph())
      dailyInventoryHeatmapGraph()
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("DailyInventoryHeatmapWithFacets-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, dailyInventoryHeatmapGraph(), width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("DailyInventoryHeatmapWithFacets-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, dailyInventoryHeatmapGraph(), width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_makingDailyFacetsInventoryHeatmap_ui("makingDailyFacetsInventoryHeatmap_1")

## To be copied in the server
# mod_makingDailyFacetsInventoryHeatmap_server("makingDailyFacetsInventoryHeatmap_1")
