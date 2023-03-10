#' makingAnnualFacetsInventoryHeatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingAnnualFacetsInventoryHeatmap_ui <- function(id, label){
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

    fluidRow(align = "center",
             column(4, actionButtonStyled(ns("heatmap"), span("HeatMap", id=ns("boxplotAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("annualInventoryHeatmapOut")), type=5),
  )
}

#' makingAnnualFacetsInventoryHeatmap Server Functions
#'
#' @noRd
mod_makingAnnualFacetsInventoryHeatmap_server <- function(id, cleanedData, annualInventoryHeatmapOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataPLT <- reactiveVal()

    observeEvent(cleanedData, {
      req(cleanedData)
      dataPLT(cleanedData)
    })

    annualInventoryHeatmapGraph <- eventReactive(input$heatmap, {
      req(dataPLT(), annualInventoryHeatmapOptions)

      # plot
      annualInventoryHeatmap <-
        purrr::imap(dataPLT(), ~{

          ggmissfct_annual(.x) %>%

            dplyr::filter(Variable != "Date") %>%

            ggplot2::ggplot(aes(x=Date, y=Variable, fill=factor(NAbool))) +
            ggplot2::geom_tile(height = 1.5, width = 1.5)+

            ggplot2::ggtitle(.x$Facet[1]) +

            ggplot2::scale_x_continuous(
              breaks = seq(
                min(.x$Date), max(.x$Date), as.numeric(annualInventoryHeatmapOptions$xAxisBreakStep())
              ),
              expand = c(0, 0)
            ) +

            ggplot2::scale_fill_manual(values=c("black", "gray"), name="", labels = c("Lacunes","Observations")) +
            ggplot2::labs(
              x = NA, y = NA
            ) +

            ggplot2::scale_y_discrete(expand = c(0, 0)) +

            ggplot2::theme_bw()+
            # personnalisation des éléments du graphique
            ggplot2::theme(
              # axes
              axis.text.x = ggplot2::element_text(
                family = "Times", size=as.numeric(annualInventoryHeatmapOptions$xAxisTextSize()),
                color = annualInventoryHeatmapOptions$axisTextColor(),
                angle=as.numeric(annualInventoryHeatmapOptions$xAxisTextAngle()),
                hjust = as.numeric(annualInventoryHeatmapOptions$xAxisTextHjust()),
                vjust = as.numeric(annualInventoryHeatmapOptions$xAxisTextVjust()),
                margin = ggplot2::margin(t = .1, unit = "cm")
              ),
              axis.ticks = ggplot2::element_line(
                colour=annualInventoryHeatmapOptions$axisTicksColor(),
                size=as.numeric(annualInventoryHeatmapOptions$axisTicksSize())
              ),
              axis.title = ggplot2::element_blank(),
              # les grilles
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              # texte de l'axe des y
              axis.text.y= ggplot2::element_text(
                family = "Times", color =  annualInventoryHeatmapOptions$axisTextColor(),
                size=as.numeric(annualInventoryHeatmapOptions$yAxisTextSize())
              ),
              # légende
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.margin = ggplot2::margin(
                b=as.numeric(annualInventoryHeatmapOptions$legMarginB()),
                l=as.numeric(annualInventoryHeatmapOptions$legMarginL()),
                t=as.numeric(annualInventoryHeatmapOptions$legMarginT()),
                r=as.numeric(annualInventoryHeatmapOptions$legMarginR()),
                unit = "cm"
              ),
              legend.text = ggplot2::element_text(
                family = "Times", size=as.numeric(annualInventoryHeatmapOptions$LegTextSize()),
                color =  annualInventoryHeatmapOptions$LegTextColor(),
                margin = ggplot2::margin(
                  b=as.numeric(annualInventoryHeatmapOptions$legTextMarginB()),
                  l=as.numeric(annualInventoryHeatmapOptions$legTextMarginL()),
                  t=as.numeric(annualInventoryHeatmapOptions$legTextMarginT()),
                  r=as.numeric(annualInventoryHeatmapOptions$legTextMarginR()),
                  unit = "cm"
                )
              ),

              plot.title = ggplot2::element_text(
                hjust = as.numeric(annualInventoryHeatmapOptions$plotTitleAlign()),
                size = as.numeric(annualInventoryHeatmapOptions$plotTitleSize()),
                face = "bold", margin = ggplot2::margin(b=.1, unit = "cm")
              ),

              plot.margin = ggplot2::margin(c(.1, .1, .1, .1), unit = "cm"),
              plot.background = ggplot2::element_rect(fill="white", color = "white", size=.0001),
              # contours
              panel.border = ggplot2::element_rect(
                color=annualInventoryHeatmapOptions$panelBackgroundLineColor(), fill=NA,
                size=as.numeric(annualInventoryHeatmapOptions$panelBackgroundLineSize())
              )
            )
        })


      return(
        ggpubr::ggarrange(
          plotlist=annualInventoryHeatmap, legend = "bottom", common.legend = T,
          nrow = as.numeric(annualInventoryHeatmapOptions$facetNrow())
        )
      )
    })

    output$annualInventoryHeatmapOut <- renderPlot({
      req(annualInventoryHeatmapGraph())
      annualInventoryHeatmapGraph()
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("AnnualInventoryHeatmapWithFacets-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, annualInventoryHeatmapGraph(), width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("AnnualInventoryHeatmapWithFacets-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, annualInventoryHeatmapGraph(), width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_makingAnnualFacetsInventoryHeatmap_ui("makingAnnualFacetsInventoryHeatmap_1")

## To be copied in the server
# mod_makingAnnualFacetsInventoryHeatmap_server("makingAnnualFacetsInventoryHeatmap_1")
