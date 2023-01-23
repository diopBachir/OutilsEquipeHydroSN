#' makingMonthlyBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingMonthlyBoxplot_ui <- function(id, label){
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
             column(4, actionButtonStyled(ns("boxplot"), span("Boxplots", id=ns("boxplotAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("monthlyBoxplot")), type=5),
    # stats
    div(dataTableOutput(ns("statsSummary")), style="font-size:85%")
  )
}

#' makingMonthlyBoxplot Server Functions
#'
#' @noRd
mod_makingMonthlyBoxplot_server <- function(id, cleanedData, monthlyBoxplotOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataPLT <- reactiveVal()

    observeEvent(cleanedData, {
      req(cleanedData)
      dataPLT(cleanedData)
    })


    monthlyBoxplotGraph <- eventReactive(input$boxplot, {
      req(dataPLT(), monthlyBoxplotOptions)

      # pivoter le graphique
      coordFlip <- function(){
        if(monthlyBoxplotOptions$coordFlip() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # plot
      monthlyBplt <-
        ggplot2::ggplot(
          data=dataPLT(),
          aes(x=stats::reorder(variable, valeur), y=valeur),
        )+
        # barres d'erreur
        ggplot2::stat_boxplot(geom = "errorbar")+
        # boites à moustaches
        ggplot2::geom_boxplot(
          outlier.shape = as.integer(monthlyBoxplotOptions$outlierShape()), outlier.alpha = 1,
          outlier.color = monthlyBoxplotOptions$outlierColor(), outlier.size = as.numeric(monthlyBoxplotOptions$outlierSize()),
          color = monthlyBoxplotOptions$boxplotColor(), fill = monthlyBoxplotOptions$boxplotFill(),
          alpha =  as.numeric(monthlyBoxplotOptions$boxplotAlpha()), width = as.numeric(monthlyBoxplotOptions$boxplotsWidth())
        ) +
        # graduation de l'axe des ordonnées
        ggplot2::scale_y_continuous(
          limits = c(min(dataPLT()$valeur), max(dataPLT()$valeur)),
          breaks = seq(min(dataPLT()$valeur), max(dataPLT()$valeur), length.out=as.numeric(monthlyBoxplotOptions$yAxisNbreaks())),
          labels = round(seq(min(dataPLT()$valeur), max(dataPLT()$valeur), length.out=as.numeric(monthlyBoxplotOptions$yAxisNbreaks())), 2)
        )+

        coordFlip() +

        # personnalisation des éléments du graphique
        ggplot2::theme(
          # axes
          axis.text.x = ggplot2::element_text(
            family = "Times", size=as.numeric(monthlyBoxplotOptions$xAxisTextSize()),
            color = monthlyBoxplotOptions$axisTextColor(),
            angle=as.numeric(monthlyBoxplotOptions$xAxisTextAngle()),
            hjust = as.numeric(monthlyBoxplotOptions$xAxisTextHjust()),
            vjust = as.numeric(monthlyBoxplotOptions$xAxisTextVjust())
          ),
          axis.ticks = ggplot2::element_line(
            colour=monthlyBoxplotOptions$axisTicksSize(), size=as.numeric(monthlyBoxplotOptions$axisTicksSize())
          ),
          axis.title.y = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(monthlyBoxplotOptions$yAxisTitleSize())
          ),
          # les grilles
          panel.grid.major = ggplot2::element_line(
            linetype = ifelse(length(monthlyBoxplotOptions$panelGridLineType()),
                              as.numeric(monthlyBoxplotOptions$panelGridLineType()),
                              monthlyBoxplotOptions$panelGridLineType()),
            color = monthlyBoxplotOptions$panelGridColor(), size=grid::unit(.23, "cm")
          ),
          panel.grid.minor = ggplot2::element_blank(),
          # texte de l'axe des y
          axis.text.y= ggplot2::element_text(
            family = "Times", size=as.numeric(monthlyBoxplotOptions$yAxisTextSize()), color =  monthlyBoxplotOptions$axisTextColor()
          ),
          # contours
          panel.background = ggplot2::element_rect(
            color=monthlyBoxplotOptions$panelBackgroundLineColor(), fill="white", size=as.numeric(monthlyBoxplotOptions$panelBackgroundLineSize())
          ),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(
            family = "Times", size = as.numeric(monthlyBoxplotOptions$stripTextSize()),
            margin = ggplot2::margin(b = as.numeric(monthlyBoxplotOptions$stripMarginBottom()), unit = "pt")
          ),
          panel.spacing.y = grid::unit(as.numeric(monthlyBoxplotOptions$panelYspacing()), "cm"),
          panel.spacing.x = grid::unit(as.numeric(monthlyBoxplotOptions$panelXspacing()), "cm"),
        )+

        ggplot2::labs(
          x = NULL, y = monthlyBoxplotOptions$yAxisTitle()
        ) +

        ggplot2::facet_wrap(vars(mois), nrow = as.numeric(monthlyBoxplotOptions$facetsNrow()))

      # résummé statistique
      statSum <- dataPLT() %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          Min. = round(min(valeur, na.rm = T), 2), Quart1 = round(quantile(valeur, .25), 2),
          Médianne = round(median(valeur), 2), Quart3 = round(quantile(valeur, .75), 2),
          Moyenne = round(mean(valeur), 2), Max = round(max(valeur), 2),
          "Ecart type" = round(sd(valeur), 2)
        )

      return(list(monthlyBplt, statSum))
    })

    output$monthlyBoxplot <- renderPlot({
      req(monthlyBoxplotGraph())
      monthlyBoxplotGraph()[[1]]
    })

    output$statsSummary <- renderDataTable({
      req(monthlyBoxplotGraph())
      monthlyBoxplotGraph()[[2]]
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("BoxplotsMensuels-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, monthlyBoxplotGraph()[[1]], width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("BoxplotsMensuels-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, monthlyBoxplotGraph()[[1]], width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_makingMonthlyBoxplot_ui("makingMonthlyBoxplot_1")

## To be copied in the server
# mod_makingMonthlyBoxplot_server("makingMonthlyBoxplot_1")
