#' makingUnivariateBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingUnivariateBoxplot_ui <- function(id){
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
             column(4, actionButtonStyled(ns("boxplot"), span("Boxplots", id=ns("boxplotAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(12, sliderInput(ns("yAxisRounding"), label="Nbre de Décimales dans l'axe Y", min = 0, max = 6, value = 0))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("univariateBoxplot")), type=5),
    # stats
    div(dataTableOutput(ns("statsSummary")), style="font-size:85%")
  )
}

#' makingUnivariateBoxplot Server Functions
#'
#' @noRd
mod_makingUnivariateBoxplot_server <- function(id, cleanedData, univariateBoxplotOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataPLT_univar <- reactiveVal()

    observeEvent(cleanedData, {
      req(cleanedData)
      dataPLT_univar(cleanedData)
    })

    univariateBoxplot <- eventReactive(ignoreInit=T, ignoreNULL=T, input$boxplot, {
      req(dataPLT_univar(), univariateBoxplotOptions)

      # pivoter le graphique
      coordFlip <- function(){
        if(univariateBoxplotOptions$coordFlip() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # plot
      univariateBplt <-
        ggplot2::ggplot(
          data=dataPLT_univar(),
          aes(x=variable, y=valeur),
        )+
        # barres d'erreur
        ggplot2::stat_boxplot(geom = "errorbar")+
        # boites à moustaches
        ggplot2::geom_boxplot(
          outlier.shape = as.integer(univariateBoxplotOptions$outlierShape()), outlier.alpha = 1,
          outlier.color = univariateBoxplotOptions$outlierColor(), outlier.size = as.numeric(univariateBoxplotOptions$outlierSize()),
          color = univariateBoxplotOptions$boxplotColor(), fill = univariateBoxplotOptions$boxplotFill(),
          alpha =  as.numeric(univariateBoxplotOptions$boxplotAlpha()), width = as.numeric(univariateBoxplotOptions$boxplotsWidth())
        )+
        # graduation de l'axe des ordonnées [0, Pmm max., pas de 200mm]
        ggplot2::scale_y_continuous(
          limits = c(min(dataPLT_univar()$valeur), max(dataPLT_univar()$valeur)),
          breaks = seq(min(dataPLT_univar()$valeur), max(dataPLT_univar()$valeur), length.out=as.numeric(univariateBoxplotOptions$yAxisNbreaks())),
          labels = round(seq(min(dataPLT_univar()$valeur), max(dataPLT_univar()$valeur), length.out=as.numeric(univariateBoxplotOptions$yAxisNbreaks())), input$yAxisRounding)
        )+

        coordFlip() +

        # personnalisation des éléments du graphique
        ggplot2::theme(
          # axes
          axis.text.x = ggplot2::element_text(
            family = "Times", size=as.numeric(univariateBoxplotOptions$xAxisTextSize()),
            color = univariateBoxplotOptions$axisTextColor(),
            angle=as.numeric(univariateBoxplotOptions$xAxisTextAngle()),
            hjust = as.numeric(univariateBoxplotOptions$xAxisTextHjust()),
            vjust = as.numeric(univariateBoxplotOptions$xAxisTextVjust())
          ),
          axis.ticks = ggplot2::element_line(
            colour=univariateBoxplotOptions$axisTicksColor(), size=as.numeric(univariateBoxplotOptions$axisTicksSize())
          ),
          axis.title.y = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(univariateBoxplotOptions$yAxisTitleSize())
          ),
          # les grilles
          panel.grid.major = ggplot2::element_line(
            ifelse(length(univariateBoxplotOptions$panelGridLineType()),
                   as.numeric(univariateBoxplotOptions$panelGridLineType()),
                   univariateBoxplotOptions$panelGridLineType()),
            color = univariateBoxplotOptions$panelGridColor(), size=grid::unit(.23, "cm")
          ),
          panel.grid.minor = ggplot2::element_blank(),
          # texte de l'axe des y
          axis.text.y= ggplot2::element_text(
            family = "Times", size=as.numeric(univariateBoxplotOptions$yAxisTextSize()), color =  univariateBoxplotOptions$axisTextColor()
          ),
          # contours
          panel.background = ggplot2::element_rect(
            color=univariateBoxplotOptions$panelBackgroundLineColor(), fill="white", size=as.numeric(univariateBoxplotOptions$panelBackgroundLineSize())
          )
        )+

        ggplot2::labs(
          x = NULL, y = univariateBoxplotOptions$yAxisTitle(), title = NULL
        )

      # résummé statistique
      statSum <- cleanedData %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          Min. = round(min(valeur, na.rm = T), 2), Quart1 = round(quantile(valeur, .25), 2),
          Médianne = round(median(valeur), 2), Quart3 = round(quantile(valeur, .75), 2),
          Moyenne = round(mean(valeur), 2), Max = round(max(valeur), 2),
          "Ecart type" = round(sd(valeur), 2)
        )

      return(list(univariateBplt, statSum))
    })

    output$univariateBoxplot <- renderPlot({
      req(univariateBoxplot())
      univariateBoxplot()[[1]]
    })

    output$statsSummary <- renderDataTable({
      req(univariateBoxplot())
      univariateBoxplot()[[2]]
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("Boxplot-Univarié-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, univariateBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("Boxplot-Univarié-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, univariateBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )
  })
}

## To be copied in the UI
# mod_makingUnivariateBoxplot_ui("makingUnivariateBoxplot_1")

## To be copied in the server
# mod_makingUnivariateBoxplot_server("makingUnivariateBoxplot_1")
