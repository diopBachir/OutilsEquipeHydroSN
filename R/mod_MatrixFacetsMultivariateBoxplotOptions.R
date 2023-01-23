#' MatrixFacetsMultivariateBoxplotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MatrixFacetsMultivariateBoxplotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),

      column(4, selectInput(ns("xAxisTextSize"), label = div("Xaxis TextSize", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(5, 40, 1), 2), selected = 11)),
      column(4, selectInput(ns("yAxisTextSize"), label = div("Yaxis TextSize", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(5, 40, 1), 2), selected = 10)),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis TextColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("xAxisTextAngle"), label = div("Xaxis Angle", style="family:Georgia;text-align:left;font-size:65%"), choices = seq(0, 360, 5), selected = 0)),
      column(4, selectInput(ns("xAxisTextHjust"), label = div("Xaxis Hjust", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0.50)),
      column(4, selectInput(ns("xAxisTextVjust"), label = div("Xaxis Vjust", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = .50)),

      column(4, selectInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:65%"),  choices = seq(5, 35, 1), selected = 16)),
      column(4, selectInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(.1, 7, .01), 2), selected = 1.00)),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("panelGridLineType"), label = div("Grid LineType", style="family:Georgia;text-align:left;font-size:65%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("panelGridColor"), label = div("Grid Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),

      column(4, selectInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(.5, 7, .01), 2), selected = 1.60)),
      column(4, selectInput(ns("outlierShape"), label = div("Outlier Shape", style="family:Georgia;text-align:left;font-size:65%"),  choices = 0:25, selected = 1)),

      column(4, selectInput(ns("outlierColor"), label = div("Outlier Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("outlierSize"), label = div("Outlier Size", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(.2, 10, .01), 2), selected = 2.00)),
      column(4, selectInput(ns("boxplotColor"), label = div("Boxplots Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),

      column(4, selectInput(ns("boxplotAlpha"), label = div("Box. Alpha", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(0, 1, .1), 1), selected = 1)),
      column(4, selectInput(ns("boxplotsWidth"), label = div("Box. Width", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(.1, 1, .01), 2), selected = .50)),

      column(4, selectInput(ns("threshold"), label = div("Seuil", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Oui"=TRUE, "Non"=FALSE), selected = FALSE)),
      column(4, selectInput(ns("thresholdColor"), label = div("Couleur Seuil", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "red")),
      column(4, selectInput(ns("thresholdSize"), label = div("Seuil LineSize", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(.2, 10, .01), 2), selected = .9)),
      column(4, selectInput(ns("thresholdLineType"), label = div("Seuil LineType", style="family:Georgia;text-align:left;font-size:65%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 1)),

      column(4, selectInput(ns("legendNrow"), label = div("Leg. Nrow", style="family:Georgia;text-align:left;font-size:65%"),  choices = 1:10, selected = 1)),
      column(4, selectInput(ns("legendByRow"), label = div("Leg. Fill", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("By Row" = TRUE, "By Column" = FALSE), selected = TRUE)),
      column(4, selectInput(ns("legendKeyWidth"), label = div("Leg. KeyWidth", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(.2, 10, .01), 2), selected = .60)),

      column(4, selectInput(ns("legendKeyHeight"), label = div("Leg. KeyHeight", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(.2, 10, .01), 2), selected = .5)),
      column(4, selectInput(ns("legDir"), label = div("Leg. direction", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("H"="horizontal", "V"="vertical"), selected = "horizontal")),
      column(4, selectInput(ns("LegTextSize"), label = div("Leg. TextSize", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(5, 35, 1), 2), selected = 12)),

      column(4, selectInput(ns("LegTextColor"), label = div("Leg. TextColor", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("legTextMarginB"), label = div("Leg. TextMargB.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginT"), label = div("Leg. TextMargT.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(4, selectInput(ns("legTextMarginL"), label = div("Leg. TextMargL.", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginR"), label = div("Leg. TextMargR.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legPosType"), label = div("Leg. Pos.Type", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Côté", "Coord."), selected = "Côté")),

      column(4, selectInput(ns("legPosXcoord"), label = div("Leg. XaxisCoords", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(0, 1, .001), 3), selected = 0.500)),
      column(4, selectInput(ns("legPosYcoord"), label = div("Leg. YaxisCoords", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(0, 1, .001), 3), selected = 0.500)),
      column(4, selectInput(ns("legLoc"), label = div("Leg. Position", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("top", "bottom", "left", "right"), selected = "bottom")),

      column(4, selectInput(ns("legMarginB"), label = div("Legend MargB.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legMarginT"), label = div("Legend MargT.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = -.60)),
      column(4, selectInput(ns("legMarginL"), label = div("Legend MargL.", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legMarginR"), label = div("Legend MargR.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),

      # strips
      column(4, selectInput(ns("stripTextSize"), label = div("Strip TextSize", style="family:Georgia;text-align:left;font-size:65%"),  choices = round(seq(5, 35, 1), 2), selected = 12)),
      column(4, selectInput(ns("stripTextMarginB"), label = div("Strip MargB.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 2)),
      column(4, selectInput(ns("stripTextMarginT"), label = div("Strip MargT.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(4, selectInput(ns("stripTextMarginL"), label = div("Strip MargL.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("stripTextMarginR"), label = div("Strip MargR.", style="family:Georgia;text-align:left;font-size:65%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("stripTextMarginUnit"), label = div("Strip MargUnit", style="family:Georgia;text-align:left;font-size:65%"), choices = c("pt", "cm"), selected = "pt")),

      column(4, selectInput(ns("FacetScale"), label = div("FacetScales", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("fixed", "free_x", "free_y"), selected = "free_x")),
      column(4, selectInput(ns("panelXspacing"), label = div("panelXspacing", style="family:Georgia;text-align:left;font-size:65%"), choices =round(seq(-5, 5, .01), 2), selected = -0.05)),
      column(4, selectInput(ns("panelYspacing"), label = div("panelYspacing", style="family:Georgia;text-align:left;font-size:65%"), choices =round(seq(-5, 5, .01), 2), selected = .5)),

      column(4, selectInput(ns("flipAxis"), label = div("Fip Coords", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Oui" = TRUE, "Non" = FALSE), selected = "TRUE")),
      column(4, selectInput(ns("reorderGprah"), label = div("Trier les boxs", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Desc."="Descendant", "Asc."="Ascendant"), selected = "Ascendant")),

      column(12, textInput(ns("variableColors"), label = div("Couleur des Groupes", style="family:Georgia;text-align:left;font-size:65%"), value = "", placeholder = "red;green;blue;cyan;orange;...")),
      column(12, textInput(ns("xAxisTitle"), label = div("Xaxis Title", style="family:Georgia;text-align:left;font-size:65%"), value = "", placeholder = "Titre Axe-X")),
      column(12, textInput(ns("yAxisTitle"), label = div("Yaxis Title", style="family:Georgia;text-align:left;font-size:65%"), value = "", placeholder = "Titre Axe-Y"))

    )
  )
}

#' MatrixFacetsMultivariateBoxplotOptions Server Functions
#'
#' @noRd
mod_MatrixFacetsMultivariateBoxplotOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(
      list(
        xAxisTextSize = reactive({input$xAxisTextSize}),
        yAxisTextSize = reactive({input$yAxisTextSize}),
        axisTextColor = reactive({input$axisTextColor}),
        xAxisTextAngle = reactive({input$xAxisTextAngle}),
        xAxisTextHjust = reactive({input$xAxisTextHjust}),
        AxisTitleSize = reactive({input$AxisTitleSize}),
        xAxisTextVjust = reactive({input$xAxisTextVjust}),
        axisTicksSize = reactive({input$axisTicksSize}),
        axisTicksColor = reactive({input$axisTicksColor}),
        panelGridLineType = reactive({input$panelGridLineType}),
        panelGridColor = reactive({input$panelGridColor}),
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),
        outlierShape = reactive({input$outlierShape}),
        outlierSize = reactive({input$outlierSize}),
        outlierColor = reactive({input$outlierColor}),
        boxplotColor = reactive({input$boxplotColor}),
        boxplotAlpha = reactive({input$boxplotAlpha}),
        yAxisNbreaks = reactive({input$yAxisNbreaks}),
        xAxisTitle = reactive({input$xAxisTitle}),
        yAxisTitle = reactive({input$yAxisTitle}),
        boxplotsWidth = reactive({input$boxplotsWidth}),
        variableColors = reactive({input$variableColors}),
        threshold = reactive({input$threshold}),
        thresholdColor = reactive({input$thresholdColor}),
        thresholdSize = reactive({input$thresholdSize}),
        thresholdLineType = reactive({input$thresholdLineType}),
        legendNrow = reactive({input$legendNrow}),
        legendByRow = reactive({input$legendByRow}),
        legendKeyWidth = reactive({input$legendKeyWidth}),
        legendKeyHeight = reactive({input$legendKeyHeight}),
        legDir 				= reactive({ input$legDir  }) ,
        LegTextSize			= reactive({ input$LegTextSize }) ,
        LegTextColor		= reactive({ input$LegTextColor }) ,
        legTextMarginB		= reactive({ input$legTextMarginB  }) ,
        legTextMarginT		= reactive({ input$legTextMarginT  }) ,
        legTextMarginL		= reactive({ input$legTextMarginL  }) ,
        legTextMarginR		= reactive({ input$legTextMarginR }) ,
        legPosType 				= reactive({ input$legPosType  }) ,
        legPosXcoord 				= reactive({ input$legPosXcoord  }) ,
        legPosYcoord 				= reactive({ input$legPosYcoord  }) ,
        legLoc 				= reactive({ input$legLoc  }) ,
        legMarginB			= reactive({ input$legMarginB }) ,
        legMarginT			= reactive({ input$legMarginT }) ,
        legMarginL			= reactive({ input$legMarginL }) ,
        legMarginR			= reactive({ input$legMarginR }) ,
        # Strips
        stripTextSize      = reactive({ input$stripTextSize  }) ,
        stripMarginB      = reactive({ input$stripTextMarginB }) ,
        stripMarginT      = reactive({ input$stripTextMarginT }) ,
        stripMarginL      = reactive({ input$stripTextMarginL }) ,
        stripMarginR      = reactive({ input$stripTextMarginR }) ,
        stripMarginUnit      = reactive({ input$stripTextMarginUnit }) ,
        panelXspacing      = reactive({ input$panelXspacing }),
        panelYspacing      = reactive({ input$panelYspacing }),
        FacetScale      = reactive({ input$FacetScale }),
        flipAxis      = reactive({ input$flipAxis }),
        reorderGprah      = reactive({ input$reorderGprah })
      )
    )
  })
}

## To be copied in the UI
# mod_MatrixFacetsMultivariateBoxplotOptions_ui("MatrixFacetsMultivariateBoxplotOptions_1")

## To be copied in the server
# mod_MatrixFacetsMultivariateBoxplotOptions_server("MatrixFacetsMultivariateBoxplotOptions_1")
