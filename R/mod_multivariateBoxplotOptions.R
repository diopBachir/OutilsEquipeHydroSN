#' multivariateBoxplotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multivariateBoxplotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),

      column(4, selectInput(ns("xAxisTextSize"), label = div("X-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 13)),
      column(4, selectInput(ns("yAxisTextSize"), label = div("Y-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 13)),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis text color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), choices = seq(0, 360, 5), selected = 0)),
      column(4, selectInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0.50)),
      column(4, selectInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = .50)),

      column(4, selectInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:75%"),  choices = seq(5, 35, 1), selected = 16)),
      column(4, selectInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.1, 7, .01), 2), selected = 1.00)),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("panelGridLineType"), label = div("Grid LineType", style="family:Georgia;text-align:left;font-size:75%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("panelGridColor"), label = div("Grid Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),

      column(4, selectInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.5, 7, .01), 2), selected = 1.60)),
      column(4, selectInput(ns("outlierShape"), label = div("Outlier Shape", style="family:Georgia;text-align:left;font-size:75%"),  choices = 0:25, selected = 1)),

      column(4, selectInput(ns("outlierColor"), label = div("Outlier Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("outlierSize"), label = div("Outlier Size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.2, 10, .01), 2), selected = 2.00)),
      column(4, selectInput(ns("boxplotColor"), label = div("Boxplots Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),

      column(4, selectInput(ns("boxplotAlpha"), label = div("Box. Alpha", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(0, 1, .1), 1), selected = 1)),
      column(4, selectInput(ns("yAxisNbreaks"), label = div("Y-axis Nbreaks", style="family:Georgia;text-align:left;font-size:75%"), choices = seq(3, 15, 1), selected = 4)),
      column(4, selectInput(ns("boxplotsWidth"), label = div("Box. Width", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.1, 1, .01), 2), selected = .50)),

      column(4, selectInput(ns("thresholdColor"), label = div("Couleur Seuil", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "red")),
      column(4, selectInput(ns("thresholdSize"), label = div("Seuil LineSize", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.2, 10, .01), 2), selected = 1.3)),
      column(4, selectInput(ns("thresholdLineType"), label = div("Seuil LineType", style="family:Georgia;text-align:left;font-size:75%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 1)),

      column(4, selectInput(ns("legendNrow"), label = div("Legend Nrow", style="family:Georgia;text-align:left;font-size:75%"),  choices = 1:10, selected = 1)),
      column(4, selectInput(ns("legendByRow"), label = div("Legend Fill", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Row" = TRUE, "Column" = FALSE), selected = TRUE)),
      column(4, selectInput(ns("legendKeyWidth"), label = div("Leg. KeyWidth", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.2, 10, .01), 2), selected = .60)),

      column(4, selectInput(ns("legendKeyHeight"), label = div("Leg. KeyHeight", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.2, 10, .01), 2), selected = 1.1)),
      column(4, selectInput(ns("legDir"), label = div("Leg. direction", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("H"="horizontal", "V"="vertical"), selected = "horizontal")),
      column(4, selectInput(ns("LegTextSize"), label = div("Leg. text size", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(5, 35, 1), 2), selected = 13)),

      column(4, selectInput(ns("LegTextColor"), label = div("Leg. text color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("legTextMarginB"), label = div("Leg. Text Marg. bottom", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginT"), label = div("Leg. Text Marg. top", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(4, selectInput(ns("legTextMarginL"), label = div("Leg. Text Marg. left", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginR"), label = div("Leg. Text Marg. right", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legPosType"), label = div("Leg. Pos. Loc. Type", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Côté", "Coordonnées"), selected = "Côté")),

      column(4, selectInput(ns("legPosXcoord"), label = div("Leg. Pos. Xcoords", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(0, 1, .001), 3), selected = 0.500)),
      column(4, selectInput(ns("legPosYcoord"), label = div("Leg. Pos. Ycoords", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(0, 1, .001), 3), selected = 0.500)),
      column(4, selectInput(ns("legLoc"), label = div("Legend Pos. Loc.", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("top", "bottom", "left", "right"), selected = "bottom")),

      column(4, selectInput(ns("legMarginB"), label = div("Leg. Marg. botom", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legMarginT"), label = div("Leg. Marg. top", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = -.60)),
      column(4, selectInput(ns("legMarginL"), label = div("Leg. Marg. left", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(4, selectInput(ns("legMarginR"), label = div("Leg. Marg. right", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("flipAxis"), label = div("Fip Coords", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Oui" = TRUE, "Non" = FALSE), selected = TRUE)),
      column(4, selectInput(ns("reorderGprah"), label = div("Trier les boxs", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Desc"="Descendant", "Asc"="Ascendant"), selected = "Ascendant")),

      column(12, textInput(ns("threshold"), label = div("Valeur Seuil", style="family:Georgia;text-align:left;font-size:85%"), value = "", placeholder = "0")),
      column(12, textInput(ns("variableColors"), label = div("Couleur des Catégories|Types", style="family:Georgia;text-align:left;font-size:85%"), value = "", placeholder = "red;green;blue;cyan;orange;...")),
      column(12, textInput(ns("xAxisTitle"), label = div("X-Axis Title (Graph Pivoté)", style="family:Georgia;text-align:left;font-size:85%"), value = "", placeholder = "Titre Axe-X")),
      column(12, textInput(ns("yAxisTitle"), label = div("Y-Axis Title (Graph Pivoté)", style="family:Georgia;text-align:left;font-size:85%"), value = "", placeholder = "Titre Axe-Y"))

    )
  )
}

#' multivariateBoxplotOptions Server Functions
#'
#' @noRd
mod_multivariateBoxplotOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(list(
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
      flipAxis      = reactive({ input$flipAxis }),
      reorderGprah      = reactive({ input$reorderGprah })
    ))

  })
}

## To be copied in the UI
# mod_multivariateBoxplotOptions_ui("multivariateBoxplotOptions_1")

## To be copied in the server
# mod_multivariateBoxplotOptions_server("multivariateBoxplotOptions_1")
