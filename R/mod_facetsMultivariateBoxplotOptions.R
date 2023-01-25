#' facetsMultivariateBoxplotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_facetsMultivariateBoxplotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),

      column(4, numericInput(ns("xAxisTextSize"), label = div("X-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 13)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Y-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 13)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0.50)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=35, step=1, value = 16)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=5, value = 0)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"),  min=.5, max=7, step=.01, value = 1.60)),
      column(4, numericInput(ns("outlierShape"), label = div("Outlier Shape", style="family:Georgia;text-align:left;font-size:65%"),  min = 0, max=25, step=1, value = 1)),
      column(4, numericInput(ns("outlierSize"), label = div("Outlier Size", style="family:Georgia;text-align:left;font-size:65%"), min=.2, max=10, step=.01, value = 2.00)),
      column(4, numericInput(ns("boxplotAlpha"), label = div("Box. Alpha", style="family:Georgia;text-align:left;font-size:65%"),  min=0, max=1, step=.1, value = 1)),
      column(4, numericInput(ns("boxplotsWidth"), label = div("Box. Width", style="family:Georgia;text-align:left;font-size:65%"),  min=.1, max=1, step=.01, value = .50)),
      column(4, numericInput(ns("thresholdSize"), label = div("Seuil LineSize", style="family:Georgia;text-align:left;font-size:65%"), min=.2, max=10, step=.01, value = .9)),
      column(4, numericInput(ns("legendNrow"), label = div("Leg. Nrow", style="family:Georgia;text-align:left;font-size:65%"),  min=1, max=10, step=1, value = 1)),
      column(4, numericInput(ns("legendKeyWidth"), label = div("Leg. KeyWidth", style="family:Georgia;text-align:left;font-size:65%"),  min=.2, max=10, step=.01, value = .80)),
      column(4, numericInput(ns("legendKeyHeight"), label = div("Leg. KeyHeight", style="family:Georgia;text-align:left;font-size:65%"),  min=.2, max=10, step=.01, value = .8)),
      column(4, numericInput(ns("LegTextSize"), label = div("Leg. TextSize", style="family:Georgia;text-align:left;font-size:65%"),  min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("legTextMarginB"), label = div("Leg. TextMargB.", style="family:Georgia;text-align:left;font-size:70%"), min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"), label = div("Leg. TextMargT.", style="family:Georgia;text-align:left;font-size:70%"), min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginL"), label = div("Leg. TextMargL.", style="family:Georgia;text-align:left;font-size:70%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginR"), label = div("Leg. TextMargR.", style="family:Georgia;text-align:left;font-size:70%"), min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legPosXcoord"), label = div("LegXaxisCoords", style="family:Georgia;text-align:left;font-size:65%"),  min=0, max=1, step=.001, value = 0.500)),
      column(4, numericInput(ns("legPosYcoord"), label = div("LegYaxisCoords", style="family:Georgia;text-align:left;font-size:65%"),  min=0, max=1, step=.001, value = 0.500)),
      column(4, numericInput(ns("legMarginB"), label = div("Leg. Marg. B.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginT"), label = div("Leg. Marg. T.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = -.60)),
      column(4, numericInput(ns("legMarginL"), label = div("Leg. Marg. L.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginR"), label = div("Leg. Marg. R.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("stripTextSize"), label = div("Strip TextSize", style="family:Georgia;text-align:left;font-size:65%"),  min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("stripTextMarginB"), label = div("Strip Marg. B.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 2)),
      column(4, numericInput(ns("stripTextMarginT"), label = div("Strip Marg. T.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("stripTextMarginL"), label = div("Strip Marg. L.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("stripTextMarginR"), label = div("Strip Marg. R.", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("panelYspacing"), label = div("panelYspacing", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
      column(6, numericInput(ns("panelXspacing"), label = div("panelXspacing", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=5, step=.01, value = -.01)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis TextColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelGridLineType"), label = div("Grid LineType", style="family:Georgia;text-align:left;font-size:65%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("panelGridColor"), label = div("Grid Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("outlierColor"), label = div("Outlier Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("boxplotColor"), label = div("Boxplots Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("threshold"), label = div("Seuil", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Oui"=TRUE, "Non"=FALSE), selected = FALSE)),
      column(4, selectInput(ns("thresholdLineType"), label = div("Seuil LineType", style="family:Georgia;text-align:left;font-size:65%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 1)),
      column(4, selectInput(ns("thresholdColor"), label = div("Couleur Seuil", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "red")),
      column(4, selectInput(ns("legendByRow"), label = div("Leg. Fill", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("By Row" = TRUE, "By Column" = FALSE), selected = TRUE)),
      column(4, selectInput(ns("legDir"), label = div("Leg. direction", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("H"="horizontal", "V"="vertical"), selected = "horizontal")),
      column(4, selectInput(ns("LegTextColor"), label = div("Leg. TextColor", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("legPosType"), label = div("Leg. Pos.Type", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Côté", "Coordonnées"), selected = "Côté")),
      column(4, selectInput(ns("legLoc"), label = div("Leg. Position", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("top", "bottom", "left", "right"), selected = "bottom")),
      # strips
      column(6, selectInput(ns("stripTextMarginUnit"), label = div("Strip Marg. Unit", style="family:Georgia;text-align:left;font-size:65%"), choices = c("pt", "cm"), selected = "pt")),
      column(6, selectInput(ns("FacetScale"), label = div("FacetScales", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("fixed", "free_x", "free_y"), selected = "free_x")),
      column(6, selectInput(ns("flipAxis"), label = div("Pivoter Graphique", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Oui" = TRUE, "Non" = FALSE), selected = "TRUE")),
      column(6, selectInput(ns("reorderGprah"), label = div("Trier Les Boxs", style="family:Georgia;text-align:left;font-size:65%"),  choices = c("Asc."="Descendant", "Desc."="Ascendant"), selected = "Ascendant")),
      column(12, textInput(ns("variableColors"), label = div("Couleur des Groupes", style="family:Georgia;text-align:left;font-size:70%"), value = "", placeholder = "red;green;blue;cyan;orange;...")),
      column(12, textInput(ns("xAxisTitle"), label = div("X-Axis Title (Graph Pivoté)", style="family:Georgia;text-align:left;font-size:70%"), value = "", placeholder = "Titre Axe-X")),
      column(12, textInput(ns("yAxisTitle"), label = div("Y-Axis Title (Graph Pivoté)", style="family:Georgia;text-align:left;font-size:70%"), value = "", placeholder = "Titre Axe-Y"))
    )
  )
}

#' facetsMultivariateBoxplotOptions Server Functions
#'
#' @noRd
mod_facetsMultivariateBoxplotOptions_server <- function(id){
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
# mod_facetsMultivariateBoxplotOptions_ui("facetsMultivariateBoxplotOptions_1")

## To be copied in the server
# mod_facetsMultivariateBoxplotOptions_server("facetsMultivariateBoxplotOptions_1")
