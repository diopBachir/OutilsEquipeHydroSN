#' TrendAnalysisGraphsOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TrendAnalysisGraphsOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, h4("GGPLOT [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),
      column(4, numericInput(ns("bvContSize"), label = div("BV Contour Size", style="family:Georgia;text-align:left;font-size:65%"), min=0.01, max=7, step=.01, value=2.5)),
      column(4, numericInput(ns("northArrowWidth"), label = div("NorthArrowWidth", style="family:Georgia;text-align:left;font-size:65%"), min=-10, max=10, step=.1, value = 1.5)),
      column(4, numericInput(ns("northArrowHeight"), label = div("NorthArrowHeight", style="family:Georgia;text-align:left;font-size:65%"),min=-10, max=10, step=.1, value = 1.5)),
      column(4, numericInput(ns("northArrowPadx"), label = div("NorthArrowPadx", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = -.47)),
      column(4, numericInput(ns("northArrowPady"), label = div("NorthArrowPady", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("scaleWidthHint"), label = div("ScaleBarWidthHint", style="family:Georgia;text-align:left;font-size:65%"),min=.01, max=10, step=.01, value = .13)),
      column(4, numericInput(ns("scaleTickHeight"), label = div("ScaleBarTicksHeight", style="family:Georgia;text-align:left;font-size:65%"), min=.01, max=10, step=.01, value = .30)),
      column(4, numericInput(ns("scaleTextCex"), label = div("ScaleBarTextCex", style="family:Georgia;text-align:left;font-size:65%"),min=.01, max=10, step=.01, value = 1.15)),
      column(4, numericInput(ns("xAxisTextSize"), label = div("XaxiTextSize", style="family:Georgia;text-align:left;font-size:65%"),min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("YaxisTextSize", style="family:Georgia;text-align:left;font-size:65%"),min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("XaxisVjust", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("XaxisHjust", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("XaxisAngle", style="family:Georgia;text-align:left;font-size:65%"),min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTitleSize"), label = div("X-TitleSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("yAxisTitleSize"), label = div("Y-TitleSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("axisTicksSize"), label = div("TicksSize", style="family:Georgia;text-align:left;font-size:65%"),min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("PanelSize", style="family:Georgia;text-align:left;font-size:65%"), min=.5, max=7, step=.01, value = 2.7)),
      column(4, numericInput(ns("facetNrow"), label = div("FacetsNrow", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=10, step=1, value = 0)),
      column(4, numericInput(ns("stripTextSize"), label = div("StripTextize", style="family:Georgia;text-align:left;font-size:65%"), min=5, 35, 1, value = 13)),
      column(4, numericInput(ns("stripMarginB"), label = div("StripTextMargB", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 2)),
      column(4, numericInput(ns("stripMarginT"), label = div("StripTextMargT", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("stripMarginL"), label = div("StripTextMargL", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("stripMarginR"), label = div("StripTextMargR", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTitleAngle"),  label = div("LegTitleOrientation", style="family:Georgia;text-align:left;font-size:65%"),min=0, max=360, step=45, value = 0)),
      column(4, numericInput(ns("legTexAngle"),  label = div("LegTextOrientation", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=45, value = 0)),
      column(4, numericInput(ns("LegTextSize"),  label = div("LegTextSize", style="family:Georgia;text-align:left;font-size:65%"),min=5, max=35, step=1, value = 12)),
      column(4, numericInput(ns("legTextMarginB"),  label = div("LegTextMargB", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"),  label = div("LegTextMargT", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginL"),  label = div("LegTextMargL", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = -.15)),
      column(4, numericInput(ns("legTextMarginR"),  label = div("LegTextMargR", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legPosXcoord"),  label = div("LegPosXcoords", style="family:Georgia;text-align:left;font-size:65%"),min=0, max=1, step=.001, value = 0.500)),
      column(4, numericInput(ns("legPosYcoord"),  label = div("LegPosYcoords", style="family:Georgia;text-align:left;font-size:65%"),min=0, max=1, step=.001, value = 0.500)),
      column(4, numericInput(ns("legMarginB"),  label = div("LegMargB", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = .05)),
      column(4, numericInput(ns("legMarginT"),  label = div("LegMargT", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginL"),  label = div("LegMargL", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginR"),  label = div("LegMargR", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("panelYspacing"), label = div("panelYspacing", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("panelXspacing"), label = div("panelXspacing", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=5, step=.01, value = .1)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("AxisTextColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("bvContColor"), label = div("BvContourColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("northArrowLocation"), label = div("NorthArrowLoc.", style="family:Georgia;text-align:left;font-size:65%"), choices = c("bl", "br", "tl", "tr"), selected = "tl")),
      column(4, selectInput(ns("scaleLocation"), label = div("ScaleBarLoc.", style="family:Georgia;text-align:left;font-size:65%"), choices = c("bl", "br", "tl", "tr"), selected = "bl")),
      # column(2, selectInput(ns("scaleLineWidth"), label = div("Line width",min=.01, 10, .01), selected = .18)),
      column(4, selectInput(ns("scaleStyle"), label = div("ScaleBarStyle", style="family:Georgia;text-align:left;font-size:65%"), choices = c("bar", "ticks"), selected = "ticks")),
      column(4, selectInput(ns("axisTicksColor"), label = div("TicksColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelGridColor"), label = div("GridColor", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("PanelColor", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelGridLineType"), label = div("GridLineType", style="family:Georgia;text-align:left;font-size:65%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("stripMarginUnit"), label = div("StripTextMargUnit", style="family:Georgia;text-align:left;font-size:65%"), choices = c("pt", "cm"), selected = "pt")),
      column(4, selectInput(ns("legDir"),  label = div("LegDirection",  style="family:Georgia;text-align:left;font-size:65%"), choices = c("H"="horizontal", "V"="vertical"), selected = "horizontal")),
      column(4, selectInput(ns("LegTextColor"),  label = div("LegTextColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("legPalette"), label = div("ColourBarPalette", style="family:Georgia;text-align:left;font-size:65%"), choices = c("Temp"="temperature", "Blues"="blues"), selected = "temperature")),
      column(4, selectInput(ns("legPosType"),  label = div("LegPositionType", style="family:Georgia;text-align:left;font-size:65%"), choices = c("Côté"="Côté", "Coords"="Coordonnées"), selected = "Côté")),
      column(4, selectInput(ns("legLoc"),  label = div("LegendLocation",  style="family:Georgia;text-align:left;font-size:65%"), choices = c("top", "bottom", "left", "right"), selected = "bottom")),

      column(12, textInput(ns("variableColors"), label = div("Couleur des Groupes", style="family:Georgia;text-align:left;font-size:70%"), value = "", placeholder = "red;green;blue;cyan;orange;..."))
    )
  )
}

#' TrendAnalysisGraphsOptions Server Functions
#'
#' @noRd
mod_TrendAnalysisGraphsOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        # north arrow
        northArrowLocation	= reactive({ input$northArrowLocation  }) ,
        northArrowWidth 	= reactive({ input$northArrowWidth  }) ,
        northArrowHeight 	= reactive({ input$northArrowHeight  }) ,
        northArrowPadx  	= reactive({ input$northArrowPadx   }) ,
        northArrowPady	    = reactive({ input$northArrowPady }) ,
        # --------------------------------------------------------#
        # scale bar
        scaleWidthHint 		= reactive({ input$scaleWidthHint  }) ,
        scaleTickHeight		= reactive({ input$scaleTickHeight  }) ,
        scaleTextCex		= reactive({ input$scaleTextCex }) ,
        scaleStyle		=		reactive({ input$scaleStyle }) ,
        # scaleLineWidth 		= reactive({ input$scaleLineWidth   }) ,
        scaleLocation		= reactive({ input$scaleLocation }) ,
        bvContSize 			= reactive({ input$bvContSize  }) ,
        bvContColor			= reactive({ input$bvContColor }) ,
        xAxisTextSize = reactive({input$xAxisTextSize}),
        yAxisTextSize = reactive({input$yAxisTextSize}),
        axisTextColor = reactive({input$axisTextColor}),
        xAxisTextAngle = reactive({input$xAxisTextAngle}),
        xAxisTextVjust = reactive({input$xAxisTextVjust}),
        xAxisTextHjust = reactive({input$xAxisTextHjust}),
        yAxisTitleSize = reactive({input$yAxisTitleSize}),
        xAxisTitleSize = reactive({input$xAxisTitleSize}),
        axisTicksSize = reactive({input$axisTicksSize}),
        axisTicksColor = reactive({input$axisTicksColor}),
        panelGridLineType = reactive({input$panelGridLineType}),
        panelGridColor = reactive({input$panelGridColor}),
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),
        # Strips
        facetNrow      = reactive({ input$facetNrow  }) ,
        stripTextSize      = reactive({ input$stripTextSize  }) ,
        stripMarginB      = reactive({ input$stripMarginB }) ,
        stripMarginT      = reactive({ input$stripMarginT }) ,
        stripMarginL      = reactive({ input$stripMarginL }) ,
        stripMarginR      = reactive({ input$stripMarginR }) ,
        stripMarginUnit      = reactive({ input$stripMarginUnit }) ,
        panelXspacing      = reactive({ input$panelXspacing }),
        panelYspacing      = reactive({ input$panelYspacing }),

        legDir 				= reactive({ input$legDir  }) ,
        legTitleAngle		= reactive({ input$legTitleAngle }) ,
        legTexAngle 		= reactive({ input$legTexAngle  }) ,
        LegTextSize			= reactive({ input$LegTextSize }) ,
        LegTextColor		= reactive({ input$LegTextColor }) ,
        legTextMarginB		= reactive({ input$legTextMarginB  }) ,
        legTextMarginT		= reactive({ input$legTextMarginT  }) ,
        legTextMarginL		= reactive({ input$legTextMarginL  }) ,
        legTextMarginR		= reactive({ input$legTextMarginR }) ,
        legPosType 				= reactive({ input$legPosType  }) ,
        legPosXcoord 				= reactive({ input$legPosXcoord  }) ,
        legPosYcoord 				= reactive({ input$legPosYcoord  }) ,
        legPalette 			= reactive({ input$legPalette  }) ,
        legLoc 				= reactive({ input$legLoc  }) ,
        legPalette 			= reactive({ input$legPalette  }) ,
        legMarginB			= reactive({ input$legMarginB }) ,
        legMarginT			= reactive({ input$legMarginT }) ,
        legMarginL			= reactive({ input$legMarginL }) ,
        legMarginR			= reactive({ input$legMarginR }) ,

        variableColors      = reactive({ input$variableColors })
      )
    )
  })
}

## To be copied in the UI
# mod_TrendAnalysisGraphsOptions_ui("TrendAnalysisGraphsOptions_1")

## To be copied in the server
# mod_TrendAnalysisGraphsOptions_server("TrendAnalysisGraphsOptions_1")
