#' dailyInventoryHeatmapOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dailyInventoryHeatmapOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    column(12, h4("GGPLOT [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),

    fluidRow(
      column(4, numericInput(ns("xAxisTextSize"), label = div("X-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Y-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 7)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=35, step=1, value = 16)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"),  min=.5, max=7, step=.01, value = 1.3)),
      column(4, numericInput(ns("barHeight"), label = div("Bar Height", style="family:Georgia;text-align:left;font-size:65%"),   min=0.1, max=65, step=.1, value = .9)),
      column(4, numericInput(ns("barWidth"), label = div("Bar Width", style="family:Georgia;text-align:left;font-size:65%"),   min=0.1, max=50, step=.1, value = 45)),
      column(4, numericInput(ns("barTitleHjust"), label = div("Title hjust", style="family:Georgia;text-align:left;font-size:65%"),   min=-2, max=2, step=.1, value = .5)),
      column(4, numericInput(ns("barTitleVjust"), label = div("Title vjust", style="family:Georgia;text-align:left;font-size:65%"),   min=-2, max=2, step=.1, value = -1)),
      column(4, numericInput(ns("barTicksLineWidth"), label = div("Ticks width", style="family:Georgia;text-align:left;font-size:65%"),  min=0.1, max=10, step=.1, value = 1)),
      column(4, numericInput(ns("barTitleSize"), label = div("Bar TitleTextSize", style="family:Georgia;text-align:left;font-size:65%"),   min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("barTitleMarginB"), label = div("Leg. TitleMarginB",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = .25)),
      column(4, numericInput(ns("barTitleMarginT"), label = div("Leg. TitleMarginT.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("barTitleMarginL"), label = div("Leg. TitleMarginL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("barTitleMarginR"), label = div("Leg. TitleMarginR.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginB"), label = div("Leg. TextMargB.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"), label = div("Leg. TextMargT.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = -.15)),
      column(4, numericInput(ns("legTextMarginL"), label = div("Leg. TextMargL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginR"), label = div("Leg. TextMargR.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("LegTextSize"), label = div("Legend TextSize", style="family:Georgia;text-align:left;font-size:65%"),   min=5, max=35, step=1, value = 12)),
      column(4, numericInput(ns("xAxisBreakStep"), label = div("Date Break Step", style="family:Georgia;text-align:left;font-size:65%"), min=1, max=10, step=1, value = 4)),
      # la légende
      column(6, numericInput(ns("legMarginB"), label = div("LegMarginB.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = .05)),
      column(6, numericInput(ns("legMarginT"), label = div("LegMarginT.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("legMarginL"), label = div("LegMarginL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("legMarginR"), label = div("LegMarginR.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis text color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("barTitleLoc"), label = div("Title Loc.", style="family:Georgia;text-align:left;font-size:65%"),   choices = c("top", "bottom", "left", "right"), selected = "top")),
      column(4, selectInput(ns("barTicksColor"), label = div("Ticks color", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
      column(4, selectInput(ns("LegTextColor"), label = div("Legend Text Color", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
      column(12, textInput(ns("legTitleName"), label = div("Titre de la légende", style="family:Georgia;text-align:left;font-size:75%"), value = "% Lacunes"))
    )
  )
}

#' dailyInventoryHeatmapOptions Server Functions
#'
#' @noRd
mod_dailyInventoryHeatmapOptions_server <- function(id){
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
        xAxisBreakStep = reactive({input$xAxisBreakStep}),
        axisTicksSize = reactive({input$axisTicksSize}),
        axisTicksColor = reactive({input$axisTicksColor}),
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),

        # guide_color
        barHeight      = reactive({ input$barHeight  }) ,
        barWidth       = reactive({ input$barWidth   }) ,
        barTitleLoc      = reactive({ input$barTitleLoc }) ,
        barTitleHjust    = reactive({ input$barTitleHjust }) ,
        barTitleVjust    = reactive({ input$barTitleVjust }) ,
        barTicksColor    = reactive({ input$barTicksColor  }) ,
        barTicksLineWidth  = reactive({ input$barTicksLineWidth  }) ,
        barTitleSize     = reactive({ input$barTitleSize  }) ,
        barTitleMarginB    = reactive({ input$barTitleMarginB }) ,
        barTitleMarginL  = reactive({ input$barTitleMarginL  }) ,
        barTitleMarginT  = reactive({ input$barTitleMarginT  }) ,
        barTitleMarginR    = reactive({ input$barTitleMarginR }) ,
        # theme
        LegTextSize      = reactive({ input$LegTextSize }) ,
        LegTextColor   = reactive({ input$LegTextColor }) ,
        legTextMarginB   = reactive({ input$legTextMarginB  }) ,
        legTextMarginT   = reactive({ input$legTextMarginT  }) ,
        legTextMarginL   = reactive({ input$legTextMarginL  }) ,
        legTextMarginR   = reactive({ input$legTextMarginR }) ,
        legMarginB     = reactive({ input$legMarginB }) ,
        legMarginT     = reactive({ input$legMarginT }) ,
        legMarginL     = reactive({ input$legMarginL }) ,
        legMarginR     = reactive({ input$legMarginR }) ,
        legTitleName   = reactive({ input$legTitleName })
      )
    )

  })
}

## To be copied in the UI
# mod_dailyInventoryHeatmapOptions_ui("dailyInventoryHeatmapOptions_1")

## To be copied in the server
# mod_dailyInventoryHeatmapOptions_server("dailyInventoryHeatmapOptions_1")
