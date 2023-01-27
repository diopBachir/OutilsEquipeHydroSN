#' spiHeatmapOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spiHeatmapOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(4, numericInput(ns("xAxisTextSize"), label = div("Bottom Xaxis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 13)),
    column(4, numericInput(ns("yAxisTextSize"), label = div("Yaxis L|R TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 13)),
    column(4, numericInput(ns("yRightAxisTitleSize"), label = div("Right Yaxis TitleSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 15)),
    column(4, numericInput(ns("xAxisTextAngle"), label = div("Xaxis Angle", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=5, value = 90)),
    column(4, numericInput(ns("xAxisTextHjust"), label = div("Xaxis Hjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
    column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .5)),
    column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), min=.1, max=7, step=.01, value = 1.00)),
    column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"),  min=.5, max=7, step=.01, value = 1.5)),
    column(4, numericInput(ns("barHeight"), label = div("Bar Height", style="family:Georgia;text-align:left;font-size:65%"),   min=0.1, max=65, step=.1, value = 1.2)),
    column(4, numericInput(ns("barWidth"), label = div("Bar Width", style="family:Georgia;text-align:left;font-size:65%"),   min=0.1, max=50, step=.1, value = 40)),
    column(4, numericInput(ns("barTicksLineWidth"), label = div("Ticks width", style="family:Georgia;text-align:left;font-size:65%"),   min=0.1, max=10, step=.1, value = 1)),
    column(4, numericInput(ns("legTextMarginB"), label = div("legTextMarginB",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
    column(4, numericInput(ns("legTextMarginT"), label = div("legTextMarginT",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = -.1)),
    column(4, numericInput(ns("legTextMarginL"), label = div("legTextMarginL", style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
    column(4, numericInput(ns("legTextMarginR"), label = div("legTextMarginR",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
    column(4, numericInput(ns("LegTextSize"), label = div("LegendTextSize", style="family:Georgia;text-align:left;font-size:65%"),   min=5, max=35, step=1, value = 12)),
    column(4, numericInput(ns("xAxisBreakStep"), label = div("DateBreakStep", style="family:Georgia;text-align:left;font-size:65%"), min=1, max=10, step=1, value = 4)),
    # la légende
    column(4, numericInput(ns("legMarginB"), label = div("LegendMarginB",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, 5, step=.01, value = 0)),
    column(4, numericInput(ns("legMarginT"), label = div("LegendMarginT",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, 5, step=.01, value = -.3)),
    column(4, numericInput(ns("legMarginL"), label = div("LegendMarginL", style="family:Georgia;text-align:left;font-size:65%"),   min=-5, 5, step=.01, value = 0)),
    column(4, numericInput(ns("legMarginR"), label = div("LegendMarginB",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, 5, step=.01, value = 0)),
    column(12, tags$hr(style="border-color:gray;")),
    column(4, selectInput(ns("axisTextColor"), label = div("Axis text color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
    column(4, selectInput(ns("axisTicksColor"), label = div("AxisTicksColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
    column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
    column(6, selectInput(ns("barTicksColor"), label = div("Legend Ticks Color", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
    column(6, selectInput(ns("LegTextColor"), label = div("Legend Text Color", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black"))
  )
}

#' spiHeatmapOptions Server Functions
#'
#' @noRd
mod_spiHeatmapOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        xAxisTextSize = reactive({input$xAxisTextSize}),
        yAxisTextSize = reactive({input$yAxisTextSize}),
        yRightAxisTitleSize = reactive({input$yRightAxisTitleSize}),
        axisTextColor = reactive({input$axisTextColor}),
        xAxisTextAngle = reactive({input$xAxisTextAngle}),
        xAxisTextHjust = reactive({input$xAxisTextHjust}),
        xAxisTextVjust = reactive({input$xAxisTextVjust}),
        xAxisBreakStep = reactive({input$xAxisBreakStep}),
        axisTicksSize = reactive({input$axisTicksSize}),
        axisTicksColor = reactive({input$axisTicksColor}),
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),

        # guide_color
        barHeight      = reactive({ input$barHeight  }) ,
        barWidth       = reactive({ input$barWidth   }) ,
        barTicksColor    = reactive({ input$barTicksColor  }) ,
        barTicksLineWidth  = reactive({ input$barTicksLineWidth  }) ,
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
# mod_spiHeatmapOptions_ui("spiHeatmapOptions_1")

## To be copied in the server
# mod_spiHeatmapOptions_server("spiHeatmapOptions_1")
