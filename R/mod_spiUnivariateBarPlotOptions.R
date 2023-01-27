#' spiUnivariateBarPlotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spiUnivariateBarPlotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 90%; line-height: 32px;}"),

    fluidRow(

      column(4, numericInput(ns("xAxisTextSize"), label = div("Xaxis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 13)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Yaxis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 13)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"), min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:75%"),  min=.5, max=7, step=.01, value = 1.3)),
      column(4, numericInput(ns("xAxisBreakStep"), label = div("DateBreakStep", style="family:Georgia;text-align:left;font-size:75%"), min=1, max=10, step=1, value = 4)),
      column(4, numericInput(ns("barWidth"), label = div("Bar width", style="family:Georgia;text-align:left;font-size:75%"), min=0, max=1, step=.001, value = .5)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis Textcolor", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),


    )
  )
}

#' spiUnivariateBarPlotOptions Server Functions
#'
#' @noRd
mod_spiUnivariateBarPlotOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        xAxisTextSize = reactive({input$xAxisTextSize}),
        yAxisTextSize = reactive({input$yAxisTextSize}),
        axisTextColor = reactive({input$axisTextColor}),
        xAxisTextAngle = reactive({input$xAxisTextAngle}),
        xAxisTextHjust = reactive({input$xAxisTextHjust}),
        xAxisTextVjust = reactive({input$xAxisTextVjust}),
        xAxisBreakStep = reactive({input$xAxisBreakStep}),
        axisTicksSize = reactive({input$axisTicksSize}),
        axisTicksColor = reactive({input$axisTicksColor}),
        barWidth = reactive({input$barWidth}),
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize})
      )
    )

  })
}

## To be copied in the UI
# mod_spiUnivariateBarPlotOptions_ui("spiUnivariateBarPlotOptions_1")

## To be copied in the server
# mod_spiUnivariateBarPlotOptions_server("spiUnivariateBarPlotOptions_1")
