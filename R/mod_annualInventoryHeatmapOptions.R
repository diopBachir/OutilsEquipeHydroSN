#' annualInventoryHeatmapOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annualInventoryHeatmapOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
<<<<<<< HEAD
    tags$style(type='text/css', ".selectize-input { font-size: 96%; line-height: 32px;}"),

    column(12, h4("GGPLOT [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),
=======
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),
>>>>>>> a014fec39e3ca8f56557472a648d7c907fdd55a9

    fluidRow(

      column(4, numericInput(ns("xAxisTextSize"), label = div("Xaxis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Yaxis TextSize", style="family:Georgia;text-align:left;font-size:65%"), min=5, max=40, step=1, value = 7)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:65%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"),  min=.5, max=7, step=.01, value = 1.3)),
      column(4, numericInput(ns("legTextMarginB"), label = div("Leg. TextMargB.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"), label = div("Leg. TextMargT.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginL"), label = div("Leg. TextMargL.", style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginR"), label = div("Leg. TextMargR.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 1.50)),
      column(4, numericInput(ns("LegTextSize"), label = div("Leg. TextSize", style="family:Georgia;text-align:left;font-size:65%"),   min=5, max=35, step=1, value = 12)),
      column(4, numericInput(ns("xAxisBreakStep"), label = div("DateBreakStep", style="family:Georgia;text-align:left;font-size:65%"), min=1, max=10, step=1, value = 4)),
      column(4, numericInput(ns("legMarginB"), label = div("Leg. MarginB.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = .05)),
      column(4, numericInput(ns("legMarginT"), label = div("Leg. MarginT.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("legMarginL"), label = div("Leg. MarginL.", style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("legMarginR"), label = div("Leg. MarginR.",style="family:Georgia;text-align:left;font-size:65%"),   min=-5, max=5, step=.01, value = 0)),
      column(12, tags$hr(style="border-color:gray;")),
      column(6, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(6, selectInput(ns("LegTextColor"), label = div("Leg. TextColor", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
      column(6, selectInput(ns("axisTextColor"), label = div("Axis TextSize", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(6, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      # la légende
      column(12, textInput(ns("legTitleName"), label = div("Titre de la légende", style="family:Georgia;text-align:left;font-size:75%"), value = "% Lacunes"))
    )
  )
}

#' annualInventoryHeatmapOptions Server Functions
#'
#' @noRd
mod_annualInventoryHeatmapOptions_server <- function(id){
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
        panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
        panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),

        # theme
        LegTextSize			= reactive({ input$LegTextSize }) ,
        LegTextColor		= reactive({ input$LegTextColor }) ,
        legTextMarginB		= reactive({ input$legTextMarginB  }) ,
        legTextMarginT		= reactive({ input$legTextMarginT  }) ,
        legTextMarginL		= reactive({ input$legTextMarginL  }) ,
        legTextMarginR		= reactive({ input$legTextMarginR }) ,
        legMarginB			= reactive({ input$legMarginB }) ,
        legMarginT			= reactive({ input$legMarginT }) ,
        legMarginL			= reactive({ input$legMarginL }) ,
        legMarginR			= reactive({ input$legMarginR }) ,
        legTitleName		= reactive({ input$legTitleName })
      )
    )
  })
}

## To be copied in the UI
# mod_annualInventoryHeatmapOptions_ui("annualInventoryHeatmapOptions_1")

## To be copied in the server
# mod_annualInventoryHeatmapOptions_server("annualInventoryHeatmapOptions_1")
