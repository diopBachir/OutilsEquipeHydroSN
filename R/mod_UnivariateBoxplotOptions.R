#' UnivariateBoxplotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_UnivariateBoxplotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    fluidRow(

      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),

      column(4, selectInput(ns("xAxisTextSize"), label = div("X-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 12)),
      column(4, selectInput(ns("yAxisTextSize"), label = div("Y-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 12)),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis text color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), choices = seq(0, 360, 5), selected = 90)),
      column(4, selectInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = 1.00)),
      column(4, selectInput(ns("xAxisTextVjust"), label = div("Axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = .50)),

      column(4, selectInput(ns("yAxisTitleSize"), label = div("Y TitleSize", style="family:Georgia;text-align:left;font-size:75%"),  choices = seq(5, 35, 1), selected = 16)),
      column(4, selectInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.1, 7, .01), 2), selected = 1.00)),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("panelGridLineType"), label = div("Grid LineType", style="family:Georgia;text-align:left;font-size:75%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("panelGridColor"), label = div("Grid Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),

      column(4, selectInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.5, 7, .01), 2), selected = 2.7)),
      column(4, selectInput(ns("coordFlip"), label = div("Transposer", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Oui" = TRUE, "Non" = FALSE), selected = FALSE)),
      column(4, selectInput(ns("outlierShape"), label = div("Outlier Shape", style="family:Georgia;text-align:left;font-size:75%"),  choices = 0:25, selected = 1)),

      column(4, selectInput(ns("outlierColor"), label = div("Outlier Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("outlierSize"), label = div("Outlier Size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.2, 10, .01), 2), selected = 2.00)),
      column(4, selectInput(ns("boxplotColor"), label = div("Boxplots Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),

      column(4, selectInput(ns("boxplotFill"), label = div("Boxplots Fill", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("boxplotAlpha"), label = div("Box. Alpha", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(0, 1, .1), 1), selected = .3)),
      column(4, selectInput(ns("yAxisNbreaks"), label = div("Y-axis Nbreaks", style="family:Georgia;text-align:left;font-size:75%"), choices = seq(3, 15, 1), selected = 4)),

      column(4, selectInput(ns("boxplotsWidth"), label = div("Box. Width", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.1, 1, .01), 2), selected = .50)),
      column(8, textAreaInput(ns("yAxisTitle"), label = div("Y-Axis Title", style="family:Georgia;text-align:left;font-size:75%"), value = "Variable (unité)"))

    )
  )
}

#' UnivariateBoxplotOptions Server Functions
#'
#' @noRd
mod_UnivariateBoxplotOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(list(
      xAxisTextSize = reactive({input$xAxisTextSize}),
      yAxisTextSize = reactive({input$yAxisTextSize}),
      axisTextColor = reactive({input$axisTextColor}),
      xAxisTextAngle = reactive({input$xAxisTextAngle}),
      xAxisTextHjust = reactive({input$xAxisTextHjust}),
      yAxisTitleSize = reactive({input$yAxisTitleSize}),
      xAxisTextVjust = reactive({input$xAxisTextVjust}),
      axisTicksSize = reactive({input$axisTicksSize}),
      axisTicksColor = reactive({input$axisTicksColor}),
      panelGridLineType = reactive({input$panelGridLineType}),
      panelGridColor = reactive({input$panelGridColor}),
      panelBackgroundLineColor = reactive({input$panelBackgroundLineColor}),
      panelBackgroundLineSize = reactive({input$panelBackgroundLineSize}),
      coordFlip = reactive({input$coordFlip}),
      outlierShape = reactive({input$outlierShape}),
      outlierSize = reactive({input$outlierSize}),
      outlierColor = reactive({input$outlierColor}),
      boxplotColor = reactive({input$boxplotColor}),
      boxplotFill = reactive({input$boxplotFill}),
      boxplotAlpha = reactive({input$boxplotAlpha}),
      yAxisNbreaks = reactive({input$yAxisNbreaks}),
      yAxisTitle = reactive({input$yAxisTitle}),
      boxplotsWidth = reactive({input$boxplotsWidth})
    ))

  })
}

## To be copied in the UI
# mod_UnivariateBoxplotOptions_ui("UnivariateBoxplotOptions_1")

## To be copied in the server
# mod_UnivariateBoxplotOptions_server("UnivariateBoxplotOptions_1")
