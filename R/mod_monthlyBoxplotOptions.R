#' monthlyBoxplotOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_monthlyBoxplotOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 95%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),
      column(4, numericInput(ns("xAxisTextSize"), label = div("X-axis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Y-axis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 12)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), min=0, max=375, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = 1)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("Axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("yAxisTitleSize"), label = div("Y TitleSize", style="family:Georgia;text-align:left;font-size:75%"),  min=5, max=35, step=1, value = 16)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"),min=.1, max=7, step=.01, value = 1)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:75%"),  min=.5, max=7, step=.01, value = 1.75)),
      column(4, numericInput(ns("outlierShape"), label = div("Outlier Shape", style="family:Georgia;text-align:left;font-size:75%"),  min=0, max=25, step=1, value = 1)),
      column(4, numericInput(ns("outlierSize"), label = div("Outlier Size", style="family:Georgia;text-align:left;font-size:75%"), min=2, max=10, step=.01, value = 2.00)),
      column(4, numericInput(ns("boxplotAlpha"), label = div("Box. Alpha", style="family:Georgia;text-align:left;font-size:75%"),  min=0, max=1, step=1, value = .3)),
      column(4, numericInput(ns("yAxisNbreaks"), label = div("Y-axis Nbreaks", style="family:Georgia;text-align:left;font-size:75%"), min=3, max=15, step=1, value = 4)),
      column(4, numericInput(ns("stripTextSize"), label = div("Strip TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 13)),
      column(4, numericInput(ns("stripMarginBottom"), label = div("Strip MargB", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .20)),
      column(4, numericInput(ns("panelXspacing"), label = div("Panel X-space", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .10)),
      column(4, numericInput(ns("panelYspacing"), label = div("Panel Y-space", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .25)),
      column(4, numericInput(ns("facetsNrow"), label = div("Facets N-row", style="family:Georgia;text-align:left;font-size:75%"), min=3, max=12, step=1, value = 3)),
      column(4, numericInput(ns("boxplotsWidth"), label = div("Box. Width", style="family:Georgia;text-align:left;font-size:75%"),  min=.1, max=1, step=.01, value = .50)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis TextColor", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelGridLineType"), label = div("Grid LineType", style="family:Georgia;text-align:left;font-size:75%"), choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = 2)),
      column(4, selectInput(ns("panelGridColor"), label = div("Grid Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(4, selectInput(ns("coordFlip"), label = div("Transposer", style="family:Georgia;text-align:left;font-size:75%"),  choices = c("Oui" = TRUE, "Non" = FALSE), selected = FALSE)),
      column(4, selectInput(ns("outlierColor"), label = div("Outlier Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("boxplotColor"), label = div("Boxplots Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("boxplotFill"), label = div("Boxplots Fill", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "gray")),
      column(12, textInput(ns("yAxisTitle"), label = div("Y-Axis Title", style="family:Georgia;text-align:left;font-size:75%"), value = "Variable (unité)"))

    )
  )
}

#' monthlyBoxplotOptions Server Functions
#'
#' @noRd
mod_monthlyBoxplotOptions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
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
        boxplotsWidth = reactive({input$boxplotsWidth}),
        stripTextSize = reactive({input$stripTextSize}),
        stripMarginBottom = reactive({input$stripMarginBottom}),
        panelXspacing = reactive({input$panelXspacing}),
        panelYspacing = reactive({input$panelYspacing}),
        facetsNrow = reactive({input$facetsNrow})
      )
    )

  })
}

## To be copied in the UI
# mod_monthlyBoxplotOptions_ui("monthlyBoxplotOptions_1")

## To be copied in the server
# mod_monthlyBoxplotOptions_server("monthlyBoxplotOptions_1")
