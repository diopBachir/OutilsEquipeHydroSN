#' annualFacetsInventoryHeatmapOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annualFacetsInventoryHeatmapOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 92%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),
      column(4, numericInput(ns("xAxisTextSize"), label = div("Xaxis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 9)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Yaxis TextSize", style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 7)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), min=-5, max=5, step=.01, value = .50)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"), min=.1, 7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Contour Size", style="family:Georgia;text-align:left;font-size:75%"),  min=.5, max=7, step=.01, value = 1.3)),
      column(4, numericInput(ns("legTextMarginB"), label = div("Legend Text Marg. B.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"), label = div("Legend Text Marg. T.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginL"), label = div("Legend Text Marg. L.", style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginR"), label = div("Legend Text Marg. R.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 1.50)),
      column(4, numericInput(ns("LegTextSize"), label = div("Legend TextSize", style="family:Georgia;text-align:left;font-size:75%"),   min=5, max=35, step=1, value = 12)),
      column(4, numericInput(ns("xAxisBreakStep"), label = div("Date Break Step", style="family:Georgia;text-align:left;font-size:75%"), min=1, max=10, step=1, value = 4)),
      column(4, numericInput(ns("legMarginB"), label = div("Leg. Margin B.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = .2)),
      column(4, numericInput(ns("legMarginT"), label = div("Leg. Margin T.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = .4)),
      column(6, numericInput(ns("legMarginL"), label = div("Leg. Margin L.", style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 1.50)),
      column(6, numericInput(ns("legMarginR"), label = div("Leg. Margin R.",style="family:Georgia;text-align:left;font-size:75%"),   min=-5, max=5, step=.01, value = 0)),
      column(6, numericInput(ns("facetNrow"), label = div("Facts Nrow",style="family:Georgia;text-align:left;font-size:75%"),    min=1, max=10, step=1, value = 1)),
      column(6, numericInput(ns("plotTitleSize"), label = div("Taille des Titres de Graph",style="family:Georgia;text-align:left;font-size:75%"), min=5, max=40, step=1, value = 11)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis TextColor", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("TicksColor", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("PanelColor", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(6, selectInput(ns("LegTextColor"), label = div("Legend TextColor", style="family:Georgia;text-align:left;font-size:75%"),   choices = colors(), selected = "black")),
      column(6, selectInput(ns("plotTitleAlign"), label = div("Graph TitleAlign",style="family:Georgia;text-align:left;font-size:75%"),   choices = c("left"=0, "center"=.425, "right"=1), selected = .425)),
      column(12, textInput(ns("legTitleName"), label = div("Titre de la légende", style="family:Georgia;text-align:left;font-size:100%"), value = "% Lacunes"))
    )
  )
}

#' annualFacetsInventoryHeatmapOptions Server Functions
#'
#' @noRd
mod_annualFacetsInventoryHeatmapOptions_server <- function(id){
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
        legTitleName		= reactive({ input$legTitleName }) ,

        facetNrow		= reactive({ input$facetNrow }) ,
        plotTitleAlign		= reactive({ input$plotTitleAlign }) ,
        plotTitleSize		= reactive({ input$plotTitleSize })
      )
    )

  })
}

## To be copied in the UI
# mod_annualFacetsInventoryHeatmapOptions_ui("annualFacetsInventoryHeatmapOptions_1")

## To be copied in the server
# mod_annualFacetsInventoryHeatmapOptions_server("annualFacetsInventoryHeatmapOptions_1")
