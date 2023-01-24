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

    fluidRow(

      column(4, selectInput(ns("xAxisTextSize"), label = div("X-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 12)),
      column(4, selectInput(ns("yAxisTextSize"), label = div("Y-axis text size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(5, 40, 1), 2), selected = 7)),
      column(4, selectInput(ns("axisTextColor"), label = div("Axis text color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),

      column(4, selectInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:75%"), choices = seq(0, 360, 5), selected = 90)),
      column(4, selectInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = .5)),
      column(4, selectInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(-5, 5, .01), 2), selected = .5)),

      column(4, selectInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:75%"),  choices = seq(5, 35, 1), selected = 16)),
      column(4, selectInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:75%"), choices = round(seq(.1, 7, .01), 2), selected = 1.00)),
      column(4, selectInput(ns("axisTicksColor"), label = div("Ticks Color", style="family:Georgia;text-align:left;font-size:75%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("Panel Color", style="family:Georgia;text-align:left;font-size:75%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:75%"),  choices = round(seq(.5, 7, .01), 2), selected = 1.3)),

      column(4, selectInput(ns("barHeight"), label = div("Bar Height", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(0.1, 75, .1), 2), selected = .9)),
      column(4, selectInput(ns("barWidth"), label = div("Bar Width", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(0.1, 50, .1), 2), selected = 45)),
      column(4, selectInput(ns("barTitleLoc"), label = div("Title Loc.", style="family:Georgia;text-align:left;font-size:75%"),   choices = c("top", "bottom", "left", "right"), selected = "top")),
      column(4, selectInput(ns("barTitleHjust"), label = div("Title hjust", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-2, 2, .1), 2), selected = .5)),
      column(4, selectInput(ns("barTitleVjust"), label = div("Title vjust", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-2, 2, .1), 2), selected = -1)),
      column(4, selectInput(ns("barTicksColor"), label = div("Ticks color", style="family:Georgia;text-align:left;font-size:75%"),   choices = colors(), selected = "black")),
      column(4, selectInput(ns("barTicksLineWidth"), label = div("Ticks width", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(0.1, 10, .1), 2), selected = 1)),
      column(4, selectInput(ns("barTitleSize"), label = div("Bar Title Text Size", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(5, 35, 1), 2), selected = 13)),
      column(4, selectInput(ns("barTitleMarginB"), label = div("Legend Title Margin B",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = .25)),
      column(4, selectInput(ns("barTitleMarginT"), label = div("Legend Title Margin T.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("barTitleMarginL"), label = div("Legend Title Margin L.", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("barTitleMarginR"), label = div("Legend Title Margin R.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginB"), label = div("Legend Text Marg. B.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginT"), label = div("Legend Text Marg. T.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = -.15)),
      column(4, selectInput(ns("legTextMarginL"), label = div("Legend Text Marg. L.", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legTextMarginR"), label = div("Legend Text Marg. R.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(4, selectInput(ns("LegTextSize"), label = div("Legend Text Size", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(5, 35, 1), 2), selected = 12)),
      column(4, selectInput(ns("LegTextColor"), label = div("Legend Text Color", style="family:Georgia;text-align:left;font-size:75%"),   choices = colors(), selected = "black")),
      column(4, selectInput(ns("xAxisBreakStep"), label = div("Date Break Step", style="family:Georgia;text-align:left;font-size:75%"), choices = 1:10, selected = 4)),
      # la légende
      column(4, selectInput(ns("legMarginB"), label = div("Legend Margin B.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = .05)),
      column(4, selectInput(ns("legMarginT"), label = div("Legend Margin T.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legMarginL"), label = div("Legend Margin L.", style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),
      column(4, selectInput(ns("legMarginR"), label = div("Legend Margin R.",style="family:Georgia;text-align:left;font-size:75%"),   choices = round(seq(-5, 5, .01), 2), selected = 0)),

      column(12, textInput(ns("legTitleName"), label = div("Titre de la légende", style="family:Georgia;text-align:left;font-size:100%"), value = "% Lacunes"))
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
