#' dailyFacetsInventoryHeatmapOptions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dailyFacetsInventoryHeatmapOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css', ".selectize-input { font-size: 80%; line-height: 32px;}"),

    fluidRow(
      column(12, h4("GGPLOT -- [Configuration||Options]", style="color:#3474A7;family:Georgia;text-align:center;")),
      column(4, numericInput(ns("xAxisTextSize"), label = div("X-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"),min=5, max=40, step=1, value = 9)),
      column(4, numericInput(ns("yAxisTextSize"), label = div("Y-axis TextSize", style="family:Georgia;text-align:left;font-size:65%"),min=5, max=40, step=1, value = 7)),
      column(4, numericInput(ns("xAxisTextAngle"), label = div("X-axis Angle", style="family:Georgia;text-align:left;font-size:65%"), min=0, max=360, step=5, value = 90)),
      column(4, numericInput(ns("xAxisTextHjust"), label = div("X-axis Hjust", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("xAxisTextVjust"), label = div("X-axis Vjust", style="family:Georgia;text-align:left;font-size:65%"),min=-5, max=5, step=.01, value = .5)),
      column(4, numericInput(ns("AxisTitleSize"), label = div("Axis TitleSize", style="family:Georgia;text-align:left;font-size:65%"),  min=5, max=35, step=1, value = 16)),
      column(4, numericInput(ns("axisTicksSize"), label = div("Ticks Size", style="family:Georgia;text-align:left;font-size:65%"), min=.1, max=7, step=.01, value = 1.00)),
      column(4, numericInput(ns("panelBackgroundLineSize"), label = div("Panel Size", style="family:Georgia;text-align:left;font-size:65%"), min=.5, max=7, step=.01, value = 1.3)),
      column(4, numericInput(ns("barHeight"), label = div("Bar Height", style="family:Georgia;text-align:left;font-size:65%"),  min=0.1, max=65, step=.1, value = .9)),
      column(4, numericInput(ns("barWidth"), label = div("Bar Width", style="family:Georgia;text-align:left;font-size:65%"),  min=0.1, max=50, step=.1, value = 45)),
      column(4, numericInput(ns("barTitleHjust"), label = div("Title hjust", style="family:Georgia;text-align:left;font-size:65%"),  min=-2, max=2, step=.1, value = .5)),
      column(4, numericInput(ns("barTitleVjust"), label = div("Title vjust", style="family:Georgia;text-align:left;font-size:65%"),  min=-2, max=2, step=.1, value = -1)),
      column(4, numericInput(ns("barTicksLineWidth"), label = div("Axis Ticks LineWidth", style="family:Georgia;text-align:left;font-size:65%"),  min=0.1, max=10, step=.1, value = 1)),
      column(4, numericInput(ns("barTitleSize"), label = div("BarColour Title TextSize", style="family:Georgia;text-align:left;font-size:65%"),  min=5, max=35, step=1, value = 13)),
      column(4, numericInput(ns("barTitleMarginB"), label = div("Legend TitleMarginB",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = -.04)),
      column(4, numericInput(ns("barTitleMarginT"), label = div("Legend TitleMarginT.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("barTitleMarginL"), label = div("Legend TitleMarginL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),

      column(4, numericInput(ns("barTitleMarginR"), label = div("Legend TitleMarginR.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginB"), label = div("Legend TextMargB.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginT"), label = div("Legend TextMargT.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = -.15)),
      column(4, numericInput(ns("legTextMarginL"), label = div("Legend TextMargL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legTextMarginR"), label = div("Legend TextMargR.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("LegTextSize"), label = div("Legend Labels TextSize", style="family:Georgia;text-align:left;font-size:65%"),  min=5, max=35, step=1, value = 12)),
      column(4, numericInput(ns("xAxisBreakStep"), label = div("Xaxis Date Break Step", style="family:Georgia;text-align:left;font-size:65%"),min=1, max=10, step=1, value = 4)),
      # la légende
      column(4, numericInput(ns("legMarginB"), label = div("Legend MarginB.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = .05)),
      column(4, numericInput(ns("legMarginT"), label = div("Legend MarginT.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginL"), label = div("Legend MarginL.", style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("legMarginR"), label = div("Legend MarginR.",style="family:Georgia;text-align:left;font-size:65%"),  min=-5, max=5, step=.01, value = 0)),
      column(4, numericInput(ns("facetNrow"), label = div("Nrow Facets",style="family:Georgia;text-align:left;font-size:65%"),   min=1, max=10, step=1, value = 1)),
      column(4, numericInput(ns("plotTitleSize"), label = div("Graph TitleSize",style="family:Georgia;text-align:left;font-size:65%"),min=5, max=40, step=1, value = 11)),
      column(12, tags$hr(style="border-color:gray;")),
      column(4, selectInput(ns("axisTextColor"), label = div("AxisTextColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("axisTicksColor"), label = div("TicksColor", style="family:Georgia;text-align:left;font-size:65%"), choices = colors(), selected = "black")),
      column(4, selectInput(ns("panelBackgroundLineColor"), label = div("PanelColor", style="family:Georgia;text-align:left;font-size:65%"),  choices = colors(), selected = "black")),
      column(4, selectInput(ns("barTitleLoc"), label = div("Title Loc.", style="family:Georgia;text-align:left;font-size:65%"),   choices = c("top", "bottom", "left", "right"), selected = "top")),
      column(4, selectInput(ns("LegTextColor"), label = div("Leg. TextColor", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
      column(4, selectInput(ns("barTicksColor"), label = div("BarTicksColor", style="family:Georgia;text-align:left;font-size:65%"),   choices = colors(), selected = "black")),
      column(4, selectInput(ns("plotTitleAlign"), label = div("Graph TitleAlign",style="family:Georgia;text-align:left;font-size:65%"),   choices = c("left"=0, "center"=.425, "right"=1), selected = .425)),
      column(8, textInput(ns("legTitleName"), label = div("Titre de la légende", style="family:Georgia;text-align:left;font-size:75%"), value = "% Lacunes"))
    )
  )
}

#' dailyFacetsInventoryHeatmapOptions Server Functions
#'
#' @noRd
mod_dailyFacetsInventoryHeatmapOptions_server <- function(id){
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
        legTitleName   = reactive({ input$legTitleName }),

        facetNrow    = reactive({ input$facetNrow }) ,
        plotTitleAlign   = reactive({ input$plotTitleAlign }) ,
        plotTitleSize    = reactive({ input$plotTitleSize })
      )
    )
  })
}

## To be copied in the UI
# mod_dailyFacetsInventoryHeatmapOptions_ui("dailyFacetsInventoryHeatmapOptions_1")

## To be copied in the server
# mod_dailyFacetsInventoryHeatmapOptions_server("dailyFacetsInventoryHeatmapOptions_1")
