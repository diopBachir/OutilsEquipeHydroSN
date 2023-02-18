#' unique_periode_time_serie_interpolation_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_unique_periode_time_serie_interpolation_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '

            .shiny-input-container {
                color: gray;
            }
            .textBoxed {
                display:block;
                padding:1.5px;
                margin:0 0 0px;
                margin-top:0px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:0px;
                font-family:georgia;
            }
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
                margin-top: -0.3em;
                line-height: 1.5em;
                vertical-align: text-bottom;
                box-sizing: border-box;
            }
            .loading.dots::after {
                text-rendering: geometricPrecision;
                content: "⠋\\A⠙\\A⠹\\A⠸\\A⠼\\A⠴\\A⠦\\A⠧\\A⠇\\A⠏";
                animation: spin10 1s steps(10) infinite;
                animation-duration: 1s;
                animation-timing-function: steps(10);
                animation-delay: 0s;
                animation-iteration-count: infinite;
                animation-direction: normal;
                animation-fill-mode: none;
                animation-play-state: running;
                animation-name: spin10;
            }
            .loading::after {
                display: inline-table;
                white-space: pre;
                text-align: left;
            }
            .form-group { margin-bottom: 0 !important; }
            .form-label-top { margin-bottom: 0 !important; }
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')),


    fluidRow(align="center",
             column(2, actionButton(ns("north"), label = div(tags$strong("Flèche Nord"), style="font-size:75%;text-align:center"), icon = icon("compass"), width = "100%")),
             column(2, actionButton(ns("scale"), label =  div(tags$strong("Barre D'échelle"), style="font-size:75%;text-align:center"), icon = icon("barcode"), width = "100%")),
             column(2, actionButton(ns("layers"), label =  div(tags$strong("Couches"), style="font-size:75%;text-align:center"), icon = icon("layer-group"), width = "100%")),
             column(2, actionButton(ns("colorbar"), label =  div(tags$strong("Légende"), style="font-size:75%;text-align:center"), icon = icon("elementor"), width = "100%")),
             column(2, actionButton(ns("legendtheme"), label =  div(tags$strong("Theme Légende"), style="font-size:75%;text-align:center"), icon = icon("affiliatetheme"), width = "100%")),
             column(2, actionButton(ns("axistheme"), label =  div(tags$strong("Theme Axes"), style="font-size:75%;text-align:center"), icon = icon("affiliatetheme"), width = "100%"))
    ),
    tags$hr(style="border-color:gray;"),
    fluidRow(align = "center",
             column(3, dipsaus::actionButtonStyled(ns("idw"), span("Interpolation", id=ns("idwAnimate")), icon = icon("buromobelexperte"), class= "", type="primary")),
             column(3, dipsaus::actionButtonStyled(ns("map"), span("Cartographie", id=ns("mapAnimate")), icon = icon("map"), class= "", type="primary")),
             column(3, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(3, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),
    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
      column(6,  uiOutput(ns("isolinesCheckBoxGroup"), width="100%")),
      column(6,  uiOutput(ns("barLegendRoundingSlider"), width="100%")),
      column(12, plotOutput(ns("interpolationResult"), width="100%"))
    )
  )
}

#' unique_periode_time_serie_interpolation_map Server Functions
#'
#' @noRd
mod_unique_periode_time_serie_interpolation_map_server <- function(id, bassin, stations, interpolationData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #| DEFINITION DES OPTIONS

    northOptions <- reactiveValues()
    scaleOptions <- reactiveValues()
    layersOptions <- reactiveValues()
    colorbarOptions <- reactiveValues()
    legendThemeOptions <- reactiveValues()
    axisThemeOptions <- reactiveValues()

    # Légende ---------------------------------------------------------------------------------------------------------------------------|
    # initial options
    northOptions$northArrowLocation <- "tl"
    northOptions$northArrowWidth <- 1.5
    northOptions$northArrowHeight <- 1.5
    northOptions$northArrowPadx <- -.25
    northOptions$northArrowPady <- .25

    observeEvent(input$north, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3('Manipulation De La Flèche Nord De la Carte', style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(6, sliderInput(ns("northArrowWidth"), label = "Largeur", min = -10, max = 10, value =  northOptions$northArrowWidth, step = .1, width = "100%")),
          column(6, sliderInput(ns("northArrowHeight"), label = "Hauteur", min = -10, max = 10, value = northOptions$northArrowHeight, step = .1)),
          column(4, sliderInput(ns("northArrowPadx"), label = "Padx",  min = -5, max = 5, value = northOptions$northArrowPadx, step = .01)),
          column(4,selectInput(
            ns("northArrowLocation"), label = "Position", choices = c(
              "Bas-Gauche" = "bl", "Bas-Droit" = "br","Haut-Gauche" = "tl", "Haut-Droit" = "tr"
            ), selected = northOptions$northArrowLocation)
          ),
          column(4, sliderInput(ns("northArrowPady"), label = "Pady",  min = -5, max = 5, value = northOptions$northArrowPady, step = .01)),
        ),
        footer=tagList(
          actionButton(ns("submitLeg"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))
    })
    # only store the information if the user clicks submit
    observeEvent(input$submitLeg, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      northOptions$northArrowLocation <-  input$northArrowLocation
      northOptions$northArrowWidth <-  input$northArrowWidth
      northOptions$northArrowHeight <-  input$northArrowHeight
      northOptions$northArrowPadx <-  input$northArrowPadx
      northOptions$northArrowPady <-  input$northArrowPady
      removeModal()
    })

    # Barre d'échelle -----------------------------------------------------------------------------------------------------------------|
    # initial options
    scaleOptions$scaleWidthHint <- .13
    scaleOptions$scaleTickHeight <- .30
    scaleOptions$scaleLocation <- "bl"
    scaleOptions$scaleTextCex <- 1.15
    scaleOptions$scaleStyle <- "ticks"

    observeEvent(input$scale, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Manipulation De La Barre D'échelle De la Carte", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(6, sliderInput(ns("scaleWidthHint"), label = "Largeur", min = .01, max = 10, value = scaleOptions$scaleWidthHint, step = .01, width = "100%")),
          column(6, sliderInput(ns("scaleTickHeight"), label = "Hauteur des Ticks", min = .01, max = 10, value = scaleOptions$scaleTickHeight, step = .01, width = "100%")),
          column(4,selectInput(
            ns("scaleLocation"), label = "Position", choices = c(
              "Bas-Gauche" = "bl", "Bas-Droit" = "br","Haut-Gauche" = "tl", "Haut-Droit" = "tr"
            ), selected = scaleOptions$scaleLocation)
          ),
          column(4, sliderInput(ns("scaleTextCex"), label = "Taille de Police",  min = .01, max = 10, value = scaleOptions$scaleTextCex, step = .01)),
          column(4,selectInput(
            ns("scaleStyle"), label = "Style de la barre", choices = c("Barre" = "bar", "Ticks" = "ticks"), selected = scaleOptions$scaleStyle
          )),
        ),
        footer=tagList(
          actionButton(ns("submitScale"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitScale, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      scaleOptions$scaleWidthHint <- input$scaleWidthHint
      scaleOptions$scaleTickHeight <- input$scaleTickHeight
      scaleOptions$scaleLocation <- input$scaleLocation
      scaleOptions$scaleTextCex <- input$scaleTextCex
      scaleOptions$scaleStyle <- input$scaleStyle
      removeModal()
    })

    # Couches -----------------------------------------------------------------------------------------------------------------|
    # init options
    layersOptions$bvContSize <- 1.40
    layersOptions$IsolineSize <- .80
    layersOptions$IsolineFontSize <- 3.5
    layersOptions$IsolineLabelNudgeX <- 0
    layersOptions$IsolineLabelNudgeY <- 0
    layersOptions$IsolineLabelAlpha <- .35
    layersOptions$bvContColor <- "black"
    layersOptions$IsolineColor <- "black"
    layersOptions$IsolineType <- "2"
    layersOptions$IsolineLabelColor <- 'black'
    layersOptions$IsolineLabelFill <- "gray"

    observeEvent(input$layers, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Manipulation Des Couches De la Carte", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("bvContSize"), label = "Largeur Du Trait de Contour Du Bassin", min = 0.01, max = 10, value = layersOptions$bvContSize, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineSize"), label = "Epaisseur Des Isolignes", min = 0, max = 10, value = layersOptions$IsolineSize, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineLabelAlpha"), label = "Transparence Des Etiquettes Isolignes", min = 0, max = 1, value = layersOptions$IsolineLabelAlpha, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineFontSize"), label = "Taille De police Des Etiquettes D'isolignes", min = 1, max = 10, value = layersOptions$IsolineFontSize, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineLabelNudgeX"), label = "Décalage Horizontal Des Etiquettes D'isolignes", min = -5, max = 5, value = layersOptions$IsolineLabelNudgeX, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineLabelNudgeY"), label = "Décalage Vertical Des Etiquettes D'isolignes", min = -5, max = 5, value = layersOptions$IsolineLabelNudgeY, step = .01, width = "100%")),

          column(4,selectInput(
            ns("bvContColor"), label = "Couleur Contour Bassin", choices = colors(), selected = layersOptions$bvContColor, width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineColor"), label = "Couleur Des Isolignes", choices = colors(), selected = layersOptions$IsolineColor, width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineType"), label = "Style De Lignes Isolignes", choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = layersOptions$IsolineType, width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineLabelColor"), label = "Couleur Des Isolignes", choices = colors(), selected = layersOptions$IsolineLabelColor, width = "100%")
          ),

          column(4,selectInput(
            ns("IsolineLabelFill"), label = "Couleur De Fond Des Isolignes", choices = colors(), selected = layersOptions$IsolineLabelFill, width = "100%")
          )
        ),
        footer=tagList(
          actionButton(ns("submitLayers"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitLayers, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      layersOptions$bvContSize <- input$bvContSize
      layersOptions$IsolineSize <- input$IsolineSize
      layersOptions$IsolineFontSize <- input$IsolineFontSize
      layersOptions$IsolineLabelNudgeX <- input$IsolineLabelNudgeX
      layersOptions$IsolineLabelNudgeY <- input$IsolineLabelNudgeY
      layersOptions$IsolineLabelAlpha <- input$IsolineLabelAlpha
      layersOptions$bvContColor <- input$bvContColor
      layersOptions$IsolineColor <- input$IsolineColor
      layersOptions$IsolineType <- input$IsolineType
      layersOptions$IsolineLabelColor <- input$IsolineLabelColor
      layersOptions$IsolineLabelFill <- input$IsolineLabelFill
      removeModal()
    })

    # Barre de légende -----------------------------------------------------------------------------------------------------------------|
    # init options
    colorbarOptions$barHeight <- 1.1
    colorbarOptions$barWidth <- 30
    colorbarOptions$barTitleHjust <- .5
    colorbarOptions$barTitleVjust <- .5
    colorbarOptions$barTicksLineWidth <- .7
    colorbarOptions$legTitleAngle <- 0
    colorbarOptions$barTitleSize <- 13
    colorbarOptions$barTitleMarginT <- 0
    colorbarOptions$barTitleMarginR <- 0
    colorbarOptions$barTitleMarginB <- -.1
    colorbarOptions$barTitleMarginL <- 0
    colorbarOptions$barTitleLoc <- "top"
    colorbarOptions$barTicksColor <- "gray"

    observeEvent(input$colorbar, {
      showModal(modalDialog(
        tags$h3("Manipulation De La Barre De Légende [Echelle Numérique]", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("barHeight"), label = "Hauteur", min = 0.1, max = 50, value = colorbarOptions$barHeight, step = .1, width = "100%")),
          column(4, sliderInput(ns("barWidth"), label = "Largeur", min = 0.01, max = 50, value = colorbarOptions$barWidth, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleHjust"), label = "Alignement Horizontal", min = -2, max = 2, value = colorbarOptions$barTitleHjust, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleVjust"), label = "Alignement Vertical", min = -2, max = 2, value = colorbarOptions$barTitleVjust, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTicksLineWidth"), label = "Epaisseur des Graduations", min = .1, max = 10, value = colorbarOptions$barTicksLineWidth, step = .1, width = "100%")),
          column(4, sliderInput(ns("barTitleSize"), label = "Taille de la Police", min = 5, max = 50, value = colorbarOptions$barTitleSize, step = 1, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginT"), label = "Marge Supérieure", min = -5, max = 5, value = colorbarOptions$barTitleMarginT, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginR"), label = "Marge Droite", min = -5, max = 5, value = colorbarOptions$barTitleMarginR, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginB"), label = "Marge Inférieure", min = -5, max = 5, value = colorbarOptions$barTitleMarginB, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginL"), label = "Marge Gauche", min = -5, max = 5, value = colorbarOptions$barTitleMarginL, step = .01, width = "100%")),

          column(4,selectInput(ns("barTitleLoc"), label = "Position",  choices = c("En Haut"="top", "En Bas"="bottom", "A Gauche"="left", "A Droite"="right"), selected = colorbarOptions$barTitleLoc, width = "100%")),
          column(4, sliderInput(ns("legTitleAngle"), label = "Orientation Du titre De la Légende", min = 0, max = 360, value = colorbarOptions$legTitleAngle, step = .01, width = "100%")),
          column(4,selectInput(ns("barTicksColor"), label = "Couleur Des Graduations",  choices = colors(), selected = colorbarOptions$barTicksColor)),

        ),
        footer=tagList(
          actionButton(ns("submitColorBar"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitColorBar, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      colorbarOptions$barHeight <- input$barHeight
      colorbarOptions$barWidth <- input$barWidth
      colorbarOptions$legTitleAngle <- input$legTitleAngle
      colorbarOptions$barTitleHjust <- input$barTitleHjust
      colorbarOptions$barTitleVjust <- input$barTitleVjust
      colorbarOptions$barTicksLineWidth <- input$barTicksLineWidth
      colorbarOptions$barTitleSize <- input$barTitleSize
      colorbarOptions$barTitleMarginT <- input$barTitleMarginT
      colorbarOptions$barTitleMarginR <- input$barTitleMarginR
      colorbarOptions$barTitleMarginB <- input$barTitleMarginB
      colorbarOptions$barTitleMarginL <- input$barTitleMarginL
      colorbarOptions$barTitleLoc <- input$barTitleLoc
      colorbarOptions$barTicksColor <- input$barTicksColor
      removeModal()
    })

    # Etiquettes De La Légende -----------------------------------------------------------------------------------------------------------------|
    # init options
    legendThemeOptions$legTitleAngle <- 0
    legendThemeOptions$legTexAngle <- 0
    legendThemeOptions$LegTextSize <- 12
    legendThemeOptions$legTextMarginT <-  -.15
    legendThemeOptions$legTextMarginR <- 0
    legendThemeOptions$legTextMarginB <- 0
    legendThemeOptions$legTextMarginL <- 0
    legendThemeOptions$legMarginT <- 0
    legendThemeOptions$legMarginR <- 0
    legendThemeOptions$legMarginB <- .1
    legendThemeOptions$legMarginL <- 0
    legendThemeOptions$legDir <- "horizontal"
    legendThemeOptions$LegTextColor <- "black"
    legendThemeOptions$legLoc <- "bottom"
    legendThemeOptions$legPalette <- "temperature"
    legendThemeOptions$legTitleName <- "Variable Interpolée (unité)"

    observeEvent(input$legendtheme, {
      showModal(modalDialog(
        tags$h3("Configuration Du Thème De La Légende", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("legTitleAngle"), label = "Orientation du Titre", min = 0, max = 360, value = legendThemeOptions$legTitleAngle, step = 45, width = "100%")),
          column(4, sliderInput(ns("legTexAngle"), label = "Orientation Des Etiquettes", min = 0, max = 360, value = legendThemeOptions$legTexAngle, step = 45, width = "100%")),
          column(4, sliderInput(ns("LegTextSize"), label = "Taille de Police Des Etiquettes", min = 5, max = 35, value = legendThemeOptions$LegTextSize, step = 1, width = "100%")),

          column(4, sliderInput(ns("legTextMarginT"), label = "Marge Supérieure Des Etiquettes", min = -5, max = 5, value = legendThemeOptions$legTextMarginT, step = .01, width = "100%")),
          column(4, sliderInput(ns("legTextMarginR"), label = "Marge Droite Des Etiquettes", min = -5, max = 5, value = legendThemeOptions$legTextMarginR, step = .01, width = "100%")),
          column(4, sliderInput(ns("legTextMarginB"), label = "Marge Inférieure Des Etiquettes", min = -5, max = 5, value = legendThemeOptions$legTextMarginB, step = .01, width = "100%")),

          column(4, sliderInput(ns("legTextMarginL"), label = "Marge Gauche Des Etiquettes", min = -5, max = 5, value = legendThemeOptions$legTextMarginL, step = .01, width = "100%")),
          column(4, sliderInput(ns("legMarginT"), label = "Marge Supérieure De La Légende", min = -5, max = 5, value = legendThemeOptions$legMarginT, step = .01, width = "100%")),
          column(4, sliderInput(ns("legMarginR"), label = "Marge Droite De La Légende", min = -5, max = 5, value = legendThemeOptions$legMarginR, step = .01, width = "100%")),

          column(4,selectInput(ns("legDir"), label = "Direction De La Légende",  choices = c("Horizontale" = "horizontal", "Verticale" = "vertical"), selected = legendThemeOptions$legDir, width = "100%")),
          column(4, sliderInput(ns("legMarginB"), label = "Marge Inférieure De La Légende", min = -5, max = 5, value = legendThemeOptions$legMarginB, step = .01, width = "100%")),
          column(4, selectInput(ns("LegTextColor"), label = "Couleur Des Etiquettes", choices = colors(), selected = legendThemeOptions$LegTextColor, width = "100%"))
        ),
        fluidRow(
          column(4,selectInput(ns("legPalette"), label = "Palette de Couleur", choices = c("Température" = "temperature", "Blues" = "blues"), selected = legendThemeOptions$legPalette, width = "100%")),
          column(4,sliderInput(ns("legMarginL"), label = "Marge Gauche De La Légende", min = -5, max = 5, value = legendThemeOptions$legMarginL, step = .01, width = "100%")),
          column(4,selectInput(ns("legLoc"), label = "Position De La Légende", choices = c("En Haut"="top", "En Bas"="bottom", "A Gauche"="left", "A Droite"="right"), selected = legendThemeOptions$legLoc, width = "100%")),

          column(12,textInput(ns("legTitleName"), label = "Titre De La Légende", value = legendThemeOptions$legTitleName, placeholder = "Titre légende...", width = "100%")),
        ),
        footer=tagList(
          actionButton(ns("submitLegendTheme"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitLegendTheme, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      legendThemeOptions$legTitleAngle <- input$legTitleAngle
      legendThemeOptions$legTexAngle <- input$legTexAngle
      legendThemeOptions$LegTextSize <- input$LegTextSize
      legendThemeOptions$legTextMarginT <- input$legTextMarginT
      legendThemeOptions$legTextMarginR <- input$legTextMarginR
      legendThemeOptions$legTextMarginB <- input$legTextMarginB
      legendThemeOptions$legTextMarginL <- input$legTextMarginL
      legendThemeOptions$legMarginT <- input$legMarginT
      legendThemeOptions$legMarginR <- input$legMarginR
      legendThemeOptions$legMarginB <- input$legMarginB
      legendThemeOptions$legMarginL <- input$legMarginL
      legendThemeOptions$legDir <- input$legDir
      legendThemeOptions$LegTextColor <- input$LegTextColor
      legendThemeOptions$legLoc <- input$legLoc
      legendThemeOptions$legPalette <- input$legPalette
      legendThemeOptions$legTitleName <- input$legTitleName
      removeModal()
    })

    # Theme Des Axes -----------------------------------------------------------------------------------------------------------------|
    # init options
    axisThemeOptions$xAxisTextSize <- 12
    axisThemeOptions$axisTextColor <- "black"
    axisThemeOptions$yAxisTextSize <- 12

    observeEvent(input$axistheme, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Configuration Du Thème Des Etiquettes Des Axes", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("xAxisTextSize"), label = "Taille de Police Axe-X", min = 5, max = 35, value = axisThemeOptions$xAxisTextSize, step = 1, width = "100%")),
          column(4,selectInput(ns("axisTextColor"), label = "Couleur Des Etiquettes", choices = colors(), selected = axisThemeOptions$axisTextColor)),
          column(4, sliderInput(ns("yAxisTextSize"), label = "Taille de Police Axe-Y", min = 5, max = 35, value = axisThemeOptions$yAxisTextSize, step = 1, width = "100%"))
        ),
        footer=tagList(
          actionButton(ns("submitAxisTheme"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitAxisTheme, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      axisThemeOptions$xAxisTextSize <- input$xAxisTextSize
      axisThemeOptions$axisTextColor <- input$axisTextColor
      axisThemeOptions$yAxisTextSize <- input$yAxisTextSize
      removeModal()
    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #| CARTOGRAPHIE
    # mise en place~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    bv_wgs84<- reactive({
      shiny::req(bassin)
      bassin
    })

    # gridded cell points in WGS84-4326 proj
    station.in.wgs84<-  reactive(({
      shiny::req(stations)
      prec_grid_georef(stations, "+init=epsg:4326")[[1]]
    }))


    # Interpolation data georeferenced in UTM
    interpolationData.WGS<- reactive({
      shiny::req(meanPeriod(), station.in.wgs84())
      # shiny::req(interpolationData, station.in.wgs84())
      data_cleaning(meanPeriod(), station.in.wgs84())
    })

    ## Summarising data
    meanPeriod<- reactive({
      shiny::req(interpolationData)
      transpose_df(
        interpolationData %>%
          dplyr::select(-Date) %>%
          tidyr::pivot_longer(
            1:ncol(interpolationData)-1, names_to = "Station", values_to = "Valeur"
          ) %>%
          dplyr::group_by(Station) %>%
          dplyr::summarise(
            Moyenne = mean(Valeur)
          ) %>%
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
          )
      ) %>%
        dplyr::rename(Station = 1) %>%
        dplyr::mutate(across(-1, as.numeric, .names = "{.col}"))

    })

    # interpolation et nettoyage ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    cleanData<- reactiveVal()

    observeEvent(ignoreInit = T, ignoreNULL = T,  input$idw,  {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")

      shiny::req(station.in.wgs84(), interpolationData.WGS())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "idwAnimate", class = "loading dots")
      shinyjs::disable("idw")

      # Notification
      id <- showNotification(
        "Traitement en cours ...", duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      #------------------------------------------------------------------------------#
      # GRILLE D4INTERPOLATION

      # definition of WGS84 (epsg:4326):
      wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84")

      #------------------------------------------------------------------------------#
      # interpolation mask contour from sf to spdf (SpatialPolygonsDataFrame):
      bv_sp <- sf::as_Spatial(bassin)
      # convert spatial layers from sf object to the Spatial Polygon Data Frame (SPDF)
      sp::proj4string(bv_sp)<-wgs84

      #------------------------------------------------------------------------------#
      # define the grid:
      grd <- as.data.frame(sp::spsample(bv_sp, "regular", n = 20000))
      names(grd)       <- c("X", "Y")
      sp::coordinates(grd) <- c("X", "Y")
      sp::gridded(grd)     <- TRUE  # Create SpatialPixel object
      sp::fullgrid(grd)    <- TRUE  # Create SpatialGrid object
      sp::proj4string(grd) <- wgs84

      #grd_bu <- grd # backup the grid (we use it later again)
      #------------------------------------------------------------------------------#

      # conversion en objet data.frame
      prec.df<- interpolationData.WGS()@data

      #---------------------------------------------------------------------------#
      gridded.points<- sf::as_Spatial(sf::st_transform(sf::st_as_sf(interpolationData.WGS()), 4326))
      grd<- sf::as_Spatial(sf::st_transform(sf::st_as_sf(grd), 4326))

      # interpolation sur toutes les pas de temps
      list.idw <- colnames(prec.df)[-1] %>%
        purrr::set_names() %>%
        purrr::map(
          ., ~ gstat::idw(
            stats::as.formula(paste(.x, "~ 1")),
            locations = gridded.points, newdata = grd
          )
        )

      # isolines
      cont<- as.data.frame(list.idw)  %>%
        dplyr::rename(x= db_Moyenne.coords.x1, y= db_Moyenne.coords.x2, var1.pred=db_Moyenne.var1.pred)  %>%
        dplyr::select(-c(4, 5))
      cont.raster<- raster::rasterFromXYZ(cont)
      cont.raster.mask<-  raster::mask(cont.raster, bassin)
      cont.raster.mask.fn<-  sf::st_as_sf(raster::rasterToContour(cont.raster.mask))
      sf::st_crs(cont.raster.mask.fn)<- 4326

      # transformation du résultat en tibble
      idw.output<-
        as.data.frame(list.idw) %>%
        tibble::as_tibble() %>%
        dplyr::select(1, 2, tidyselect::ends_with("var1.pred"))
      # renommage des colonnes
      names(idw.output)<- c(
        "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
      )

      idw.output.sf<- idw.output %>%
        #* transformation en objet sf
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        #* selection des pixels à l'intérieur du contour du bassin
        sf::st_filter(bassin)

      idw.output.df<- idw.output.sf %>%
        #* transformation en tibble
        tibble::tibble() %>%
        #* suppression de la colonne {geometry}
        dplyr::select(-geometry) %>%
        cbind(sf::st_coordinates(idw.output.sf)) %>%
        dplyr::rename(longitude = X, latitude = Y)

      #-----------------------------------------------------------------------------------#
      # # Button settings
      shinyjs::enable("idw")
      shinyjs::removeClass(id = "idwAnimate", class = "loading dots")

      # return
      cleanData(list(idw.output.df, cont.raster.mask.fn))

      # confirmation
      # # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Résultats De L'Interpolation [Méthode Utilisée : IDW]", style="color:#3474A7;family:Georgia;text-align:center;"),
        # Résumé de l'interpolation
        fluidRow(align = "left",
                 column(12, h4(
                   "Résultats de l'Interpolation || Résumé Statistique",
                   style=paste0(
                     "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                   )
                 )),
                 column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("summaryResult")), type = 5))
        ),
        # Isolignes
        fluidRow(align = "left",
                 column(12, h4(
                   "Résultats de l'Interpolation || ISOLIGNES",
                   style=paste0(
                     "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                   )
                 )),
                 column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("isolines")), type = 5))
        ),
        footer=tagList(
          modalButton('Fermer', icon = icon("power-off"))
        ),
        size = "l"
      ))

      # isohètes checkBoxGroup
      output$isolinesCheckBoxGroup <- renderUI({
        isohytes<- cleanData()[[2]]
        # shiny::req(cleanData())
        fluidRow(align="center",
          column(12,
                 checkboxGroupInput(
                   ns("isolignes"), div(
                     "Choix Des isohyètes A Afficher Dans La Carte !", class="textBoxed"
                   ),
                   choices = unique(isohytes$level), inline = TRUE, width = "100%",
                   selected = unique(isohytes$level)
                 )
          )
        )
      })

      # isohètes checkBoxGroup
      output$barLegendRoundingSlider <- renderUI({
        shiny::req(cleanData())
        fluidRow(align="center",
                 column(12,
                        sliderInput(ns("barNumberRounding"), label = "Nombre de Décimales dans la Légende", min = 0, max = 10,
                                    value =  2, step = 1, width = "100%")
                 )
        )
      })

      # Affichage du résult de d'interpolation
      output$summaryResult<- renderPrint({
        shiny::req(cleanData())
        summary(cleanData()[[1]])
      })

      output$isolines<- renderPrint({
        shiny::req(cleanData())
        cleanData()[[2]]
      })

    })

    # cartographie ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
    mean_period_plot_result<- eventReactive(ignoreInit = T, ignoreNULL = T, input$map, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav"
      req(cleanData())

      # palette de couleurs
      palett <- gplots::rich.colors(100, palette = colorbarOptions$legPalette, rgb = F)

      # plotting
      plotMap <- reactive({
        datatPLT<- as.data.frame(cleanData()[[1]])
        isohyete<- cleanData()[[2]][which(cleanData()[[2]]$level %in%  input$isolignes),]
        isohyete$level <- as.numeric(isohyete$level)
        ggplot2::ggplot(datatPLT) +
          # spatialisation des résultats de l'interpolation
          ggplot2::geom_tile(
            data =  datatPLT, ggplot2::aes(x = longitude, y = latitude, fill = Moyenne, color = Moyenne), size = .6
          ) +
          ggplot2::geom_sf(
            data=bv_wgs84(), color=layersOptions$bvContColor, linewidth=as.numeric(layersOptions$bvContSize), fill=NA
          ) +
          ggplot2::geom_sf(
            data =isohyete, size = as.numeric(layersOptions$IsolineSize), color= layersOptions$IsolineColor,
            linetype=ifelse(length(layersOptions$IsolineType), as.numeric(layersOptions$IsolineType), layersOptions$IsolineType), show.legend = F
          ) +
          ggplot2::geom_sf_label(
            data =  isohyete, ggplot2::aes(label = level),  size = as.numeric(layersOptions$IsolineFontSize),
            nudge_y = as.numeric(layersOptions$IsolineLabelNudgeX), nudge_x = as.numeric(layersOptions$IsolineLabelNudgeY),
            alpha = as.numeric(layersOptions$IsolineLabelAlpha), color =  layersOptions$IsolineLabelColor, fill =  layersOptions$IsolineLabelFill
          ) +
          ggplot2::scale_fill_gradientn(
            colours =  rev(palett),
            limits = c( min(datatPLT$Moyenne), max(datatPLT$Moyenne)),
            breaks = seq(min(datatPLT$Moyenne), max(datatPLT$Moyenne), length.out = 6),
            labels = round(seq(min(datatPLT$Moyenne), max(datatPLT$Moyenne), length.out = 6), input$barNumberRounding)
          )+
          ggplot2::scale_color_gradientn(
            colours =  rev(palett),
            limits = c( min(datatPLT$Moyenne), max(datatPLT$Moyenne)),
            breaks = seq(min(datatPLT$Moyenne), max(datatPLT$Moyenne), length.out = 6),
            labels = round(seq(min(datatPLT$Moyenne), max(datatPLT$Moyenne), length.out = 6), 2),
            guide = "none"
          )+
          #theme homogène
          ggplot2::theme_bw()+
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              barwidth=as.numeric(colorbarOptions$barWidth), barheight=as.numeric(colorbarOptions$barHeight),
              title.position = colorbarOptions$barTitleLoc,
              title.hjust = as.numeric(colorbarOptions$barTitleHjust), ticks.colour =  colorbarOptions$barTicksColor,
              ticks.linewidth = as.numeric(colorbarOptions$barTicksLineWidth), frame.colour = "gray", frame.linewidth = .3,
              title.theme = ggplot2::element_text(
                family = "Times", size = as.numeric(colorbarOptions$barTitleSize),
                margin = ggplot2::margin(
                  b=as.numeric(colorbarOptions$barTitleMarginB), l=as.numeric(colorbarOptions$barTitleMarginL),
                  t=as.numeric(colorbarOptions$barTitleMarginT), r=as.numeric(colorbarOptions$barTitleMarginR), unit = "cm"
                ),
                angle =  as.numeric(colorbarOptions$legTitleAngle)
              )
            ),
            color = "none"
          ) +
          ggplot2::theme(
            # légende
            legend.direction =  legendThemeOptions$legDir,
            legend.text = ggplot2::element_text(
              family = "Times", size=as.numeric(legendThemeOptions$LegTextSize), color = legendThemeOptions$LegTextColor,
              margin = ggplot2::margin(
                b=as.numeric(legendThemeOptions$legTextMarginB), l=as.numeric(legendThemeOptions$barTitleMarginL),
                t=as.numeric(legendThemeOptions$legTextMarginT), r=as.numeric(legendThemeOptions$legTextMarginR), unit = "cm"
              )
            ),
            legend.position =  legendThemeOptions$legLoc,
            legend.margin = ggplot2::margin(
              b=as.numeric(legendThemeOptions$legMarginB), l=as.numeric(legendThemeOptions$legMarginL),
              t=as.numeric(legendThemeOptions$legMarginT), r=as.numeric(legendThemeOptions$legMarginR), unit = "cm"
            ),
            legend.box.background = ggplot2::element_blank(),
            legend.background = ggplot2::element_blank(),
            legend.box.just = "center",
            axis.text.x = ggplot2::element_text(
              family = "Times", size=as.numeric(axisThemeOptions$xAxisTextSize), color = axisThemeOptions$axisTextColor
            ),
            axis.text.y = ggplot2::element_text(
              family = "Times", size=as.numeric(axisThemeOptions$yAxisTextSize), color = axisThemeOptions$axisTextColor
            ),
            axis.title = ggplot2::element_blank(),
            #axis.text = element_text (family = "Times", size=10, color = "black")
            plot.background = ggplot2::element_rect(fill="white", color = "white", size=.0001),
            plot.margin = ggplot2::margin(rep(.5, 4) ,unit = "cm")
          ) +
          #titre de la légende
          ggplot2::labs(
            fill = legendThemeOptions$legTitleName, color = legendThemeOptions$legTitleName, linetype=NULL
          ) +
          # ajout de la flèche Nord
          northArrow(
            location = northOptions$northArrowLocation, width=as.numeric(northOptions$northArrowWidth),
            height = as.numeric(northOptions$northArrowHeight), padx=as.numeric(northOptions$northArrowPadx),
            pady= as.numeric(northOptions$northArrowPady)
          )+
          # ajout de l'échelle
          scaleBar(
            location=scaleOptions$scaleLocation,
            height= as.numeric(scaleOptions$scaleTickHeight), width.hint=as.numeric(scaleOptions$scaleWidthHint),
            text.cex = as.numeric(scaleOptions$scaleTextCex),  scale.style=scaleOptions$scaleStyle
            # line.width = as.numeric(input$scaleLineWidth)
          )
        })

      # export
      plotMap()

    })

    # affichage
    output$interpolationResult<- renderPlot({
      req(mean_period_plot_result())
      mean_period_plot_result()
    }, res = 55)

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("Interpolation_Spatiale", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, mean_period_plot_result(), width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("Interpolation_Spatiale", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, mean_period_plot_result(), width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_unique_periode_time_serie_interpolation_map_ui("unique_periode_time_serie_interpolation_map_1")

## To be copied in the server
# mod_unique_periode_time_serie_interpolation_map_server("unique_periode_time_serie_interpolation_map_1")
