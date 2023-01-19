#' multiperiode_periode_time_serie_interpolation_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multiperiode_periode_time_serie_interpolation_map_ui <- function(id){
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
             column(3, actionButton(ns("north"), label = div(tags$strong("Flèche Nord"), style="font-size:85%;"), icon = icon("compass"), width = "100%")),
             column(3, actionButton(ns("scale"), label =  div(tags$strong("Barre D'échelle"), style="font-size:85%;"), icon = icon("barcode"), width = "100%")),
             column(3, actionButton(ns("layers"), label =  div(tags$strong("Couches"), style="font-size:85%;"), icon = icon("layer-group"), width = "100%")),
             column(3, actionButton(ns("colorbar"), label =  div(tags$strong("Légende"), style="font-size:85%;"), icon = icon("elementor"), width = "100%")),
             column(3, actionButton(ns("legendtheme"), label =  div(tags$strong("Theme Légende"), style="font-size:85%;"), icon = icon("affiliatetheme"), width = "100%")),
             column(3, actionButton(ns("axistheme"), label =  div(tags$strong("Theme Axes"), style="font-size:85%;"), icon = icon("affiliatetheme"), width = "100%")),
             column(3, actionButton(ns("facettheme"), label =  div(tags$strong("Theme Facets"), style="font-size:85%;"), icon = icon("affiliatetheme"), width = "100%")),
             column(3, actionButton(ns("axisoverride"), label =  div(tags$strong("Overriding des Axes"), style="font-size:85%;"), icon = icon("exchange"), width = "100%"))
    ),
    tags$hr(style="border-color:gray;"),
    fluidRow(align = "center",
             column(3, dipsaus::actionButtonStyled(ns("load"), span("Charger Options", id=ns("loadAnimate")), icon = icon("map"), class= "", type="primary")),
             column(3, dipsaus::actionButtonStyled(ns("map"), span("Cartographie", id=ns("mapAnimate")), icon = icon("map"), class= "", type="primary")),
             column(3, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(3, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),
    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(12,  uiOutput(ns("mapOptions"), width="100%")),
             column(12, uiOutput(ns("isolinesCheckBoxGroup"), width="100%")),
             column(12, shinycssloaders::withSpinner(plotOutput(ns("interpolationResult"), width="100%")))
    ),

    tags$hr(style="border-color:gray;"),
  )
}

#' multiperiode_periode_time_serie_interpolation_map Server Functions
#'
#' @noRd
mod_multiperiode_periode_time_serie_interpolation_map_server <- function(id, bassin, donnees){
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
    facetThemeOptions <- reactiveValues()
    axisOverridingCode <- reactiveValues()

    # Légende ---------------------------------------------------------------------------------------------------------------------------|
    # initial options
    northOptions$northArrowLocation <- "tl"
    northOptions$northArrowWidth <- 1.5
    northOptions$northArrowHeight <- 1.5
    northOptions$northArrowPadx <- -.45
    northOptions$northArrowPady <- .25

    observeEvent(ignoreInit = T, ignoreNULL = T, input$north, {
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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitLeg, {
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

    observeEvent(ignoreInit = T, ignoreNULL = T, input$scale, {
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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitScale, {
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
    layersOptions$bvContSize <- 1.20
    layersOptions$IsolineSize <- .80
    layersOptions$IsolineFontSize <- 3.5
    layersOptions$IsolineLabelNudgeX <- 0
    layersOptions$IsolineLabelNudgeY <- 0
    layersOptions$IsolineLabelAlpha <- .4
    layersOptions$bvContColor <- "black"
    layersOptions$IsolineColor <- "black"
    layersOptions$IsolineType <- "2"
    layersOptions$IsolineLabelColor <- 'black'
    layersOptions$IsolineLabelFill <- "gray"

    observeEvent(ignoreInit = T, ignoreNULL = T, input$layers, {
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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitLayers, {
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
    colorbarOptions$barWidth <- 35
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

    observeEvent(ignoreInit = T, ignoreNULL = T, input$colorbar, {
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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitColorBar, {
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
    legendThemeOptions$legMarginB <- .05
    legendThemeOptions$legMarginL <- 0
    legendThemeOptions$legDir <- "horizontal"
    legendThemeOptions$LegTextColor <- "black"
    legendThemeOptions$legPosType <- "Côté"
    legendThemeOptions$legPosXcoord <- .5
    legendThemeOptions$legPosYcoord <- .5
    legendThemeOptions$legLoc <- "bottom"
    legendThemeOptions$legPalette <- "temperature"
    legendThemeOptions$legTitleName <- "Variable Interpolée (unité)"

    observeEvent(ignoreInit = T, ignoreNULL = T, input$legendtheme, {
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
          column(4, selectInput(ns("legPosType"), label = "Type De Positionnement de La Légende",  choices = c("Côté", "Coordonnées"), selected = legendThemeOptions$legPosType)),
          column(4,selectInput(ns("legLoc"), label = "Position De La Légende", choices = c("En Haut"="top", "En Bas"="bottom", "A Gauche"="left", "A Droite"="right"), selected = legendThemeOptions$legLoc, width = "100%")),

          column(4, sliderInput(ns("legMarginL"), label = "Marge Gauche De La Légende", min = -5, max = 5, value = legendThemeOptions$legMarginL, step = .01, width = "100%")),
          column(2, sliderInput(ns("legPosXcoord"), label = "Coordonnées X",  min = 0, max = 1, step = .01, value =  legendThemeOptions$legPosXcoord)),
          column(2, sliderInput(ns("legPosYcoord"), label = "Coordonnées Y",  min = 0, max = 1, step = .01, value = legendThemeOptions$legPosYcoord)),

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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitLegendTheme, {
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
      legendThemeOptions$legPosType <- input$legPosType
      legendThemeOptions$legPosXcoord <- input$legPosXcoord
      legendThemeOptions$legPosYcoord <- input$legPosYcoord
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

    observeEvent(ignoreInit = T, ignoreNULL = T, input$axistheme, {
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
    observeEvent(ignoreInit = T, ignoreNULL = T, input$submitAxisTheme, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      axisThemeOptions$xAxisTextSize <- input$xAxisTextSize
      axisThemeOptions$axisTextColor <- input$axisTextColor
      axisThemeOptions$yAxisTextSize <- input$yAxisTextSize
      removeModal()
    })

    # Theme Des Facets -----------------------------------------------------------------------------------------------------------------|
    # init options
    facetThemeOptions$stripTextSize <- 13
    facetThemeOptions$stripTextMarginB <- 2
    facetThemeOptions$stripTextMarginT <- 0
    facetThemeOptions$stripTextMarginL <- 0
    facetThemeOptions$stripTextMarginR <- 0
    facetThemeOptions$stripTextMarginUnit <- "pt"

    observeEvent(ignoreInit = T, ignoreNULL = T, input$facettheme, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Configuration Du Thème Des Facets [Box]", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("stripTextSize"), label = "Taille de Police Des Titres de Graphiques", min = 5, max = 35, value = facetThemeOptions$stripTextSize, step = 1, width = "100%")),
          column(4, sliderInput(ns("stripTextMarginT"), label = "Marge  Supérieures Des Titres de Graphiques", min = -5, max = 5, value = facetThemeOptions$stripTextMarginT, step = .01, width = "100%")),
          column(4, sliderInput(ns("stripTextMarginR"), label = "Marge  Supérieures Des Titres de Graphiques", min = -5, max = 5, value = facetThemeOptions$stripTextMarginR, step = .01, width = "100%")),
          column(4, sliderInput(ns("stripTextMarginB"), label = "Marge  Supérieures Des Titres de Graphiques", min = -5, max = 5, value = facetThemeOptions$stripTextMarginB, step = .01, width = "100%")),
          column(4, selectInput(ns("stripTextMarginUnit"), label = "Unité de Mesure Des Marges", choices = c("pt", "cm"), selected = facetThemeOptions$stripTextMarginUnit)),
          column(4, sliderInput(ns("stripTextMarginL"), label = "Marge  Supérieures Des Titres de Graphiques", min = -5, max = 5, value = facetThemeOptions$stripTextMarginL, step = .01, width = "100%"))
        ),
        footer=tagList(
          actionButton(ns("submitFacetsTheme"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "l"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitFacetsTheme, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      facetThemeOptions$stripTextSize <- input$stripTextSize
      facetThemeOptions$stripTextMarginB <- input$stripTextMarginB
      facetThemeOptions$stripTextMarginT <- input$stripTextMarginT
      facetThemeOptions$stripTextMarginL <- input$stripTextMarginL
      facetThemeOptions$stripTextMarginR <- input$stripTextMarginR
      facetThemeOptions$stripTextMarginUnit <- input$stripTextMarginUnit
      removeModal()
    })

    # Manipulation des axes -----------------------------------------------------------------------------------------------------------------|
    # init options
    axisOverridingCode$xbreaks <- ""
    axisOverridingCode$ybreaks <- ""

    observeEvent(ignoreInit = T, ignoreNULL = T, input$axisoverride, {
      # beepr::beep("data/mixkit-explainer-video-game-alert-sweep-236.wav")
      showModal(modalDialog(
        tags$h3("Overriding des Axes du Graphique [Code R]", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(12,  textInput(ns("xbreaks"), label = "Graduations de l'axe des Longitudes {X}", value = axisOverridingCode$xbreaks, placeholder = "x1;x2;x2;x4;...", width = "100%")),
        ),
        fluidRow(
          column(12,  textInput(ns("ybreaks"), label = "Graduations de l'axe des Latitudes {Y}", value = axisOverridingCode$ybreaks, placeholder = "y1;y2;y2;y4;...", width = "100%"))
        ),
        footer=tagList(
          actionButton(ns("submitAxisOverride"), 'Valider', class = "btn-success", icon = icon("thumbs-up")),
          modalButton('Annuler', icon = icon("power-off"))
        ),
        size = "m"
      ))

    })
    # only store the information if the user clicks submit
    observeEvent(input$submitFacetsTheme, {
      #beepr::beep("data/mixkit-mouse-click-close-1113.wav")
      axisOverridingCode$xbreaks <- input$xbreaks
      axisOverridingCode$ybreaks <- input$ybreaks
      removeModal()
    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #| CARTOGRAPHIE

    # to store mapping result
    plot_redsult<- reactiveValues()

    # loading Options
    observeEvent(ignoreInit = T, ignoreNULL = T,  input$load,  {
      req(donnees, bassin)

      # niveau de zoom
      zoomLevel<- reactive({
        ifelse(
          as.numeric(sf::st_area(bassin)) > 3118775314,
          3118775314*8/as.numeric(sf::st_area(bassin)),
          as.numeric(sf::st_area(bassin)*8/3118775314+8)
        )
      })

      # isohètes checkBoxGroup
      output$isolinesCheckBoxGroup <- renderUI({
        isohytes<- future::value(donnees)[[2]]
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
      output$mapOptions <- renderUI({
        # shiny::req(cleanData())
        fluidRow(align="center",
                 column(4, sliderInput(ns("nrowFacets"), label = div("Nombre de Lignes des Facets", style="font-size:85%;font-family:georgia"), min = 2, max = 6, value =  2, step = 1, width = "100%")),
                 column(4, sliderInput(ns("facetsXspacing"), label = div("Espacement Horizontale Facets", style="font-size:85%;font-family:georgia"), min = 0, max = 4, value =  0, step = .01, width = "100%")),
                 column(4, sliderInput(ns("barNumberRounding"), label = div("Nbre de Décimales Légende", style="font-size:85%;font-family:georgia"), min = 0, max = 6, value =  2, step = 1, width = "100%")),
                 column(4, sliderInput(ns("zoomin"), label = div("Niveau de zoom", style="font-size:85%;font-family:georgia"), min = 0, max = 10, value = 0, step = .01)),
                 column(4, sliderInput(ns("longAdjust"), label = div("Adjustement Longitudinale", style="font-size:85%;font-family:georgia"), min = -5, max = 5, step = .001, value = 0)),
                 column(4, sliderInput(ns("latAdjust"), label = div("Adjustement Latitudinale", style="font-size:85%;font-family:georgia"), min = -5, max = 5, step = .001, value = 0))
        )
      })
    })

    # mapping
    multiperiode_map_plot<- eventReactive(ignoreInit = T, ignoreNULL = T,  input$map,  {
      req(bassin, donnees, input$nrowFacets, input$facetsXspacing, input$barNumberRounding, input$zoomin, input$longAdjust, input$latAdjust)

      # Notification
      id <- showNotification(
        "Traitement de la Carte ... Peut prendre un certain temps ...",
        duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # data
      bv <- bassin
      tuiles <- future::value(donnees)[[1]]
      isohyetes <- future::value(donnees)[[2]]


      # gestion des entrées vecteurs
      extract_entries<-  function(text){
        text<-  gsub(" ", "", text)
        split<-  strsplit(text, ";", fixed = FALSE)[[1]]
        as.numeric(split)
      }

      # xticks  definition start ----------------------------- -------------------------------------------------------------#
      if(axisOverridingCode$xbreaks==""){
        x_ticks<- NA
      } else {
        x_ticks<- extract_entries(axisOverridingCode$xbreaks)
      }
      # filtrage
      if(!anyNA(x_ticks)){
        x_ticks_fn<- round(x_ticks, 2)
      }else{
        if(axisOverridingCode$xbreaks!=""){
          # # feedbacks
          # shinyFeedback::feedbackWarning(
          #   "xbreaks", anyNA(x_ticks),
          #   "Vecteur de données incorrect !"
          # )
          ## alert
          shinyalert::shinyalert(
            "Valeur incorrecte pour le champs {Graduations de l'axe des longitudes {X}} :",
            paste0(
              "Veillez spécifier un vecteur de nombre de la forme d'une ligne d'un fichier {CSV} avec le virgule ",
              "comme séparateur de colonnes selon le modèle {n1;n2;n3;etc.}. Le séparateur décimal doit être ",
              "un point virgule (et non une virgule comme le système français). Exemple Correcte: { 14;23.7;44;18.3 }"
            )
          )
        }

        x_ticks_fn<- round(seq(sf::st_bbox(bassin)[1], sf::st_bbox(bassin)[3], length.out=4)-.2, 2)[-1]

      }


      # yticks  definition start ----------------------------- -------------------------------------------------------------#
      if(axisOverridingCode$ybreaks==""){
        y_ticks<- NA
      } else {
        y_ticks<- extract_entries(axisOverridingCode$ybreaks)
      }
      # filtrage
      if(!anyNA(y_ticks)){
        y_ticks_fn<- round(y_ticks, 2)
      }else{
        if(axisOverridingCode$ybreaks!=""){
          # # feedbacks
          # shinyFeedback::feedbackWarning(
          #   "ybreaks", anyNA(y_ticks),
          #   "Vecteur de données incorrect !"
          # )
          ## alert
          shinyalert::shinyalert(
            "Valeur incorrecte pour le champs {Graduations de l'axe des latitudes {Y}} :",
            paste0(
              "Veillez spécifier un vecteur de nombre de la forme d'une ligne d'un fichier {CSV} avec le virgule ",
              "comme séparateur de colonnes selon le modèle {n1;n2;n3;etc.}. Le séparateur décimal doit être ",
              "un point virgule (et non une virgule comme le système français). Exemple Correcte: { 14;23.7;44;18.3 }"
            )
          )
        }

        y_ticks_fn<- round(seq(sf::st_bbox(bassin)[2], sf::st_bbox(bassin)[4], length.out=4), 2)

      }
      # xbreaks definition end ----------------------------- -------------------------------------------------------------#

      # # centre de zoom
      zoomCenter <-  c(
        sf::st_coordinates(sf::st_point_on_surface(bv))[1] + as.numeric(input$longAdjust),
        sf::st_coordinates(sf::st_point_on_surface(bv))[2] + as.numeric(input$latAdjust)
      )

      # zoom
      # Niveau de zoom
      zoomLevel<- function(){
        if(input$zoomin != 0) {
          return(
            ggplot2::coord_sf(
              expand = TRUE, label_graticule = "SW", crs = 4326,
              # étendue des longitudes
              xlim = zoomMap(zoomCenter, as.numeric(input$zoomin))[[1]],
              # étendue des latitudes
              ylim = zoomMap(zoomCenter,  as.numeric(input$zoomin))[[2]]
            )
          )
        }else{
          return(
            ggplot2::coord_sf(
              expand = TRUE, label_graticule = "SW", crs = 4326
            )
          )
        }
      }

      ### TYPE DE POSITIONNEMENT DE LA LEGENDE
      if(legendThemeOptions$legPosType == "Côté"){
        leg_pos <- legendThemeOptions$legLoc
      }else{
        leg_pos <- c(as.numeric(legendThemeOptions$legPosXcoord), as.numeric(legendThemeOptions$legPosYcoord))
      }

      ### MAKING  PLOTING
      palett <- gplots::rich.colors(100, palette = legendThemeOptions$legPalette, rgb = F)

      plotMap <- reactive({
        datatPLT<- as.data.frame(tuiles)
        isohyete<- isohyetes[which(isohyetes$level %in%  input$isolignes),]
        isohyete$level <- as.numeric(isohyete$level)

        ggplot2::ggplot(datatPLT) +
          # spatialisation des résultats de l'interpolation
          ggplot2::geom_tile(
            data =  datatPLT, ggplot2::aes(x = Longitude, y = Latitude, fill = valeur, color = valeur), size = .6
          ) +
          ggplot2::geom_sf(
            data=bv, color=layersOptions$bvContColor, linewidth=as.numeric(layersOptions$bvContSize), fill=NA
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
          ## zoom map
          zoomLevel()+
          # colourbar
          ggplot2::scale_fill_gradientn(
            colours =  rev(palett),
            limits = c( min(datatPLT$valeur), max(datatPLT$valeur)),
            breaks = seq(min(datatPLT$valeur), max(datatPLT$valeur), length.out = 6),
            labels = round(seq(min(datatPLT$valeur), max(datatPLT$valeur), length.out = 6), input$barNumberRounding)
          )+
          ggplot2::scale_color_gradientn(
            colours =  rev(palett),
            limits = c( min(datatPLT$valeur), max(datatPLT$valeur)),
            breaks = seq(min(datatPLT$valeur), max(datatPLT$valeur), length.out = 6),
            labels = round(seq(min(datatPLT$valeur), max(datatPLT$valeur), length.out = 6), 2),
            guide = "none"
          )+
          #theme homogène
          ggplot2::theme_bw()+
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              barwidth=as.numeric(colorbarOptions$barWidth), barheight=as.numeric(colorbarOptions$barHeight),
              title.position = colorbarOptions$barTitleLoc, title = legendThemeOptions$legTitleName,
              title.hjust = as.numeric(colorbarOptions$barTitleHjust), ticks.colour =  colorbarOptions$barTicksColor,
              ticks.linewidth = as.numeric(colorbarOptions$barTicksLineWidth), frame.colour = "gray", frame.linewidth = .3,
              title.theme = ggplot2::element_text(
                family = "Times", size = as.numeric(colorbarOptions$barTitleSize), face = "bold",
                margin = ggplot2::margin(
                  b=as.numeric(colorbarOptions$barTitleMarginB), l=as.numeric(colorbarOptions$barTitleMarginL),
                  t=as.numeric(colorbarOptions$barTitleMarginT), r=as.numeric(colorbarOptions$barTitleMarginR), unit = "cm"
                ),
                angle =  as.numeric(colorbarOptions$legTitleAngle)
              )
            ),
            color = "none"
          ) +
          # graduation de l'axe des longitudes
          ggplot2::scale_x_continuous(breaks = x_ticks_fn)+
          # graduation de l'axe des longitudes
          ggplot2::scale_y_continuous(breaks = y_ticks_fn)+
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
            legend.position =  leg_pos,
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
            plot.margin = ggplot2::margin(rep(.5, 4) ,unit = "cm"),
            # strips
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              family = "Times", size = as.numeric(facetThemeOptions$stripTextSize), face="bold",
              margin = ggplot2::margin(
                b =  facetThemeOptions$stripTextMarginB,  t =  facetThemeOptions$stripTextMarginT,
                l =  facetThemeOptions$stripTextMarginL,  r =  facetThemeOptions$stripTextMarginR,
                unit =  facetThemeOptions$stripTextMarginUnit
              )
            ),
            # legend.key = element_rect(color = NA, fill = NA),
            panel.spacing.y = grid::unit(.5, "cm"),
            panel.spacing.x = grid::unit(as.numeric(input$facetsXspacing), "cm")
          ) +
          ggplot2::labs(
            fill = legendThemeOptions$legTitleName, color = legendThemeOptions$legTitleName, linetype=NULL
          ) +
          # # ajout de la flèche Nord
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
          ) +

          ggplot2::facet_wrap(ggplot2::vars(variable))
      })

      plotMap()

    })

    # affichage
    output$interpolationResult<- renderPlot({
      req(multiperiode_map_plot())
      multiperiode_map_plot()
    }, res = 55)

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("MultiPeriodeSpatialInterpolation", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, multiperiode_map_plot(), width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("MultiPeriodeSpatialInterpolation", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, multiperiode_map_plot(), width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_multiperiode_periode_time_serie_interpolation_map_ui("multiperiode_periode_time_serie_interpolation_map_1")

## To be copied in the server
# mod_multiperiode_periode_time_serie_interpolation_map_server("multiperiode_periode_time_serie_interpolation_map_1")
