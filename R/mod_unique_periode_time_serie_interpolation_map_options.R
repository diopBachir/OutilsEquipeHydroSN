#' unique_periode_time_serie_interpolation_map_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_unique_periode_time_serie_interpolation_map_options_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(tags$style(type="text/css", '
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
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')
    ),

    fluidRow(align="center",
             column(2, actionButton(ns("legende"), label = div(tags$strong("Flèche Nord"), style="font-size:75%;text-align:center"), icon = icon("compass"), width = "100%")),
             column(2, actionButton(ns("scale"), label =  div(tags$strong("Barre D'échelle"), style="font-size:75%;text-align:center"), icon = icon("barcode"), width = "100%")),
             column(2, actionButton(ns("layers"), label =  div(tags$strong("Couches"), style="font-size:75%;text-align:center"), icon = icon("layer-group"), width = "100%")),
             column(2, actionButton(ns("colorbar"), label =  div(tags$strong("Légende"), style="font-size:75%;text-align:center"), icon = icon("elementor"), width = "100%")),
             column(2, actionButton(ns("legendtheme"), label =  div(tags$strong("Theme Légende"), style="font-size:75%;text-align:center"), icon = icon("affiliatetheme"), width = "100%")),
             column(2, actionButton(ns("axistheme"), label =  div(tags$strong("Theme Axes"), style="font-size:75%;text-align:center"), icon = icon("affiliatetheme"), width = "100%"))
    ),
    tags$hr(style="border-color:gray;"),
    fluidRow(align = "center",
             column(3, dipsaus::actionButtonStyled(ns("idw"), span("Interpolation", id=ns("idwAnimate")), icon = icon("itercom"), class= "", type="primary")),
             column(3, dipsaus::actionButtonStyled(ns("map"), span("Cartographie", id=ns("mapAnimate")), icon = icon("map"), class= "", type="primary")),
             column(3, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(3, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),
    tags$hr(style="border-color:gray;"),

    verbatimTextOutput(ns("test")),
    plotOutput(ns("plt"))
  )
}

#' unique_periode_time_serie_interpolation_map_options Server Functions
#'
#' @noRd
mod_unique_periode_time_serie_interpolation_map_options_server <- function(id, bassin, stations, interpolationData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    #| DEFINITION DES OPTIONS

    legendOptions <- reactiveValues()
    scaleOptions <- reactiveValues()
    layersOptions <- reactiveValues()
    colorbarOptions <- reactiveValues()
    legendThemeOptions <- reactiveValues()
    axisThemeOptions <- reactiveValues()

    # Légende ---------------------------------------------------------------------------------------------------------------------------|
    # initial options
    legendOptions$northArrowLocation <- "tl"
    legendOptions$northArrowWidth <- 1.5
    legendOptions$northArrowHeight <- 1.5
    legendOptions$northArrowPadx <- 25
    legendOptions$northArrowPady <- .25

     observeEvent(input$legende, {
      showModal(modalDialog(
        tags$h3('Manipulation De La Flèche Nord De la Carte', style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(6, sliderInput(ns("northArrowWidth"), label = "Largeur", min = -10, max = 10, value = 1.5, step = .1, width = "100%")),
          column(6, sliderInput(ns("northArrowHeight"), label = "Hauteur", min = -10, max = 10, value = 1.5, step = .1)),
          column(4, sliderInput(ns("northArrowPadx"), label = "Padx",  min = -5, max = 5, value = .25, step = .01)),
          column(4,selectInput(
            ns("northArrowLocation"), label = "Position", choices = c(
              "Bas-Gauche" = "bl", "Bas-Droit" = "br","Haut-Gauche" = "tl", "Haut-Droit" = "tr"
            ), selected = "tl")
          ),
          column(4, sliderInput(ns("northArrowPady"), label = "Pady",  min = -5, max = 5, value = .25, step = .01)),
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
      legendOptions$northArrowLocation <-  input$northArrowLocation
      legendOptions$northArrowWidth <-  input$northArrowWidth
      legendOptions$northArrowHeight <-  input$northArrowHeight
      legendOptions$northArrowPadx <-  input$northArrowPadx
      legendOptions$northArrowPady <-  input$northArrowPady
      removeModal()
    })

    # Barre d'échelle -----------------------------------------------------------------------------------------------------------------|
    # initial options
    scaleOptions$scaleWidthHint <- .13
    scaleOptions$scaleTickHeight <- .30
    scaleOptions$scaleLocation <- "tl"
    scaleOptions$scaleTextCex <- 1.15
    scaleOptions$scaleStyle <- "ticks"

    observeEvent(input$scale, {
      showModal(modalDialog(
        tags$h3("Manipulation De La Barre D'échelle De la Carte", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(6, sliderInput(ns("scaleWidthHint"), label = "Largeur", min = .01, max = 10, value = .13, step = .01, width = "100%")),
          column(6, sliderInput(ns("scaleTickHeight"), label = "Hauteur des Ticks", min = .01, max = 10, value = .30, step = .01, width = "100%")),
          column(4,selectInput(
            ns("scaleLocation"), label = "Position", choices = c(
              "Bas-Gauche" = "bl", "Bas-Droit" = "br","Haut-Gauche" = "tl", "Haut-Droit" = "tr"
            ), selected = "tl")
          ),
          column(4, sliderInput(ns("scaleTextCex"), label = "Taille de Police",  min = .01, max = 10, value = 1.15, step = .01)),
          column(4,selectInput(
            ns("scaleStyle"), label = "Style de la barre", choices = c("Barre" = "bar", "Ticks" = "ticks"), selected = "ticks"
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
      scaleOptions$scaleWidthHint <- input$scaleWidthHint
      scaleOptions$scaleTickHeight <- input$scaleTickHeight
      scaleOptions$scaleLocation <- input$scaleLocation
      scaleOptions$scaleTextCex <- input$scaleTextCex
      scaleOptions$scaleStyle <- input$scaleStyle
      removeModal()
    })

    # Couches -----------------------------------------------------------------------------------------------------------------|
    # init options
    layersOptions$TilesHeight <- .04
    layersOptions$bvContSize <- 1.40
    layersOptions$IsolineSize <- .80
    layersOptions$IsolineFontSize <- 5.50
    layersOptions$IsolineLabelNudgeX <- 0
    layersOptions$IsolineLabelNudgeY <- 0
    layersOptions$IsolineLabelAlpha <- .4
    layersOptions$bvContColor <- "black"
    layersOptions$IsolineColor <- "red"
    layersOptions$IsolineType <- "1"
    layersOptions$IsolineLabelColor <- 'red'
    layersOptions$IsolineLabelFill <- "red"

    observeEvent(input$layers, {
      showModal(modalDialog(
        tags$h3("Manipulation Des Couches De la Carte", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("TilesHeight"), label = "Hauteur Des Tuiles", min = 0, max = 10, value = .04, step = .001, width = "100%")),
          column(4, sliderInput(ns("bvContSize"), label = "Largeur Du Trait de Contour Du Bassin", min = 0.01, max = 10, value = 1.40, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineSize"), label = "Epaisseur Des Isolignes", min = 0, max = 10, value = .80, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineFontSize"), label = "Taille De police Des Etiquettes D'isolignes", min = 1, max = 10, value = 5.50, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineLabelNudgeX"), label = "Décalage Horizontal Des Etiquettes D'isolignes", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("IsolineLabelNudgeY"), label = "Décalage Vertical Des Etiquettes D'isolignes", min = -5, max = 5, value = 0, step = .01, width = "100%")),

          column(4,selectInput(
            ns("bvContColor"), label = "Couleur Contour Bassin", choices = colors(), selected = "black", width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineColor"), label = "Couleur Des Isolignes", choices = colors(), selected = "black", width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineType"), label = "Style De Lignes Isolignes", choices = c(0:7, paste0(1:7, rep(1:7, each=7))), selected = "1", width = "100%")
          ),
          column(4,selectInput(
            ns("IsolineLabelColor"), label = "Couleur Des Isolignes", choices = colors(), selected = "red", width = "100%")
          ),

          column(4, sliderInput(ns("IsolineLabelAlpha"), label = "Transparence Des Etiquettes Isolignes", min = 0, max = 1, value = .4, step = .01, width = "100%")),

          column(4,selectInput(
            ns("IsolineLabelFill"), label = "Couleur De Fond Des Isolignes", choices = colors(), selected = "red", width = "100%")
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
      layersOptions$TilesHeight <- input$TilesHeight
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
    colorbarOptions$barTitleVjust <- -1
    colorbarOptions$barTicksLineWidth <- 2
    colorbarOptions$barTitleSize <- 17
    colorbarOptions$barTitleMarginT <- 0
    colorbarOptions$barTitleMarginR <- 0
    colorbarOptions$barTitleMarginB <- .07
    colorbarOptions$barTitleMarginL <- 0
    colorbarOptions$barTitleLoc <- "top"
    colorbarOptions$barTicksColor <- "white"

    observeEvent(input$colorbar, {
      showModal(modalDialog(
        tags$h3("Manipulation De La Barre De Légende [Echelle Numérique]", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("barHeight"), label = "Hauteur", min = 0.1, max = 50, value = 1.1, step = .1, width = "100%")),
          column(4, sliderInput(ns("barWidth"), label = "Largeur", min = 0.01, max = 50, value = 30, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleHjust"), label = "Alignement Horizontal", min = -2, max = 2, value = .5, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleVjust"), label = "Alignement Vertical", min = -2, max = 2, value = -1, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTicksLineWidth"), label = "Epaisseur des Graduations", min = .1, max = 10, value = 2, step = .1, width = "100%")),
          column(4, sliderInput(ns("barTitleSize"), label = "Taille de la Police", min = 5, max = 50, value = 17, step = 1, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginT"), label = "Marge Supérieure", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginR"), label = "Marge Droite", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("barTitleMarginB"), label = "Marge Inférieure", min = -5, max = 5, value = .07, step = .01, width = "100%")),

          column(4,selectInput(ns("barTitleLoc"), label = "Position",  choices = c("En Haut"="top", "En Bas"="bottom", "A Gauche"="left", "A Droite"="right"), selected = "top", width = "100%")),
          column(4, sliderInput(ns("barTitleMarginL"), label = "Marge Gauche", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4,selectInput(ns("barTicksColor"), label = "Couleur Des Graduations",  choices = colors(), selected = "white")),

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
      colorbarOptions$barHeight <- input$barHeight
      colorbarOptions$barWidth <- input$barWidth
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
    legendThemeOptions$LegTextSize <- 17
    legendThemeOptions$legTextMarginT <-  -.25
    legendThemeOptions$legTextMarginR <- 0
    legendThemeOptions$legTextMarginB <- 0
    legendThemeOptions$legTextMarginL <- 0
    legendThemeOptions$legMarginT <- 0
    legendThemeOptions$legMarginR <- 0
    legendThemeOptions$legMarginB <- .05
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
          column(4, sliderInput(ns("legTitleAngle"), label = "Orientation du Titre", min = 0, max = 360, value = 0, step = 45, width = "100%")),
          column(4, sliderInput(ns("legTexAngle"), label = "Orientation Des Etiquettes", min = 0, max = 360, value = 0, step = 45, width = "100%")),
          column(4, sliderInput(ns("LegTextSize"), label = "Taille de Police Des Etiquettes", min = 5, max = 35, value = 17, step = 1, width = "100%")),

          column(4, sliderInput(ns("legTextMarginT"), label = "Marge Supérieure Des Etiquettes", min = -5, max = 5, value = -.25, step = .01, width = "100%")),
          column(4, sliderInput(ns("legTextMarginR"), label = "Marge Droite Des Etiquettes", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("legTextMarginB"), label = "Marge Inférieure Des Etiquettes", min = -5, max = 5, value = 0, step = .01, width = "100%")),

          column(4, sliderInput(ns("legTextMarginL"), label = "Marge Gauche Des Etiquettes", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("legMarginT"), label = "Marge Supérieure De La Légende", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4, sliderInput(ns("legMarginR"), label = "Marge Droite De La Légende", min = -5, max = 5, value = 0, step = .01, width = "100%")),

          column(4,selectInput(ns("legDir"), label = "Direction De La Légende",  choices = c("Horizontale" = "horizontal", "Verticale" = "vertical"), selected = "horizontal", width = "100%")),
          column(4, sliderInput(ns("legMarginB"), label = "Marge Inférieure De La Légende", min = -5, max = 5, value = .05, step = .01, width = "100%")),
          column(4, selectInput(ns("LegTextColor"), label = "Couleur Des Etiquettes", choices = colors(), selected = "black", width = "100%"))
        ),
        fluidRow(
          column(4,selectInput(ns("legPalette"), label = "Palette de Couleur", choices = c("Température" = "temperature", "Blues" = "blues"), selected = "temperature", width = "100%")),
          column(4,sliderInput(ns("legMarginL"), label = "Marge Gauche De La Légende", min = -5, max = 5, value = 0, step = .01, width = "100%")),
          column(4,selectInput(ns("legLoc"), label = "Position De La Légende", choices = c("En Haut"="top", "En Bas"="bottom", "A Gauche"="left", "A Droite"="right"), selected = "bottom", width = "100%")),

          column(12,textInput(ns("legTitleName"), label = "Titre De La Légende", value = "Variable Interpolée (unité)", placeholder = "Titre légende...", width = "100%")),
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
      showModal(modalDialog(
        tags$h3("Configuration Du Thème Des Etiquettes Des Axes", style="color:#3474A7;family:Georgia;text-align:center;"),
        fluidRow(
          column(4, sliderInput(ns("xAxisTextSize"), label = "Taille de Police Axe-X", min = 5, max = 35, value = 12, step = 1, width = "100%")),
          column(4,selectInput(ns("axisTextColor"), label = "Couleur Des Etiquettes", choices = colors(), selected = "black")),
          column(4, sliderInput(ns("yAxisTextSize"), label = "Taille de Police Axe-Y", min = 5, max = 35, value = 12, step = 1, width = "100%"))
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
      req(bassin)
      bassin
    })

    # gridded cell points in WGS84-4326 proj
    station.in.wgs84<-  reactive(({
      req(stations)
      prec_grid_georef(stations, "+init=epsg:4326")[[1]]
    }))


    # Interpolation data georeferenced in UTM
    interpolationData.WGS<- reactive({
      req(meanPeriod(), station.in.wgs84())
      # req(interpolationData, station.in.wgs84())
      data_cleaning(meanPeriod(), station.in.wgs84())
    })

    ## Summarising data
    meanPeriod<- reactive({
      req(interpolationData)
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
    cleanData<- reactive({ list(meanPeriod(), station.in.wgs84(), station.in.wgs84()) })

    observeEvent(ignoreInit = T, ignoreNULL = T,  input$idw,  {
      # req(bassin, stations, interpolationData)

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

      cleanData(bv_sp)

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

      # # interpolation sur toutes les pas de temps
      # list.idw <- colnames(prec.df)[-1] %>%
      #   purrr::set_names() %>%
      #   purrr::map(
      #     ., ~ gstat::idw(
      #       stats::as.formula(paste(.x, "~ 1")),
      #       locations = gridded.points, newdata = grd
      #     )
      #   )
      #
      # # isolines
      # cont<- as.data.frame(list.idw)  %>%
      #   dplyr::rename(x= db_Moyenne.coords.x1, y= db_Moyenne.coords.x2, var1.pred=db_Moyenne.var1.pred)  %>%
      #   dplyr::select(-c(4, 5))
      # cont.raster<- raster::rasterFromXYZ(cont)
      # cont.raster.mask<-  raster::mask(cont.raster, bassin)
      # cont.raster.mask.fn<-  sf::st_as_sf(raster::rasterToContour(cont.raster.mask))
      # sf::st_crs(cont.raster.mask.fn)<- 4326
      #
      # # transformation du résultat en tibble
      # idw.output<-
      #   as.data.frame(list.idw) %>%
      #   tibble::as_tibble() %>%
      #   dplyr::select(1, 2, tidyselect::ends_with("var1.pred"))
      # # renommage des colonnes
      # names(idw.output)<- c(
      #   "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
      # )
      #
      # idw.output.sf<- idw.output %>%
      #   #* transformation en objet sf
      #   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      #   #* selection des pixels à l'intérieur du contour du bassin
      #   sf::st_filter(bassin)
      #
      # idw.output.df<- idw.output.sf %>%
      #   #* transformation en tibble
      #   tibble::tibble() %>%
      #   #* suppression de la colonne {geometry}
      #   dplyr::select(-geometry) %>%
      #   cbind(sf::st_coordinates(idw.output.sf))
      #
      # #-----------------------------------------------------------------------------------#
      # # # Button settings
      # shinyjs::enable("idw")
      # shinyjs::removeClass(id = "idwAnimate", class = "loading dots")
      #
      # # return
      # plot_data(return(list(idw.output.df, cont.raster.mask.fn)))
      #
      # NULL

    })

    # confirmation
    # observeEvent(input$idw, {
    #   showModal(modalDialog(
    #     tags$h3("Résultats De L'Interpolation [Méthode Utilisée : IDW]", style="color:#3474A7;family:Georgia;text-align:center;"),
    #     # Résumé de l'interpolation
    #     fluidRow(align = "left",
    #              column(12, h4(
    #                "Résultats de l'Interpolation || Résumé Statistique",
    #                style=paste0(
    #                  "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
    #                )
    #              )),
    #              column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("summaryResult")), type = 5))
    #     ),
    #     # Isolignes
    #     fluidRow(align = "left",
    #              column(12, h4(
    #                "Résultats de l'Interpolation || ISOLIGNES",
    #                style=paste0(
    #                  "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
    #                )
    #              )),
    #              column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("isolines")), type = 5))
    #     ),
    #     footer=tagList(
    #       modalButton('Annuler', icon = icon("power-off"))
    #     ),
    #     size = "m"
    #   ))
    #
    #   # Affichage du résult de l'évaluation
    #   output$summaryResult<- renderPrint({
    #     req(plot_data())
    #     summary(plot_data()[[1]])
    #   })
    #
    #   output$isolines<- renderPrint({
    #     req(plot_data())
    #     plot_data()[[2]]
    #   })
    #
    # })

    ### traçage de la carte
    # make_plot <- eventReactive(ignoreNULL = T, ignoreInit = T, input$idw, {
    #   req(plot_data())
    # })

    output$test<- renderPrint({
      req(cleanData())
      cleanData()
    })

  })
}

## To be copied in the UI
# mod_unique_periode_time_serie_interpolation_map_options_ui("unique_periode_time_serie_interpolation_map_options_1")

## To be copied in the server
# mod_unique_periode_time_serie_interpolation_map_options_server("unique_periode_time_serie_interpolation_map_options_1")
