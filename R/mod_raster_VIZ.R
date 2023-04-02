#' raster_VIZ UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_raster_VIZ_ui <- function(id){
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
                height: 2.3em;
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

    uiOutput(ns("raster_viz_ui")),
    uiOutput(ns("layers_infos"))
  )
}

#' raster_VIZ Server Functions
#'
#' @noRd
mod_raster_VIZ_server <- function(id, raster_layer, stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # tmap::tmap_mode("view")

    output$raster_viz_ui<- renderUI({
      req(raster_layer(), stations())
      fluidRow(
        column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("raster_layer_viz"))))
      )
    })

    # layers info
    output$layers_infos<- renderUI({
      req(raster_layer(), stations())
      modalDialog(
        fluidRow(
          column(6, h4("Fichier Raster [NetCDF]", style = "color:#3474A7;text-align:center;background-color:lightgray")),
          column(6,  h4("Stations", style = "color:#3474A7;text-align:center;background-color:lightgray")),
          column(6, shinycssloaders::withSpinner(verbatimTextOutput(ns("rast")))),
          column(6, shinycssloaders::withSpinner(verbatimTextOutput(ns("vect"))))
        ),
        title = "Données Chargées",
        # footer = tagList(
        #   actionButton(ns("fermer_infos"), "Fermer", class = "btn btn-info")
        # ),
        size = "l", easyClose = TRUE, label	= "Fermer"
      )
    })

    observeEvent(input$fermer_infos, {
      removeModal()
    })

    output$rast<- renderPrint({
      req(raster_layer())
      raster_layer()
    })

    output$vect<- renderPrint({
      req(stations())
      stations()
    })

    # tmap plot
    tmap_plt<- reactive({
      req(raster_layer(), stations())

      stations_sf<- sf::st_as_sf(stations(), coords = c("Longitude", "Latitude"), crs = 4326)

      tmap::tmap_mode("view")
      tmap::tm_shape(raster_layer()[[1]])+
        tmap::tm_raster(style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = "Altitude")+
        tmap::tm_scale_bar()+
        tmap::tm_shape(stations_sf)+
        tmap::tm_dots(col = "red", size = .04, shape = 15)
    })

    # rendering plot
    output$raster_layer_viz<- leaflet::renderLeaflet({
      req(tmap_plt())
      # coordonnées centrales du DEM
      # map_view_coords<- get_raster_central_cell_coord(raster_layer())

      stations_sf<- sf::st_as_sf(stations(), coords = c("Longitude", "Latitude"), crs = 4326)

      bounds <- stations_sf %>%
        sf::st_bbox() %>%
        as.character()

      # markers
      reds <- leaflet::makeAwesomeIcon(icon='ion-waterdrop', library='ion', markerColor = 'blue', iconColor = 'white')


      tmap::tmap_leaflet(tmap_plt(), in.shiny = TRUE) %>%
        leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        leaflet::addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
        leaflet::addProviderTiles("Stamen.Terrain", group = "Stamen.Terrain") %>%
        leaflet::addProviderTiles("Stamen.Watercolor", group = "Stamen.Watercolor") %>%
        leaflet::addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
        leaflet::addProviderTiles("Wikimedia", group = "Wikimedia") %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
        # add a layers control
        leaflet::addLayersControl(
          baseGroups = c(
            "Wikimedia", "Esri.WorldImagery", "OpenStreetMap", "Stamen.Toner",
            "Stamen.Terrain", "Esri.WorldStreetMap", "Stamen.Watercolor", "CartoDB.Positron"
          ),
          # position it on the topleft
          position = "topleft"
        )  %>%
        leaflet.extras::addResetMapButton()
        # leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])

        # leaflet::setView(
        #   lng = map_view_coords[1], lat = map_view_coords[2], zoom = 8.5
        # )
    })

  })
}

## To be copied in the UI
# mod_raster_VIZ_ui("raster_VIZ_1")

## To be copied in the server
# mod_raster_VIZ_server("raster_VIZ_1")
