#' MNT_VIZ UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MNT_VIZ_ui <- function(id){
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

    uiOutput(ns("mnt_viz_ui"))
  )
}

#' MNT_VIZ Server Functions
#'
#' @noRd
mod_MNT_VIZ_server <- function(id, mnt_layer, outlets_layer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # tmap::tmap_mode("view")

    output$mnt_viz_ui<- renderUI({
      req(mnt_layer, outlets_layer)
      fluidRow(
        column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("mnt_layer_viz"))))
      )
    })

    # tmap plot
    tmap_plt<- reactive({
      req(mnt_layer, outlets_layer)

      tmap::tmap_mode("view")
      tmap::tm_shape(mnt_layer)+
        tmap::tm_raster(style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = "Altitude")+
        tmap::tm_scale_bar()+
        tmap::tm_shape(outlets_layer)+
        tmap::tm_dots(col = "black", size = .04)
    })

    # rendering plot
    output$mnt_layer_viz<- leaflet::renderLeaflet({
      req(tmap_plt())
      # coordonnées centrales du DEM
      map_view_coords<- get_raster_central_cell_coord(mnt_layer)

      tmap::tmap_leaflet(tmap_plt(), in.shiny = TRUE) %>%
        leaflet.extras::addResetMapButton() %>%
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
            "Stamen.Watercolor", "Esri.WorldImagery", "OpenStreetMap", "Stamen.Toner",
            "Stamen.Terrain", "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron"
          ),
          # position it on the topleft
          position = "topleft"
        ) %>%
        leaflet::setView(
          lng = map_view_coords[1], lat = map_view_coords[2], zoom = 8.5
        )
    })

  })
}

## To be copied in the UI
# mod_MNT_VIZ_ui("MNT_VIZ_1")

## To be copied in the server
# mod_MNT_VIZ_server("MNT_VIZ_1")
