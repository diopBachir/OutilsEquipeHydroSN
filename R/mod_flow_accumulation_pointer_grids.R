#' flow_accumulation_pointer_grids UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_flow_accumulation_pointer_grids_ui <- function(id){
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

    fluidRow(align = "center",
             column(3,
                    shinyFiles::shinySaveButton(
                      id = ns("wbt_d8_flow_accumulation"),
                      label=span("Flow Acc. [step4]", id=ns("wbt_d8_flow_accumulation_animate"), style = "font-size:90%;"),
                      filetype = list(picture ="tif"),title = "Sauvegarder Le Ficher Comme",
                      multiple = FALSE, icon = icon("compress-arrows-alt"), buttonType = "info"
                    )
             ),
             column(3,
                    shinyFiles::shinySaveButton(
                      id = ns("wbt_d8_pointer"),
                      label=span("D8 Pointer [step5]", id=ns("wbt_d8_pointer_animate"), style = "font-size:90%;"),
                      filetype = list(picture ="tif"), title = "Sauvegarder Le Ficher Comme",
                      multiple = FALSE, icon = icon("compress-arrows-alt"), buttonType = "info"
                    )
             ),
             column(3,
                    shinyFiles::shinySaveButton(
                      id = ns("wbt_extract_streams"),
                      label=span("Stream [step6]", id=ns("wbt_extract_streams_animate"), style = "font-size:90%;"),
                      filetype = list(picture ="tif"), title = "Sauvegarder Le Ficher Comme",
                      multiple = FALSE, icon = icon("water"), buttonType = "info"
                    )
             ),
             column(3,
                    shinyFiles::shinySaveButton(
                      id = ns("jenson_snap_pour_points_wbt"),
                      label=span("JensonSnap [step7]", id=ns("jenson_snap_pour_points_wbt_animate"), style = "font-size:90%;"),
                      filetype = list(picture ="shp"), title = "Sauvegarder Le Ficher Comme",
                      multiple = FALSE, icon = icon("location"), buttonType = "info"
                    )
             ),
             column(12, tags$hr(style="border-color:gray;")),
             column(12, verbatimTextOutput(ns("test"))),
             column(12, uiOutput(ns("plotting_result")))
    )

  )
}

#' flow_accumulation_pointer_grids Server Functions
#'
#' @noRd
#'
mod_flow_accumulation_pointer_grids_server <- function(
    id, workingDirectory, filled_n_brushed_dem_path, seuil_bassin,
    pour_points_layer, pour_points_path, pour_points_snap_distance
  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # flow accumulation results
    flow_accumulation<- reactiveVal()
    # pointer grid results
    pointer_file<- reactiveVal()
    # streams path
    streams_path<- reactiveVal()
    # pourpoints snapped
    pourpoints_snapped<- reactiveVal()

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 4 : Flux d'accumulation
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$wbt_d8_flow_accumulation, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(filled_n_brushed_dem_path(), !is.null(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "wbt_d8_flow_accumulation", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      flowAccumulationPath<- reactive({
        req(filled_n_brushed_dem_path(), shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_d8_flow_accumulation)$datapath)
        shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_d8_flow_accumulation)$datapath
      })

      # flow accumulation
      flow_accumulation_process_elapsed_time<- whitebox::wbt_d8_flow_accumulation(
        input = filled_n_brushed_dem_path(), output = flowAccumulationPath()
      )

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(flowAccumulationPath()))
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("process_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(flowAccumulationPath()))
        tags$h4("Opération 4 : Créer un Raster de Flux Cumulé (D8 Flow Acc.) dans Chaque Cellule", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(flow_accumulation_process_elapsed_time)
        flow_accumulation_process_elapsed_time
      })

      # plot
      output$process_results_viz <- renderPlot({
        req(file.exists(flowAccumulationPath()), pour_points_layer)

        # load filled DEM
        flow_accumulation_raster<- raster::raster(flowAccumulationPath())

        crs(flow_accumulation_raster)<- "+proj=longlat +datum=WGS84"
        flow_accumulation_raster[flow_accumulation_raster < 0] <- 0

        # send result
        flow_accumulation(flowAccumulationPath())

        # plot
        tmap::tmap_mode("plot")
        tmap::tm_shape(flow_accumulation_raster)+
          tmap::tm_raster(
            style = "cont", palette = "Blues", legend.show = FALSE, midpoint = NA, title = ""
          )+
          tmap::tm_scale_bar()+
          tmap::tm_shape(pour_points_layer)+
          tmap::tm_dots(col = "red", size = .5)
      })

    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 5 : D8 Pointer Grid
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$wbt_d8_pointer, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(filled_n_brushed_dem_path(), !is.null(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "wbt_d8_pointer", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      d8PointerGridPath<- reactive({
        req(filled_n_brushed_dem_path(), shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_d8_pointer)$datapath)
        shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_d8_pointer)$datapath
      })

      # d8 pointer grid
      d8_pointer_grid_process_elapsed_time<- whitebox::wbt_d8_pointer(
        dem = filled_n_brushed_dem_path(), output = d8PointerGridPath()
      )

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(d8PointerGridPath()))
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("process_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(d8PointerGridPath()))
        tags$h4("Opération 5 : Créer un Raster de Pointeur de Flux (D8 Pointer Grid) à partir du DEM préparé", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(d8_pointer_grid_process_elapsed_time)
        d8_pointer_grid_process_elapsed_time
      })

      # plot
      output$process_results_viz <- renderPlot({
        req(file.exists(d8PointerGridPath()))

        # load filled DEM
        d8_pointer_raster<- raster::raster(d8PointerGridPath())

        crs(d8_pointer_raster)<- "+proj=longlat +datum=WGS84"
        d8_pointer_raster[d8_pointer_raster < 0] <- 0

        # send result
        pointer_file(d8PointerGridPath())

        # plot
        tmap::tmap_mode("plot")
        tmap::tm_shape(d8_pointer_raster)+
          tmap::tm_raster(
            style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = ""
          )+
          tmap::tm_scale_bar()
          # tmap::tm_shape(pour_points_layer)+
          # tmap::tm_dots(col = "red", size = 1.2, shape = 20)
      })

    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 6 : Extraction du réseau hydrographique
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$wbt_extract_streams, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(flow_accumulation(), seuil_bassin(), dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "wbt_extract_streams", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      streamsPath<- reactive({
        req(flow_accumulation(), seuil_bassin(), pour_points_layer, file.exists(shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_extract_streams)$datapath))
        shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_extract_streams)$datapath
      })

      # d8 pointer grid
      streams_extraction_process_elapsed_time<- whitebox::wbt_extract_streams(
        flow_accum = flow_accumulation(), output = streamsPath(), threshold = seuil_bassin()
      )

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(streamsPath()), pour_points_layer)
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("streams_extraction_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(streamsPath()))
        tags$h4(paste0(
          "Opération 6 : Créer une Grille de Flux Raster (Réseau Hydrographique) en utilisant un Seuil d'Accumulation de Flux definit dans le ",
          "champ {Seuil De Chenalisation} à partir de la Grille d'Accumulation de Flux (D8 Flow Accumulation)"
         ), style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;"
        )
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(streams_extraction_process_elapsed_time)
        streams_extraction_process_elapsed_time
      })

      # tmap plot
      tmap_plt<- reactive({
        req(file.exists(streamsPath()), pour_points_layer)

        # load filled DEM
        streams_raster<- raster::raster(streamsPath())

        crs(streams_raster)<- "+proj=longlat +datum=WGS84"
        streams_raster[streams_raster < 0] <- 0

        # convert raster streams to polyline
        streams_vector <- stars::st_as_stars(streams_raster) %>%
          sf::st_as_sf(merge = TRUE) %>% # this is the raster to polygons part
          sf::st_cast("MULTILINESTRING") # cast the polygons to polylines

        # send result
        streams_path(streamsPath())

        # tmap::tmap_mode("view")
        tmap::tm_shape(streams_vector)+
          tmap::tm_lines(col = "red", lwd = 1.2, legend.show = TRUE, midpoint = NA)+
          tmap::tm_scale_bar()+
          tmap::tm_shape(pour_points_layer)+
          tmap::tm_dots(col = "red", size = .05)
      })

      # rendering plot
      # rendering plot
      output$streams_extraction_results_viz<- leaflet::renderLeaflet({
        req(tmap_plt(), pointer_file())

        # load filled DEM
        pointer_grid<- raster::raster(pointer_file())

        crs(pointer_grid)<- "+proj=longlat +datum=WGS84"
        pointer_grid[pointer_grid < 0] <- 0

        # coordonnées centrales du DEM
        map_view_coords<- get_raster_central_cell_coord(pointer_grid)

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
              "Esri.WorldImagery", "Stamen.Watercolor", "OpenStreetMap", "Stamen.Toner",
              "Stamen.Terrain", "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron"
            ),
            # position it on the topleft
            position = "topleft"
          ) %>%
          leaflet.extras::addResetMapButton() %>%
          leaflet::setView(
            lng = map_view_coords[1], lat = map_view_coords[2], zoom = 8.5
          )
      })

    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 7 : Snap Pourpoints
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$jenson_snap_pour_points_wbt, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(streams_path(), pour_points_snap_distance(), pour_points_path, dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })
      # volumes_default_dir <- reactive({
      #   # req(streams_path(), pour_points_snap_distance(), pour_points_path, dir.exists(workingDirectory))
      #   c(Home = "C:/Users/hp", "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      # })

      # file path
      shinyFiles::shinyFileSave(input, "jenson_snap_pour_points_wbt", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      ppSnappedPath<- reactive({
        req(shinyFiles::parseSavePath(volumes_default_dir(), input$jenson_snap_pour_points_wbt)$datapath)
        shinyFiles::parseSavePath(volumes_default_dir(), input$jenson_snap_pour_points_wbt)$datapath
      })

      # d8 pointer grid
      pourpoints_snap_process_elapsed_time<- whitebox::wbt_jenson_snap_pour_points(
        pour_pts = pour_points_path, streams = streams_path(), output = ppSnappedPath(), snap_dist = pour_points_snap_distance()
      )

      output$plotting_result<- renderUI({
        if(file.exists(ppSnappedPath())){
          fluidRow(
            column(12, uiOutput(ns("process_verbose_title_2"))),
            column(12, verbatimTextOutput(ns("process_verbose_2"))),
            column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("process_results_viz_2"))))
          )
        }else{
          span("Oups !!! L'opération a échoué", icon("exclamation-circle"), style="color:red;font-family:Georgia")
        }
      })

      # # --------------------------------Render Result

      # operation description
      output$process_verbose_title_2<-renderUI({
        tags$h4("Opération 7 : Déplacer les Exutoires (par accrochage) vers la Cellule (pixel) de Flux la plus Proche", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose_2<- renderPrint({
        req(pourpoints_snap_process_elapsed_time)
        pourpoints_snap_process_elapsed_time
      })

      # tmap plot
      tmap_plt<- reactive({
        req(file.exists(ppSnappedPath()), streams_path())

        # load stream raster
        streams_raster2<- raster::raster(streams_path())

        crs(streams_raster2)<- "+proj=longlat +datum=WGS84"
        streams_raster2[streams_raster2 < 0] <- 0

        # convert raster streams to polyline
        streams_vector2 <- stars::st_as_stars(streams_raster2) %>%
          sf::st_as_sf(merge = TRUE) %>% # this is the raster to polygons part
          sf::st_cast("MULTILINESTRING") # cast the polygons to polylines

        # load pourpoints snapped
        pp <- raster::shapefile(ppSnappedPath())
        pp_orig <- raster::shapefile(pour_points_path)

        # send result
        pourpoints_snapped(ppSnappedPath())

        # plot
        tmap::tmap_mode("view")
        tmap::tm_shape(streams_vector2)+
          tmap::tm_lines(col = "red", lwd = 1?2, legend.show = TRUE, midpoint = NA)+
          tmap::tm_scale_bar()+
          tmap::tm_shape(pp)+
          tmap::tm_dots(col = "red", size = .05, title = "Snapped Pourpoints", legend.show = TRUE)+
          tmap::tm_shape(pp_orig)+
          tmap::tm_dots(col = "black", size = .05, title = "OriginalPourpoints", legend.show = TRUE)
      })

      # plot
      output$process_results_viz_2 <- leaflet::renderLeaflet({
        req(tmap_plt(), file.exists(pointer_file()))

        # load filled DEM
        pointer_grid<- raster::raster(pointer_file())

        crs(pointer_grid)<- "+proj=longlat +datum=WGS84"
        pointer_grid[pointer_grid < 0] <- 0

        # coordonnées centrales du DEM
        map_view_coords<- get_raster_central_cell_coord(pointer_grid)

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
              "Esri.WorldImagery", "Stamen.Watercolor", "OpenStreetMap", "Stamen.Toner",
              "Stamen.Terrain", "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron"
            ),
            # position it on the topleft
            position = "topleft"
          ) %>%
          leaflet.extras::addResetMapButton() %>%
          leaflet::setView(lng = map_view_coords[1], lat = map_view_coords[2], zoom = 8.5)
      })

    })

  })
}

## To be copied in the UI
# mod_flow_accumulation_pointer_grids_ui("flow_accumulation_pointer_grids_1")

## To be copied in the server
# mod_flow_accumulation_pointer_grids_server("flow_accumulation_pointer_grids_1")
