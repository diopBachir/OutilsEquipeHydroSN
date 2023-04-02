#' loading_RasterFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_RasterFile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyFilesButton(
                      id = ns("raster_file"), label = "Raster [EPSG:4326]",
                      title = "Sélectionner un Ficher Raster !",
                      multiple = FALSE, icon = icon("layer-group"), buttonType = "info",
                      width = "100%"
                    )
             ),
             column(12, verbatimTextOutput(ns('test_file_select_ready')))
    )
  )
}

#' loading_RasterFile Server Functions
#'
#' @noRd
mod_loading_RasterFile_server <- function(id, workingDirectory){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_raster <- reactiveVal()
    raster_path<- reactiveVal()
    observeEvent(input$raster_file, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      req(volumes_default_dir())
      shinyFiles::shinyFileChoose(input, "raster_file", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      # pour parser le fichier choisi
      # volumes<- c(Home = fs::path_home(), "R Installation" = base::R.home(), shinyFiles::getVolumes()())

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(file.exists(volumes_default_dir(), parseFilePaths(volumes_default_dir(), input$raster_file)$datapath))
        tools::file_ext(parseFilePaths(volumes_default_dir(), input$raster_file)$datapath) == "nc"
      })

      req(extension_fichier())
      if(!extension_fichier()){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous devez choisir un fichier [.tif','.tiff','.TIF','.TIFF']"
          )
        )
      }

      # chargement du MNT
      fichier_raster <- reactive({
        req(file.exists(parseFilePaths(volumes_default_dir(), input$raster_file)$datapath), extension_fichier())
        raster::raster(parseFilePaths(volumes_default_dir(), input$raster_file)$datapath)
      })

      # Vérification de la projection du shapefile
      # req(fichier_raster())
      # proj_file<- sf::st_crs(fichier_raster())$input == "WGS 84"
      # alert
      output$test_file_select_ready<- renderPrint({
        req(volumes_default_dir(), file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$raster_file)$datapath), extension_fichier())
        shinyFiles::parseFilePaths(volumes_default_dir(), input$raster_file)$datapath
      })

      # send data
      req(fichier_raster())
      load_raster(list(
        raster::raster(shinyFiles::parseFilePaths(volumes_default_dir(), input$raster_file)$datapath)
      ))
      raster_path(shinyFiles::parseFilePaths(volumes_default_dir(), input$raster_file)$datapath)

    })

    return(list(
      raster_georeferenced = reactive({ load_raster() }),
      raster_path = reactive({ raster_path() })
    ))

  })
}

## To be copied in the UI
# mod_loading_RasterFile_ui("loading_RasterFile_1")

## To be copied in the server
# mod_loading_RasterFile_server("loading_RasterFile_1")
