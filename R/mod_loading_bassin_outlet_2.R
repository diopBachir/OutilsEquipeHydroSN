#' loading_bassin_outlet_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_bassin_outlet_2_ui <- function(id){
  ns <- NS(id)
 fluidRow(
   column(12,
          shinyFiles::shinyFilesButton(
            id = ns("datafile"), label = "Charger le ficher des exutoires [shp | gpkg]",
            title = "Veillez sélectionner Le fichier Shapefile ou Geopackage des exutoires des bassins versants !",
            multiple = FALSE, icon = icon("file"), buttonType = "info",
            width = "100%"
          )
   ),
   shiny::verbatimTextOutput(ns('test_dir_select_ready'))
 )
}

#' loading_bassin_outlet_2 Server Functions
#'
#' @noRd
mod_loading_bassin_outlet_2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    volumes <- c(Home = fs::path_home(), "R Installation" = base::R.home(), shinyFiles::getVolumes()())

    # DATADIR
    shinyFiles::shinyFileChoose(input, "datafile", roots = volumes, session = session,
                                restrictions = base::system.file(package = "base"))

    load_vector <- reactiveVal()
    observeEvent(input$datafile, {
      req(input$datafile)

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(input$datafile)
        tools::file_ext(parseFilePaths(volumes, input$datafil)$datapath) == c('shp')
      })

      output$test_dir_select_ready<- renderPrint({
        req(parseFilePaths(volumes, input$datafile)$datapath)
        parseFilePaths(volumes, input$datafile)$datapath
      })

      # if(!extension_fichier()){
      #   shinyFeedback::hideFeedback("datafile")
      #   shinyFeedback::feedbackWarning(
      #     "datafile", !extension_fichier(),
      #     "Fichier [{.shp}] Requis !"
      #   )
      # }
      #
      # # chargement du bassin
      # bassin_outlets <- reactive({
      #   req(extension_fichier())
      #   sf::st_read(parseFilePaths(volumes, input$datafile$datapath))
      # })
      #
      # # Vérification de la projection du shapefile
      # req(bassin_outlets())
      # proj_file<- sf::st_crs(bassin_outlets())$input == "WGS 84"
      # # alert
      # if(!proj_file){
      #   shinyFeedback::hideFeedback("vector_file")
      #   shinyFeedback::feedbackWarning(
      #     "vector_file", !proj_file,
      #     "Projection WGS84 | EPSG:4326 Requis !"
      #   )
      #   shinyalert::shinyalert(
      #     "Erreur de chargement !",
      #     "Veillez reprojeter la couche vectorielle vers le EPSG 4326 (WGS84) !"
      #   )
      # }
      #
      # if(proj_file){
      #   shinyFeedback::hideFeedback("datafile")
      #   shinyFeedback::feedbackSuccess(
      #     "datafile", proj_file,
      #     paste0(
      #       "Ficher Vecteur Chargé Avec Succès ", icon("check")
      #     )
      #   )
      #   # send data
      #   load_vector(list(bassin_outlets(), parseFilePaths(volumes, input$datafile$datapath)))
      #
      #   # show layer info
      #   # dialog modal
      #   req(load_vector())
      #   showModal(modalDialog(
      #     tags$h4("Chargement des Exutoires", style="color:green;family:Georgia;text-align:right;"),
      #     # affichage des résultats
      #     fluidRow(
      #       column(12, verbatimTextOutput(ns("layer_info")))
      #     ),
      #     footer=tagList(
      #       modalButton("Fermer", icon = icon("power-off"))
      #     ),
      #     size = "l"
      #   ))
      #
      #   output$layer_info<- renderPrint({
      #     req(load_vector())
      #     sf::st_read(parseFilePaths(volumes, input$datafile$datapath))
      #   })
      # }

    })

    # return(list(exutoires_bassin_versants = reactive({ load_vector() })))

  })
}

## To be copied in the UI
# mod_loading_bassin_outlet_2_ui("loading_bassin_outlet_2_1")

## To be copied in the server
# mod_loading_bassin_outlet_2_server("loading_bassin_outlet_2_1")
