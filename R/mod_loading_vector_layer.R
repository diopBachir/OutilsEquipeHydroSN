#' loading_vector_layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_vector_layer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyFilesButton(
                      id = ns("vector_file"), label = "Fichier Vecteur [EPSG 4326]",
                      title = "Sélectionner un Fichier Vectoriel !",
                      multiple = FALSE, icon = icon("file"), buttonType = "info", width = "100%"
                    )
             ),
             column(12, verbatimTextOutput(ns("load_vector_file_confirm")))
    )
  )
}

#' loading_vector_layer Server Functions
#'
#' @noRd
mod_loading_vector_layer_server <- function(id, workingDirectory){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_vector <- reactiveVal()
    observeEvent(input$vector_file, {
      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      req(volumes_default_dir())
      shinyFiles::shinyFileChoose(input, "vector_file", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(volumes_default_dir(), file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath))
        tools::file_ext(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath) == "shp"
      })

      req(extension_fichier())
      if(!extension_fichier()){
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous devez choisir un fichier shapefile. Assurez-vous que les méta-fichiers du fichier shapefile ",
            "[shp, dbf, prj, shx] sont tous présents dans le même repertoire. Cela est du au fait que seul le ",
            "fichier shapefile ne suffit pas à l'importation des shapefiles (dans {shiny})!"
          )
        )
      }

      # chargement du MNT
      fichier_vecteur <- reactive({
        req( file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath), extension_fichier())
        sf::st_read(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath)
      })

      # Vérification de la projection du shapefile
      req(fichier_vecteur())
      proj_file<- sf::st_crs(fichier_vecteur())$input == "WGS 84"
      # alert
      if(!proj_file){
        shinyalert::shinyalert(
          "Erreur de chargement !",
          "Veillez reprojeter la Couche Vecteur vers le EPSG 4326 (WGS84) !"
        )
      }else{
        output$load_vector_file_confirm<- renderPrint({
          req(volumes_default_dir(), file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath), proj_file)
          shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath
        })

        # send data
        load_vector(list(
          sf::st_read(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath),
          shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath
        ))
      }

    })

    return(list(couche_vectorielle = reactive({ load_vector() })))

  })
}

## To be copied in the UI
# mod_loading_vector_layer_ui("loading_vector_layer_1")

## To be copied in the server
# mod_loading_vector_layer_server("loading_vector_layer_1")
