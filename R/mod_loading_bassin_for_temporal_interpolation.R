#' loading_bassin_for_temporal_interpolation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_bassin_for_temporal_interpolation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyFilesButton(
                      id = ns("vector_file"), label = "Limite Bassins versant",
                      title = "Sélectionner Le Fichier Shapefile de la Limite du Bassin Versant !",
                      multiple = FALSE, icon = icon("file"), buttonType = "info", width = "100%"
                    )
             ),
             column(12, verbatimTextOutput(ns("load_vector_file_confirm")))
    )
  )
}

#' loading_bassin_for_temporal_interpolation Server Functions
#'
#' @noRd
mod_loading_bassin_for_temporal_interpolation_server <- function(id, workingDirectory){
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
      shinyFiles::shinyFileChoose(input, "vector_file", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      # chargement du fichier
      chemin_fichier_vectorielle <- reactive({
        req(file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath))
        shinyFiles::parseFilePaths(volumes_default_dir(), input$vector_file)$datapath
      })

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(chemin_fichier_vectorielle())
        if(!tools::file_ext(chemin_fichier_vectorielle()) %in% c("shp", "gpkg")){
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez choisir un fichier Geopackage ou Shapefile. Si vous importez un Shapefile [.shp] ",
              "Assurez-vous que les méta-fichiers du fichier shapefile, ou au moins les quatres fichiers",
              "[shp, dbf, prj, shx] sont tous présents dans le même repertoire que le fichier {.shp} importé. Cela est du au fait que seul le ",
              "fichier shapefile ne suffit pas à l'importation des shapefiles (dans {shiny})!"
            )
          )
          return(FALSE)
        }else{
          return(TRUE)
        }
      })

      # chargement du MNT
      fichier_vecteur <- reactive({
        req(chemin_fichier_vectorielle(), extension_fichier())
        sf::st_read(chemin_fichier_vectorielle())
      })

      # Vérification de la projection du shapefile
      proj_file<- reactive({
        req(fichier_vecteur())
        sf::st_crs(fichier_vecteur())$input == "WGS 84"
      })

      # alert
      if(proj_file()==FALSE){
        shinyalert::shinyalert(
          "Erreur de chargement !",
          "Veillez reprojeter la Couche Vecteur vers le EPSG 4326 (WGS84) !"
        )
        load_vector(NULL)
      }else{

        output$load_vector_file_confirm<- renderPrint({
          req(proj_file(), fichier_vecteur(), chemin_fichier_vectorielle())
          chemin_fichier_vectorielle()
        })

        # send data
        load_vector(sf::st_read(chemin_fichier_vectorielle()))

      }

    })

    return(list(limite_bassin_versant = reactive({ load_vector() })))

  })
}

## To be copied in the UI
# mod_loading_bassin_for_temporal_interpolation_ui("loading_bassin_for_temporal_interpolation_1")

## To be copied in the server
# mod_loading_bassin_for_temporal_interpolation_server("loading_bassin_for_temporal_interpolation_1")
