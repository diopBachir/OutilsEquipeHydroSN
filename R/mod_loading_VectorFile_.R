#' loading_VectorFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_VectorFile_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(
      ns("vector_file"), label = "Limites Bassin Versant",  accept = c(
        '.gpkg', '.shp','.dbf','.sbn','.sbx','.shx','.prj'
      ),
      multiple = T, buttonLabel = "Charger...", placeholder = "Fichier Vectoriel"
    )
  )
}

#' loading_VectorFile Server Functions
#'
#' @noRd
mod_loading_VectorFile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_vector <- reactiveVal()
    observeEvent(input$vector_file, {
      req(input$vector_file)

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        tools::file_ext(input$vector_file$datapath)[1] == 'gpkg' |
          sum(tools::file_ext(input$vector_file$datapath) %in% c('shp', 'dbf', 'prj', 'shx'))==4
        # c('shp','dbf','sbn','sbx','shx','prj', "cpg", "xml")) == length(input$vector_file$datapath)
      })

      if(!extension_fichier()){
        shinyFeedback::hideFeedback("vector_file")
        shinyFeedback::feedbackWarning(
          "vector_file", !extension_fichier(),
          "[{.gpkg}] ou [{.shp}] Requis !"
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous devez choisir un fichier géopackage {.gpkg) ou shapefile {.shp}. ",
            "Si vous importez un ficher shapefile, assurez-vous qu'au moins ces quatres ",
            "sont présents dans le même repertoire d'importation [shp, dbf, prj, shx]. ",
            "Cela est du au fait que seul le fichier shapefile ne suffit pas à l'importation des shapefiles !"
          )
        )
      }

      # chargement du bassin
      bassin_contours <- reactive({
        req(extension_fichier())
        if(tools::file_ext(input$vector_file$datapath)[1] == 'gpkg'){
          sf::st_read(input$vector_file$datapath)
        }else{
          read_shp(input$vector_file)
        }
      })

      # Vérification de la projection du shapefile
      req(bassin_contours())
      proj_file<- sf::st_crs(bassin_contours())$input == "WGS 84"
      # alert
      if(!proj_file){
        shinyFeedback::hideFeedback("vector_file")
        shinyFeedback::feedbackWarning(
          "vector_file", !proj_file,
          "Projection WGS84 | EPSG:4326 Requis !"
        )
        shinyalert::shinyalert(
          "Erreur de chargement !",
          "Veillez reprojeter la couche vectorielle vers le EPSG 4326 (WGS84) !"
        )
      }

      if(proj_file){
        shinyFeedback::hideFeedback("vector_file")
        shinyFeedback::feedbackSuccess(
          "vector_file", proj_file,
          paste0(
            "Ficher Vecteur Chargé Avec Succès ", icon("check")
          )
        )
        load_vector(bassin_contours())
      }

    })

    return(list(limite_bassin_versant = reactive({ load_vector() })))

  })
}

## To be copied in the UI
# mod_loading_VectorFile_ui("loading_VectorFile_1")

## To be copied in the server
# mod_loading_VectorFile_server("loading_VectorFile_1")
