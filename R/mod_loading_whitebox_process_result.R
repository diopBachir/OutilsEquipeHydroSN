#' loading_whitebox_process_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_whitebox_process_result_ui <- function(id, label=""){
  ns <- NS(id)
  tagList(
    fileInput(
      ns("raster_file"), label = label,  accept = c(
        '.nc', '.nc4','.tif','.tiff','.TIF','.TIFF'
      ),
      multiple = F, buttonLabel = "Charger...", placeholder = "Fichier Raster"
    )
  )
}

#' loading_whitebox_process_result Server Functions
#'
#' @noRd
mod_loading_whitebox_process_result_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_raster <- reactiveVal()
    observeEvent(input$raster_file, {
      req(input$raster_file)

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        tools::file_ext(input$raster_file$datapath) %in% c('nc', 'nc4','tif','tiff','TIF','TIFF')
      })

      if(!extension_fichier()){
        shinyFeedback::hideFeedback("raster_file")
        shinyFeedback::feedbackWarning(
          "raster_file", !extension_fichier(),
          "Type de Fichier Incorrect !"
        )
        shinyalert::shinyalert(
          "Erreur Chargement !!",
          paste(
            "Vous devez choisir un fichier ['.nc', '.nc4','.tif','.tiff','.TIF','.TIFF']"
          )
        )
      }

      # chargement du MNT
      fichier_raster <- reactive({
        req(extension_fichier())
        raster::raster(input$raster_file$datapath, crs = '+init=EPSG:4326')
      })

      # Vérification de la projection du shapefile
      req(fichier_raster())
      proj_file<- sf::st_crs(fichier_raster())$input == "WGS 84"
      # alert
      if(!proj_file){
        shinyFeedback::hideFeedback("raster_file")
        shinyFeedback::feedbackWarning(
          "raster_file", !proj_file,
          "Projection WGS84 | EPSG:4326 Requis !"
        )
        shinyalert::shinyalert(
          "Erreur de chargement !",
          "Veillez reprojeter la Couche Raster vers le EPSG 4326 (WGS84) !"
        )
      }

      if(proj_file){
        shinyFeedback::hideFeedback("raster_file")
        shinyFeedback::feedbackSuccess(
          "raster_file", proj_file,
          paste0(
            "Ficher Raster Chargé Avec Succès ", icon("check")
          )
        )

        # dialog modal
        showModal(modalDialog(
          tags$h4("Description de la Couche Raster", style="color:green;family:Georgia;text-align:right;"),
          # affichage des résultats
          fluidRow(
            column(12,  verbatimTextOutput(ns("raster_info")))
          ),
          footer=tagList(
            modalButton("Fermer", icon = icon("power-off"))
          ),
          size = "l"
        ))

        # show raster info
        output$raster_info<- renderPrint({
          req(fichier_raster())
          fichier_raster()
        })
        # send data
        load_raster(list(fichier_raster(), input$raster_file$datapath))
      }
    })

    return(list(raster_result_layer = reactive({ load_raster() })))

  })
}

## To be copied in the UI
# mod_loading_whitebox_process_result_ui("loading_whitebox_process_result_demFilled")

## To be copied in the server
# mod_loading_whitebox_process_result_server("loading_whitebox_process_result_demFilled")
