#' loading_scv_excel_layer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loading_scv_excel_layer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyFilesButton(
                      id = ns("tabular_file"), label = "Fichier Excel ou CSV",
                      title = "Sélectionner un Fichier Excel ou CSV !",
                      multiple = FALSE, icon = icon("file"), buttonType = "info", width = "100%"
                    )
             ),
             column(12, verbatimTextOutput(ns("load_tabular_file_confirm")))
    )
  )
}

#' loading_scv_excel_layer Server Functions
#'
#' @noRd
mod_loading_scv_excel_layer_server <- function(id, workingDirectory){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load_vector <- reactiveVal()
    observeEvent(input$tabular_file, {
      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      req(volumes_default_dir())
      shinyFiles::shinyFileChoose(input, "tabular_file", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      # vérification de l'extention du fichier
      extension_fichier<-  reactive({
        req(volumes_default_dir(), file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file)$datapath))
        if(tools::file_ext(shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file)$datapath) %in% c("csv", "xlsx", "xls")){
         return(TRUE)
        }else{
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste(
              "Vous devez choisir un fichier Excel ou CSV !"
            )
          )
          return(FALSE)
        }
      })


      # chargement du fichier tabulaire
      fichier_tabulaire <- reactive({
        req(file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file)$datapath), extension_fichier())
        import_data(shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file)$datapath)
      })

      # vérification des noms de colonnes
      colonnes_station<- reactive({
        req(extension_fichier(), fichier_tabulaire())
        # alert
        if(sum(names(fichier_tabulaire()) == c("Station", "Longitude", "Latitude")) == 3){
          return(TRUE)
        }else{
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste0(
              "Le fichier doit contenir trois colonnes dans cet ordre :",
              " [{Station}, {Longitude}, {Latitude}]"
            )
          )
          return(FALSE)
        }
      })

      # Validation du type des colonnes Longitude et Latitude
      type_columns<- reactive({
        req(colonnes_station(), extension_fichier())
        stations.column.type<- fichier_tabulaire()
        if((is.numeric(stations.column.type$Longitude) + is.numeric(stations.column.type$Latitude))==2){
          return(TRUE)
        }else{
          shinyalert::shinyalert(
            "Erreur Chargement !!",
            paste0(
              "Assurez-vous que les colonnes {Longitude} et {Latitude} soient de type {numeric} !"
            )
          )
        }
      })

      # return
      if(type_columns() & extension_fichier()){
        output$load_tabular_file_confirm<- renderPrint({
          req(extension_fichier(), volumes_default_dir(), file.exists(shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file )$datapath))
          shinyFiles::parseFilePaths(volumes_default_dir(), input$tabular_file )$datapath
        })

        # return
        load_vector(
          fichier_tabulaire() %>%
            mutate(Longitude=round(Longitude,2), Latitude=round(Latitude,2)) %>%
            tibble::column_to_rownames(var = "Station")
        )
      }

    })

    return(list(stations_loaded = reactive({ load_vector()  })))
  })
}

## To be copied in the UI
# mod_loading_scv_excel_layer_ui("loading_scv_excel_layer_1")

## To be copied in the server
# mod_loading_scv_excel_layer_server("loading_scv_excel_layer_1")
