#' set_watershed_delienation_project_folder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_set_watershed_delienation_project_folder_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(align = "center",
             column(12,
                    shinyFiles::shinyDirButton(
                      id = ns("datadir"), label = "Repertoire de Travail",
                      title = "Choisir Le Repertoire de Travail par Défaut !",
                      icon = icon("folder-tree"), buttonType = "info", width = "100%"
                    )

             ),
             column(12, verbatimTextOutput(ns("wd_info")))
    )
  )
}

#' set_watershed_delienation_project_folder Server Functions
#'
#' @noRd
mod_set_watershed_delienation_project_folder_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    volumes <- c(Home = fs::path_home(), "R Installation" = base::R.home(), shinyFiles::getVolumes()())

    # DATADIR
    shinyFiles::shinyDirChoose(input, "datadir", roots = volumes, session = session,
                   restrictions = system.file(package = "base"))

    workingDir<- reactive({
      req(input$datadir)
      shinyFiles::parseDirPath(volumes, input$datadir)
    })

    observeEvent(workingDir(), {
      output$wd_info<- renderPrint({
        req(dir.exists(workingDir()))
        workingDir()
      })
    })


    # return working directory
    return(list(
      wdir = reactive({ workingDir() })
    ))

  })
}

## To be copied in the UI
# mod_set_watershed_delienation_project_folder_ui("set_watershed_delienation_project_folder_1")

## To be copied in the server
# mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
