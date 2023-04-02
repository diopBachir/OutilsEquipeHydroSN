#' cru_data_extract UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cru_data_extract_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style("#final_cru_data_summary{font-size:7px; font-style:italic;overflow-y:scroll; background: ghostwhite;}")),

    fluidRow(
      column(4,
             fluidRow(
               fluidRow(align = "center", style = "background-color:lightgray",
                 column(12,
                        selectInput(
                          ns("variable"), span("Variable à Extraire", style="color:white;font-face:bold;font-family:Georgia;font-size:105%"),
                          c("dtr", "frs", "pet", "cld", "pre", "rhm", "ssh", "tmp", "tmn", "tmx", "vap", "wet", "wnd"),
                          width = "100%"
                        )
                 )
               ),
               fluidRow(align = "center",
                 column(12,
                        dipsaus::actionButtonStyled(
                          ns("extract_ncdf"), span("EXTRAIRE", id=ns("extraireAnimate"), style="font-face:bold;font-family:Georgia;font-size:105%"), class= "",
                          type="primary", icon = icon("cloud-download")
                        )
                 )
               ),
               fluidRow(style = "background-color:gray;",
                        column(12,
                               h3("Détails", style="color:white;font-family:Georgia;"),
                               p(
                                 "[cld]:Amplitude thermique (°C)", br(),
                                 "[dtr]:réquence des jours de gel (jours)", br(),
                                 "[frs]:Evapotranspiration potentielle (mm/mois)", br(),
                                 "[pet]:Couverture Nuageuse (%)", br(),
                                 "[pre]:précipitation (mm/mois)", br(),
                                 "[rhm]:Humidité relative (%)", br(),
                                 "[ssh]:Durée d'ensoleillement (heures)", br(),
                                 "[tmp]:Température moyenne quotidienne moyenne mensuelle (°C)", br(),
                                 "[tmn]:Température minimale journalière moyenne mensuelle (°C)", br(),
                                 "[tmx]:Température maximale quotidienne moyenne mensuelle (°C)", br(),
                                 "[vap]:Pression de vapeur	hectopascals (hPa)", br(),
                                 "[wet]:Fréquence des jours humides (jours)", br(),
                                 "[wnd]:vitesse du vent (m/s)", br(),
                                 style="color:white;font-family:Georgia;font-size:95%;"
                               )
                        )
               )
             )
      ),
      column(1, ""),
      column(7, uiOutput(ns("show_exrtacted_data_ui")))
    )
  )
}

#' cru_data_extract Server Functions
#'
#' @noRd
mod_cru_data_extract_server <- function(id, nc_path_file, stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # EXTRACTION
    cru_extracted<- eventReactive(ignoreNULL = TRUE, ignoreInit = TRUE, input$extract_ncdf, {
      req(nc_path_file(), stations())

      # extraction des données pour toutes les stations
      extracted_data<- tryCatch({
        # bricking
        ncdf_data<- raster::brick(
          nc_path_file(), varname = input$variable
        )
        # extraction
        nc_DataBase <- data.frame(
          raster::extract(
            ncdf_data, stations(), ncol = 2
          )
        )
        ### ajoutons le nom des stations aux tables en utilisant row.names() :
        row.names(nc_DataBase) <- row.names(stations())
        # transposition puis renvoie de la table finale
        t(nc_DataBase)
       },
       error = function(e){
         shinyalert::shinyalert("Erreur !", e$message, type = "error")
         return()
       },
       warning = function(w){
         shinyalert::shinyalert("Avertissement !", w$message, type = "warning")
         return()
       }
      )

      return(extracted_data)

    })

    # Affichage des résultats
    output$show_exrtacted_data_ui<- renderUI({
      req(cru_extracted())
      fluidRow(
        column(12,
               h4("Résumé Statistique de l'Extraction",
                  style = "color:#3474A7;text-align:center;background-color:lightgray")
        ),
        column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("final_cru_data_summary"))))
      )
    })

    output$final_cru_data_summary<- renderPrint({
      req(cru_extracted())
      summary(cru_extracted())
    })

    # return
    return(list(
      cru_extracted_data = reactive({ cru_extracted() }),
      variable_extracted = reactive({ input$variable })
    ))

  })
}

## To be copied in the UI
# mod_cru_data_extract_ui("cru_data_extract_1")

## To be copied in the server
# mod_cru_data_extract_server("cru_data_extract_1")
