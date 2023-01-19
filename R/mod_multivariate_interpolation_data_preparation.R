#' multivariate_interpolation_data_preparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multivariate_interpolation_data_preparation_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '

            .shiny-input-container {
                color: gray;
            }

            .loading {
                display: inline-block;
                overflow: hidden;
                height: 1.3em;
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

    br(),

    fluidRow(align = "center",
             column(6, dipsaus::actionButtonStyled(ns("idw"), span("Interpolation", id=ns("idwAnimate")), class= "", type="primary")),
             column(6, dipsaus::actionButtonStyled(ns("recoder"), span("Recoder", id=ns("recoderAnimate")), class= "", type="primary"))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("origVal"), label = "Valeurs Originales des Variables", value = "",
                      placeholder = "i.e., old_variable1;old_variable2;old_variable3;etc.",
                      width = "100%"
                    )
             ),
             column(6,
                    textInput(
                      ns("newVal"), label = "Nouvelles Valeurs de Variables", value ="",
                      placeholder = "i.e., new_variable1;new_variable2;new_variable3;etc.",
                      width = "100%"
                    )
             )
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    fluidRow(align = "left",
             column(12, h4(
               "Résultats de l'Interpolation || Résumé Statistique",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(12, dataTableOutput(ns("summaryResult")))
    ),

    fluidRow(align = "left",
             column(12, h4(
               "Résultats de l'Interpolation || ISOLIGNES",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(12, verbatimTextOutput(ns("isolines")))
    )
  )
}

#' multivariate_interpolation_data_preparation Server Functions
#'
#' @noRd
mod_multivariate_interpolation_data_preparation_server <- function(id, bassin, data, stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # interpolation

    # Le bassin en UTM
    bassin.utm<- reactive({
      shiny::req(bassin)
      sf::st_transform(bassin, 3857)
    })

    # gridded cell points in WGS84-4326 proj
    station.in.wgs84<-  reactive({
      shiny::req(data)
      prec_grid_georef(stations, "+init=epsg:4326")[[1]]
    })
    # gridded cell points in  UTM 28N
    # station.in.utm<-  reactive({
    #   shiny::req(data)
    #   prec.grid.georef(data[,1:3], "+init=epsg:4328")[[2]]
    # })
    # Interpolation data georeferenced in UTM
    interpolationData.UTM<- reactive({
      shiny::req(data, station.in.wgs84())
      data_cleaning_wrap_multivariate(data, station.in.wgs84())
    })

    # Définition du modèle de la grille d'interpolation spatiale
    grid.model<- reactive({
      shiny::req(bassin.utm())
      grid_def(
        bassin,  .01, 1, "+init=epsg:4326"
      )
    })

    #* IDW---------------------------------------------------------------------#
    # initatizing result
    result_idw <- reactiveVal()

    # processing
    observeEvent(ignoreInit = T, ignoreNULL = T, input$idw, {
      shiny::req(bassin.utm(), grid.model(), interpolationData.UTM())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "idwAnimate", class = "loading dots")
      shinyjs::disable("idw")

      # Notification
      id <- showNotification(
        "Interpolation par IDW ... Peut prendre un certain temps en fonction du nombre de variables...",
        duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      interpolationData<- interpolationData.UTM()
      grd<- grid.model()
      bv<- bassin.utm()
      # on.exit(waiter$hide())

      process_idw<- future::future({
        idw_multivariate(interpolationData, grd, bv, 4326)
      })

      result_idw(process_idw)

      # Catch inturrupt (or any other error) and notify user
      process_idw <- promises::catch(process_idw,
                           function(e){
                             result_idw(NULL)
                             print(e$message)
                             showNotification(e$message)
                           })

      # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
      process_idw <- promises::finally(process_idw,
                             function(){
                               # Button settings
                               shinyjs::enable("idw")
                               shinyjs::removeClass(id = "idwAnimate", class = "loading dots")
                             })

      output$summaryResult<- renderDataTable({
        shiny::req(result_idw())
        future::value(result_idw())[[1]]  %>%
          dplyr::group_by(variable)  %>%
          dplyr::summarise(
            Min. = min(valeur, na.rm = T), Quart1 = quantile(valeur, .25),
            Médianne = median(valeur), Quart3 = quantile(valeur, .75),
            Moyenne = mean(valeur), Max = max(valeur),
            "Ecart type" = sd(valeur)
          ) %>%
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}"))
      })

      output$isolines<- renderPrint({
        shiny::req(result_idw())
        future::value(result_idw())[[2]]
      })

      # Return something other than the promise so shiny remains responsive
      NULL

    })

    # recodage
    observeEvent(ignoreInit = T, ignoreNULL = T, input$recoder, {

      shiny::req(result_idw())

      #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
      # gestion des entrées vecteurs
      extract_variables_entries<-  function(text){
        # text<-  gsub(" ", "", text)
        split<-  strsplit(text, ";", fixed = FALSE)[[1]]
        split
      }

      # ------------------------------------------------------------------------------------------------------#
      # Validation des entrées vecteurs
      if(input$origVal!="" & input$newVal!=""){
        original_values<- extract_variables_entries(input$origVal)
        new_values<- extract_variables_entries(input$newVal)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("origVal")
          shinyFeedback::hideFeedback("newVal")

          donnees<- future::value(result_idw())
          dataPlot<- donnees[[1]]
          dataContours<- donnees[[2]]

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% donnees[[1]]$variable)
          shinyFeedback::feedbackWarning("origVal", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Valeurs Originales des Variables } sont biens présents dans la table résumant ",
                "résumant les résultats de l'interpolation. Cette exigence est nécessaire pour renommer ",
                "les facets (titre des grilles de carte) dans ggplot !"
              )
            )
          }

          shiny::req(testVar)
          dataPlot$variable<-  plyr::mapvalues(dataPlot$variable, original_values,new_values)
          dataContours$variable<-  plyr::mapvalues(dataContours$variable, original_values, new_values)
          result_idw(list(dataPlot, dataContours))

          output$summaryResult<- renderDataTable({
            shiny::req(testVar, result_idw())
            result_idw()[[1]]  %>%  dplyr::group_by(variable)  %>%
              dplyr::summarise(
                Min. = min(valeur, na.rm = T), Quart1 = quantile(valeur, .25),
                Médianne = median(valeur), Quart3 = quantile(valeur, .75),
                Moyenne = mean(valeur), Max = max(valeur),
                "Ecart type" = sd(valeur)
              ) %>%
              dplyr::mutate(across(where(is.numeric), ~round(., 2), .names = "{.col}"))
          })

          output$isolines<- renderPrint({
            shiny::req(testVar, result_idw())
            result_idw()[[2]]
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("origVal", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("newVal", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer des vecteurs de mêmes longueurs dans les deux champs !")
          )

        }
      }

    })

    # return
    return(list(cleaned_data_for_map = reactive({ result_idw() })))

  })
}

## To be copied in the UI
# mod_multivariate_interpolation_data_preparation_ui("multivariate_interpolation_data_preparation_1")

## To be copied in the server
# mod_multivariate_interpolation_data_preparation_server("multivariate_interpolation_data_preparation_1")
