#' mean_watershed_time_serie_value UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mean_watershed_time_serie_value_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '
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
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')
    ),

    # boutons d'interpolations
    fluidRow(align = "center",
             # krigeage
             column(3, dipsaus::actionButtonStyled(ns("krigeage"), span("Krigeage", id=ns("KrigeageAnimate")), class= "", type="primary", icon = icon("stream"))),
             # IDW
             column(3, dipsaus::actionButtonStyled(ns("idw"), span("IDW", id=ns("IDWAnimate")), class= "", type="primary", icon = icon("stream"))),
             # Thiessen
             column(3, dipsaus::actionButtonStyled(ns("thiessen"), span("Thiessen", id=ns("ThiessenAnimate")), class= "", type="primary", icon = icon("stream"))),
             # "Thin-Plate Spline"
             column(3, dipsaus::actionButtonStyled(ns("spline"), span("Spline", id=ns("SplineAnimate")), class= "", type="primary", icon = icon("stream"))),
             # Grille d'interpolation
             column(12, tags$hr(style="border-color:gray;")),
             column(12,
                    sliderInput(
                      ns("gridRes"), label="Résolution de la Grille d'Interpolation [mètres]",
                      value=10000, min=500, max=500000, step=100, width = "100%"
                    )
             ),
             column(12, tags$hr(style="border-color:gray;")),
    ),

    h4("RÉSULTATS DE L'INTERPOLATION", style = "color:#3474A7;text-align:center;background-color:lightgray"),

    # résumé statistique
    fluidRow(align = "center",
             # krigeage
             column(3,
                    h4("KRIGEAGE", style = "color:#3474A7;text-align:center;background-color:lightgray"),
                    div(dataTableOutput(ns("krigingResult")), style="font-size:75%")
             ),

             # IDW
             column(3,
                    h4("IDW", style = "color:#3474A7;text-align:center;background-color:lightgray"),
                    div(dataTableOutput(ns("idwResult")), style="font-size:75%")
             ),

             # Thiessen
             column(3,
                    h4("THIESSEN", style = "color:#3474A7;text-align:center;background-color:lightgray"),
                    div(dataTableOutput(ns("thiessenResult")), style="font-size:75%")
             ),

             # Thin-Plate Spline
             column(3,
                    h4("SPLINE", style = "color:#3474A7;text-align:center;background-color:lightgray"),
                    div(dataTableOutput(ns("splineResult")), style="font-size:75%")
             )
    )

    # valeurs NA infos
    # uiOutput(ns("removed_NA_info"))

  )
}

#' mean_watershed_time_serie_value Server Functions
#'
#' @noRd
mod_mean_watershed_time_serie_value_server <- function(id, bassin, interpolationData, stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # avertissement dépassement du nombre de lignes autorisé
    exceed_file_limit <- function(){
      modalDialog(
        paste0(
          "Pour des raison de précaution et d'optimisation, seul l'algorithme IDW est disponible pour les ",
          "fichiers dépassant 200 lignes !!!"
        ),
        title = "Algorithme Non Disponible",
        footer = tagList(
          actionButton(ns("fermer"), "Fermer", class = "btn btn-info")
        )
      )
    }

    # mise en place~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    req(bassin, stations)
    bassin.utm <- reactive({sf::st_transform(bassin, 3857)})
    # gridded cell points in WGS84-4326 proj
    station.in.wgs84<-  reactive(({
      req(stations)
      prec_grid_georef(stations, "+init=epsg:3857")[[1]]
    }))
    # gridded cell points in  UTM 28N
    station.in.utm<-  reactive({
      req(stations)
      prec_grid_georef(stations, "+init=epsg:3857")[[2]]
    })
    # Interpolation data georeferenced in UTM
    # return
    filtered_data<- reactive({
      req(interpolationData)
      interpolationData
      # interpolationData[which(rowMeans(!is.na(interpolationData)) > .01), ]
    })

    # # removed NA info
    # output$removed_NA_info<- renderUI({
    #   req(filtered_data())
    #   # modal dialog
    #   showModal(modalDialog(
    #     tags$h4("INFORMATION SUR LES VALEURS MANQUANTES [NA/NaN]", style="color:#3474A7;family:Georgia;text-align:left;"),
    #     # affichage des résultats
    #     fluidRow(
    #       column(12,
    #              tags$h5(
    #                paste0(
    #                  "Les enregistrements ne contenant aucune donnée dans toutes les stations sont ignorées dans l'interpolation. ",
    #                  "Même si les dates de ces enregistrements apparaissent dans les résultats, elles contiendront des données manquantes !"
    #                ),
    #                style="color:#3474A7;family:Georgia;text-align:left;"
    #              )
    #       ),
    #     ),
    #     footer=tagList(
    #       modalButton('Fermer', icon = icon("power-off"))
    #     ),
    #
    #     size = "m"
    #   ))
    # })

    interpolationData.UTM<- reactive({
      req(filtered_data(), station.in.utm())
      data_cleaning(interpolationData, station.in.utm())
    })
    interpolationData.UTM.wrap<- reactive({
      req(filtered_data(), station.in.utm())
      data_cleaning_wrap(filtered_data(), station.in.utm())
    })
    # Définition du modèle de la grille d'interpolation spatiale
    grid.model<- reactive({
      req(bassin.utm(), input$gridRes)
      grid_def(
        bassin.utm(), input$gridRes, 1000, "+init=epsg:3857"
      )
    })

    #
    # Tester la taille des données d'interpolation
    test_nrow <- reactive({
      req(interpolationData)
      nrow(interpolationData) <= 200
    })

    ## pour stocker les résultats
    result_krige <- reactiveVal()
    result_idw <- reactiveVal()
    result_thiessen <- reactiveVal()
    result_spline <- reactiveVal()

    # interpolation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #* KRIGEAGE----------------------------------------------------------------#
    observeEvent(ignoreNULL = T, ignoreInit = T, input$krigeage, {

      # si le fichier dépasse 200 lignes
      if(!test_nrow()){
        showModal(exceed_file_limit())

        observeEvent(input$fermer, {
          showNotification("Algorithme { KRIGEAGE } désactivé !")
          shinyjs::disable("krigeage")
          removeModal()
        })

      }

      req(bassin.utm(), interpolationData.UTM(), grid.model(), test_nrow())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "KrigeageAnimate", class = "loading dots")
      shinyjs::disable("krigeage")

      bv<- bassin.utm()
      grd<- grid.model()
      interpolationData<- interpolationData.UTM()
      proj<- "+init=epsg:3857"

      # Notification
      id <- showNotification(
        "Interpolation par Krigeage... Peut prendre un certain temps en fonction de la longueur de la serie et de la résolution de la grille...",
        duration = NULL, closeButton = FALSE
      )
      # remove notification
      on.exit(removeNotification(id), add = TRUE)

      process_krigeage<-future::future({
        krige_loop(interpolationData, grd, bv, proj)
      })

      result_krige(process_krigeage)

      # Catch inturrupt (or any other error) and notify user
      process_krigeage <- promises::catch(process_krigeage,
                                function(e){
                                  result_krige(NULL)
                                  print(e$message)
                                  showNotification(e$message)
                                })

      # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
      process_krigeage <- promises::finally(process_krigeage,
                                  function(){
                                    # Button settings
                                    shinyjs::enable("krigeage")
                                    shinyjs::removeClass(id = "KrigeageAnimate", class = "loading dots")
                                  })

      # Affichage des résultats brutes
      output$krigingResult<- renderDataTable(
        result_krige(),
        options = list(
          pageLength=10, searching = FALSE
        )
      )

    })

    #* IDW---------------------------------------------------------------------#
    # processing
    observeEvent(ignoreNULL = T, ignoreInit = T, input$idw,{

      req(bassin.utm(), grid.model(), interpolationData.UTM.wrap())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "IDWAnimate", class = "loading dots")
      shinyjs::disable("idw")

      interpolationData<- interpolationData.UTM.wrap()
      grd<- grid.model()
      bv<- bassin.utm()

      # Notification
      id <- showNotification(
        "Interpolation par IDW ... Peut prendre un certain temps en fonction de la longueur de la serie et de la résolution de la grille...",
        duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      process_idw<- future::future({
        idw_loop(interpolationData, grd, bv,  "+init=epsg:3857")
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
                               shinyjs::removeClass(id = "IDWAnimate", class = "loading dots")
                             })

      # Affichage des résultats brutes
      output$idwResult <- renderDataTable({
        req(result_idw())
        result_idw()},
        options = list(
          pageLength=10, searching = FALSE
        )
      )

      # Return something other than the promise so shiny remains responsive
      NULL

    })

    #* THISSEN---------------------------------------------------------------------#
    #* processing
    observeEvent(ignoreNULL = T, ignoreInit = T, input$thiessen,{

      if(!test_nrow()){# si le fichier dépasse 200 lignes
        showModal(exceed_file_limit())

        observeEvent(input$fermer, {
          showNotification("Algorithme { THIESSEN } désactivé !")
          shinyjs::disable("thiessen")
          removeModal()
        })

      }

      req(bassin, stations, filtered_data(), test_nrow())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "ThiessenAnimate", class = "loading dots")
      shinyjs::disable("thiessen")

      bv<- bassin
      stations<- stations
      interpolationData<- filtered_data()
      proj<-3857

      # Notification
      id <- showNotification(
        "Interpolation par Polygone de Thiessen... Peut prendre un certain temps en fonction de la longueur de la serie et de la résolution de la grille...",
        duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      process_thiessen<- future::future({
        thiessen.loop(interpolationData, stations, bv, proj)
      })

      result_thiessen(process_thiessen)

      # Catch inturrupt (or any other error) and notify user
      process_thiessen <- promises::catch(process_thiessen,
                                function(e){
                                  result_thiessen(NULL)
                                  print(e$message)
                                  showNotification(e$message)
                                })

      # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
      process_thiessen <- promises::finally(process_thiessen,
                                  function(){
                                    # Button settings
                                    shinyjs::enable("thiessen")
                                    shinyjs::removeClass(id = "ThiessenAnimate", class = "loading dots")
                                  })

      # # Affichage des résultats brutes
      output$thiessenResult <- renderDataTable({
        req(result_thiessen())
        result_thiessen()},
        options = list(
          pageLength=10, searching = FALSE
        )
      )

      # Return something other than the promise so shiny remains responsive
      NULL

    })

    #* Thin-Plane Spline-------------------------------------------------------------#
    #* processing
    observeEvent(ignoreNULL = T, ignoreInit = T, input$spline, {

      if(!test_nrow()){# si le fichier dépasse 200 lignes
        showModal(exceed_file_limit())

        observeEvent(input$fermer, {
          showNotification("Algorithme { SPLINE } désactivé !")
          shinyjs::disable("spline")
          removeModal()
        })

      }

      req(interpolationData.UTM.wrap(), bassin.utm(), grid.model(), test_nrow())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "SplineAnimate", class = "loading dots")
      shinyjs::disable("spline")

      bv<- bassin.utm()
      grd<- grid.model()
      proj<- 3857
      interpolationData<- interpolationData.UTM.wrap()

      # Notification
      id <- showNotification(
        "Interpolation par Thin-Plate Spline... Peut prendre un certain temps en fonction de la longueur de la serie et de la résolution de la grille...",
        duration = NULL, closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # tourneur
      # waiter <- waiter::Waiter$new()
      # waiter$show()
      # on.exit(waiter$hide())

      process_spline<- future::future({
        spline.loop(interpolationData, bv, proj)
      })

      result_spline(process_spline)

      # Catch inturrupt (or any other error) and notify user
      process_spline <- promises::catch(process_spline,
                              function(e){
                                result_spline(NULL)
                                print(e$message)
                                showNotification(e$message)
                              })

      # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
      process_spline <- promises::finally(process_spline,
                                function(){
                                  # Button settings
                                  shinyjs::enable("spline")
                                  shinyjs::removeClass(id = "SplineAnimate", class = "loading dots")
                                })

      # Affichage des résultats brutes
      output$splineResult <- renderDataTable({
        req(result_spline())
        result_spline()},
        options = list(
          pageLength=10, searching = FALSE
        )
      )

      # Return something other than the promise so shiny remains responsive
      NULL

    })

    # return
    return(
      list(
        # Résultats série chronologique
        kriging_output_df = reactive({result_krige()}),
        idw_output_df = reactive({result_idw()}),
        spline_output_df = reactive({result_spline()}),
        thiessen_output_df = reactive({result_thiessen()})
      )
    )

  })
}

## To be copied in the UI
# mod_mean_watershed_time_serie_value_ui("mean_watershed_time_serie_value_1")

## To be copied in the server
# mod_mean_watershed_time_serie_value_server("mean_watershed_time_serie_value_1")
