#' DEM_Preparation_4_Hydrology_Analyses UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DEM_Preparation_4_Hydrology_Analyses_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(type="text/css", '

            .shiny-input-container {
                color: gray;
            }
            .textBoxed {
                display:block;
                padding:1.5px;
                margin:0 0 0px;
                margin-top:0px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:0px;
                font-family:georgia;
            }
            .loading {
                display: inline-block;
                overflow: hidden;
                height: 2.3em;
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

    fluidRow(
      column(4,
             shinyFiles::shinySaveButton(
               id = ns("save_wbt_fill_single_cell_pits"),
               label=span("SingleCellPitsFill [step1]", id=ns("wbt_breach_depressions_least_cost_animate"), style = "font-size:105%;"),
               filetype = list(picture ="tif"),title = "Sélectionner Le Dossier de Destination !",
               multiple = FALSE, icon = icon("fill"), buttonType = "info", width = "100%"
             )
      ),
      column(4,
             shinyFiles::shinySaveButton(
               id = ns("wbt_breach_depressions_least_cost"),
               label=span("DepressionBreach [step2]", id=ns("wbt_breach_depressions_least_cost_animate"), style = "font-size:105%;"),
               filetype = list(picture ="tif"), title = "Sélectionner Le Dossier de Destination !",
               multiple = FALSE, icon = icon("red-river"), buttonType = "info", width = "100%"
             )
      ),
      column(4,
             shinyFiles::shinySaveButton(
               id = ns("wbt_fill_depressions_wang_and_liu"),
               label=span("DepressionBreach [step3]", id=ns("wbt_fill_depressions_wang_and_liu_animate"), style = "font-size:105%;"),
               filetype = list(picture ="tif"), title = "Sélectionner Le Dossier de Destination !",
               multiple = FALSE, icon = icon("table-cells"), buttonType = "info", width = "100%"
             )
      ),
      column(12, tags$hr(style="border-color:gray;")),
      column(12, uiOutput(ns("plotting_result")))
    )
  )
}

#' DEM_Preparation_4_Hydrology_Analyses Server Functions
#'
#' @noRd
mod_DEM_Preparation_4_Hydrology_Analyses_server <- function(
    id, workingDirectory, mnt_path, outlets_layer, breaching_max_dist
  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactive value for data transfert
    # filledDEM
    filledDEM<- reactiveVal()

    # breach dem
    breachedDEM<- reactiveVal()

    # fill dem given
    filledBreachedDEM<- reactiveVal()

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 1 : Remplissage des Dépressions Unicellulaires
    observeEvent(input$save_wbt_fill_single_cell_pits, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "save_wbt_fill_single_cell_pits", roots = volumes_default_dir(), session = session,
                                  restrictions = base::system.file(package = "base"))

      filledDemPath<- reactive({
        req(volumes_default_dir(), volumes_default_dir(), file.exists(shinyFiles::parseSavePath(volumes_default_dir(), input$save_wbt_fill_single_cell_pits)$datapath))
        shinyFiles::parseSavePath(volumes_default_dir(), input$save_wbt_fill_single_cell_pits)$datapath
      })

      # filling cells
      filled_dem_process_elapsed_time<- whitebox::wbt_fill_single_cell_pits(dem = mnt_path, output = filledDemPath())

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(filledDemPath()))
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("mnt_preparation_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(filledDemPath()))
        tags$h4("Opération 1 : Remplissage des dépressions unicellulaires", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(filled_dem_process_elapsed_time)
        filled_dem_process_elapsed_time
      })

      # plot
      output$mnt_preparation_results_viz <- renderPlot({
        req(file.exists(filledDemPath()), outlets_layer)

        # load filled DEM
        filled_dem_raster<- raster::raster(filledDemPath())

        crs(filled_dem_raster)<- "+proj=longlat +datum=WGS84"
        filled_dem_raster[filled_dem_raster < 0] <- 0

        # send result
        filledDEM(filledDemPath())

        # plot
        tmap::tmap_mode("plot")
        tmap::tm_shape(filled_dem_raster)+
          tmap::tm_raster(style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = "Altitude")+
          tmap::tm_scale_bar()+
          tmap::tm_shape(outlets_layer)+
          tmap::tm_dots(col = "black", size = .5)
      })

    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 2 : Creuser des chenaux dans les dépressions
    observeEvent(input$wbt_breach_depressions_least_cost, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(filledDEM(), dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "wbt_breach_depressions_least_cost", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      breachedDemPath<- reactive({
        req(volumes_default_dir(), filledDEM(), file.exists(shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_breach_depressions_least_cost)$datapath))
        shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_breach_depressions_least_cost)$datapath
      })

      # filling cells
      req(filledDEM(), breaching_max_dist())
      breached_dem_process_elapsed_time<- whitebox::wbt_breach_depressions_least_cost(
        dem = filledDEM(), output = breachedDemPath(), dist = breaching_max_dist(), fill = TRUE
      )

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(breachedDemPath()))
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("mnt_preparation_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(breachedDemPath()))
        tags$h4("Opération 2 : Creuser Des Chenaux ou Connexions dans les Dépressions", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(breached_dem_process_elapsed_time)
        breached_dem_process_elapsed_time
      })

      # plot
      output$mnt_preparation_results_viz <- renderPlot({
        req(file.exists(breachedDemPath()), outlets_layer)

        # load filled DEM
        breached_dem_raster<- raster::raster(breachedDemPath())

        crs(breached_dem_raster)<- "+proj=longlat +datum=WGS84"
        breached_dem_raster[breached_dem_raster < 0] <- 0

        # send result
        breachedDEM(breachedDemPath())

        # plot
        tmap::tmap_mode("plot")
        tmap::tm_shape(breached_dem_raster)+
          tmap::tm_raster(style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = "Altitude")+
          tmap::tm_scale_bar()+
          tmap::tm_shape(outlets_layer)+
          tmap::tm_dots(col = "black", size = .5)
      })

    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| Step 3 : Remplir toutes les dépressions du DEM à l'aide de la méthode de Wang & Liu (2006).
    observeEvent(input$wbt_fill_depressions_wang_and_liu, {

      # chemin du repertoire courant
      volumes_default_dir <- reactive({
        req(breachedDEM(), dir.exists(workingDirectory))
        c(Home = workingDirectory, "R Installation" = base::R.home(), shinyFiles::getVolumes()())
      })

      # file path
      shinyFiles::shinyFileSave(input, "wbt_fill_depressions_wang_and_liu", roots = volumes_default_dir(), session = session,
                                restrictions = base::system.file(package = "base"))

      filledBreachedDemPath<- reactive({
        req(breachedDEM(), file.exists(shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_fill_depressions_wang_and_liu)$datapath))
        shinyFiles::parseSavePath(volumes_default_dir(), input$wbt_fill_depressions_wang_and_liu)$datapath
      })

      # filling cells
      req(breachedDEM(), filledBreachedDemPath())
      filled_breached_dem_process_elapsed_time<- whitebox::wbt_fill_depressions_wang_and_liu(
        dem = breachedDEM(), output = filledBreachedDemPath()
      )

      # --------------------------------Render Result
      output$plotting_result<- renderUI({
        req(file.exists(filledBreachedDemPath()))
        fluidRow(
          column(12, uiOutput(ns("process_verbose_title"))),
          column(12, verbatimTextOutput(ns("process_verbose"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("mnt_preparation_results_viz"))))
        )
      })

      # operation description
      output$process_verbose_title<-renderUI({
        req(file.exists(filledBreachedDemPath()))
        tags$h4("Opération 3 : Remplir toutes les dépressions du DEM (méthode de Wang & Liu (2006))", style="color:#3474A7;background-color:lightgray;family:Georgia;text-align:left;")
      })

      # elapsed time
      output$process_verbose<- renderPrint({
        req(filled_breached_dem_process_elapsed_time)
        filled_breached_dem_process_elapsed_time
      })

      # plot
      output$mnt_preparation_results_viz <- renderPlot({
        req(file.exists(filledBreachedDemPath()), outlets_layer)

        # load filled DEM
        filled_breached_dem_raster<- raster::raster(filledBreachedDemPath())

        crs(filled_breached_dem_raster)<- "+proj=longlat +datum=WGS84"
        filled_breached_dem_raster[filled_breached_dem_raster < 0] <- 0

        # send result
        filledBreachedDEM(filledBreachedDemPath())

        # plot
        tmap::tmap_mode("plot")
        tmap::tm_shape(filled_breached_dem_raster)+
          tmap::tm_raster(style = "cont", palette = "-Spectral", legend.show = TRUE, midpoint = NA, title = "Altitude")+
          tmap::tm_scale_bar()+
          tmap::tm_shape(outlets_layer)+
          tmap::tm_dots(col = "black", size = .5)
      })

    })

    # #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # return
    return(
      list(
        path_to_filled_DEM = reactive({ filledDEM() }),
        path_to_breacheded_DEM = reactive({ breachedDEM() }),
        path_to_filled_breached_DEM = reactive({ filledBreachedDEM() })
      )
    )

  })
}

## To be copied in the UI
# mod_DEM_Preparation_4_Hydrology_Analyses_ui("DEM_Preparation_4_Hydrology_Analyses_1")

## To be copied in the server
# mod_DEM_Preparation_4_Hydrology_Analyses_server("DEM_Preparation_4_Hydrology_Analyses_1")
