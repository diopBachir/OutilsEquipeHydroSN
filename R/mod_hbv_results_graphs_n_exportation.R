#' hbv_results_graphs_n_exportation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hbv_results_graphs_n_exportation_ui <- function(id){
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

    fluidRow(align="center",
             column(12, h4("VISUALISATION ET EXPORTATION DES RESULTATS",
                           style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
             )),
             column(4,
                    actionButton(
                      ns("cross_validation_plot1"), icon = icon("poll"), class = "btn-info", width = "100%",
                      label=span("Cross-Validation [Sample 1]", id=ns("calage_validation_animate1"),
                                 style = "font-size:105%;")
                    )
             ),
             column(4,
                    actionButton(
                      ns("cross_validation_plot2"), label=span("Cross-Validation [Sampling 2]", id=ns("simulation_animate"),
                                                               style = "font-size:105%;"),
                      icon = icon("poll"), class = "btn-info", width = "105%"
                    )
             ),
             column(4,
                    actionButton(
                      ns("simulation_plot"), label=span("Simulation [Overall Serie]", id=ns("simulation_animate"),
                                                        style = "font-size:110%;"),
                      icon = icon("poll"), class = "btn-info", width = "100%"
                    )
             )
    ),

    tags$hr(style="border-color:blue;"),

    fluidRow(align="center",
             column(12, h4("RESULTATS TABULAIRES DES DIFFERENTES SIMULATIONS",
                           style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
             )),

             column(4,
                    h5("Cross-Validation [Sampling 1]",
                       style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;")
             ),
             column(4,
                    h5("Cross-Validation [Sampling 2]",
                       style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;")
             ),
             column(4,
                    h5("Simulation [Overall Serie]",
                       style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;")
             ),

             column(2,
                    actionButton(
                      ns("calage1"), icon = icon("table"), class = "btn-info", width = "100%",
                      label=span("Calage 1", id=ns("calage1_animate"),
                                 style = "font-size:105%;")
                    )
             ),
             column(2,
                    actionButton(
                      ns("validation1"), label=span("Valid... 1", id=ns("validation1_animate"),
                                                    style = "font-size:105%;"),
                      icon = icon("table"), class = "btn-info", width = "100%"
                    )
             ),
             column(2,
                    actionButton(
                      ns("calage2"), icon = icon("table"), class = "btn-info", width = "100%",
                      label=span("Calage 2", id=ns("calage2_animate"),
                                 style = "font-size:105%;")
                    )
             ),
             column(2,
                    actionButton(
                      ns("validation2"), label=span("Valid... 2", id=ns("validation2_animate"),
                                                    style = "font-size:105%;"),
                      icon = icon("table"), class = "btn-info", width = "100%"
                    )
             ),
             column(4,
                    actionButton(
                      ns("simulation2"), label=span("Simulation [Overall Serie]", id=ns("simulation_animate"),
                                                    style = "font-size:105%;"),
                      icon = icon("table"), class = "btn-info", width = "100%"
                    )
             ),

             tags$hr(style="border-color:blue;"),

             column(12, uiOutput(ns("tabular_out"))),

             column(12, div(DT::dataTableOutput(ns("model_out")), style = "font-size:65%"))
    )
  )
}

#' hbv_results_graphs_n_exportation Server Functions
#'
#' @noRd
mod_hbv_results_graphs_n_exportation_server <- function(
    id,
    # cross validation [1] results
    cross__validation_period_1_1, cross__validation_period_1_2, calibration_1, validation_1,
    # cross validation [1] results
    cross__validation_period_2_1, cross__validation_period_2_2, calibration_2, validation_2,
    # simulation
    simulation__period, simulation__output_result
  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # CROSS VALIDATION 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$cross_validation_plot1, {
      req(cross__validation_period_1_1(), cross__validation_period_1_2(), calibration_1(), validation_1())

      # modal dialog
      showModal(modalDialog(
        tags$h4("CALAGE/VALIDATION CROISEE DU MODEL HBV PAR SIMULATION MONTE CARLO  [RUN1]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        tags$hr(style="border-color:gray;"),
        uiOutput(ns("periode_plot")),
        tags$hr(style="border-color:gray;"),
        shinycssloaders::withSpinner(plotOutput(ns("plot_cross_validation"))),
        footer=tagList(
          fluidRow(align = "center",
                   column(2, actionButton(
                     ns("calage_plot"), label=span("Calib...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")
                   ),
                   column(2, actionButton(
                     ns("validation_plot"), label=span("Valid...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")),
                   column(2, downloadButton(ns("toExcel"), label="EXCEL", icon = icon("file-excel"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toJPEG"), label="JPEG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toSVG"), label="SVG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      #période
      output$periode_plot<- renderUI({
        req(cross__validation_period_1_1())
        div(
          paste0(
            "Résultats De La Simulation Sur La Période De Calage :::: ",
            "[Début : ", as.character(cross__validation_period_1_1()[1]),
            "  ----  Fin : ", as.character(cross__validation_period_1_1()[2]), "]", sep = ""
          ), style="color:#3474A7;family:Georgia;text-align:left;"
        )
      })

      # plot
      #* data
      calibration_1_filtered<-reactive({
        calibration_1() %>%
          dplyr::select(DatesR=date, Precip=PmmObs, Qsim=q, Qobs) %>%
          dplyr::mutate(DatesR=as.POSIXlt(lubridate::ymd(DatesR)))
      })
      calibration_1_filtered_lst<- reactive({
        temp<- as.list(calibration_1_filtered())
        class(temp)<- c("GR", "daily")
        temp
      })

      output$plot_cross_validation<- renderPlot({
        req(calibration_1_filtered_lst())
        plot_OutputsModel_wrap_hbv(
          calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
          cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
        )
      })

      #----------------------------------------------------------------------------------------------#
      ###  Exportation
      #* jpeg
      output$toJPEG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = calibration_1() %>% dplyr::rename(Qsim = q) %>%
                dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(calibration_1()$q, calibration_1()$Qobs),
                  hydroGOF::NSE(calibration_1()$q, calibration_1()$Qobs),
                  hydroGOF::rmse(calibration_1()$q, calibration_1()$Qobs),
                  hydroGOF::pbias(calibration_1()$q, calibration_1()$Qobs),
                  hydroGOF::rPearson(calibration_1()$q, calibration_1()$Qobs)**2
                )
              )
            ),
            path = file
          )
        }
      )

      # OBSERVE EVENTS
      # calibration period plot button
      observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage_plot, {
        req(calibration_1(), calibration_1_filtered_lst(), cross__validation_period_1_1())

        #période
        output$periode_plot<- renderUI({
          req(cross__validation_period_1_1())
          div(
            paste0(
              "Résultats De La Simulation Sur La Période De Calage :::: ",
              "[Début : ", as.character(cross__validation_period_1_1()[1]),
              "  ----  Fin : ", as.character(cross__validation_period_1_1()[2]), "]", sep = ""
            ), style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        output$plot_cross_validation<- renderPlot({
          req(calibration_1_filtered_lst())
          plot_OutputsModel_wrap_hbv(
            calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              calibration_1_filtered_lst(), Qobs =  calibration_1_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = calibration_1() %>% dplyr::rename(Qsim = q) %>%
                  dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                  dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(calibration_1()$q, calibration_1()$Qobs),
                    hydroGOF::NSE(calibration_1()$q, calibration_1()$Qobs),
                    hydroGOF::rmse(calibration_1()$q, calibration_1()$Qobs),
                    hydroGOF::pbias(calibration_1()$q, calibration_1()$Qobs),
                    hydroGOF::rPearson(calibration_1()$q, calibration_1()$Qobs)**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

      # validation period plot button
      observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$validation_plot, {
        req(validation_1())

        #période
        output$periode_plot<- renderUI({
          req(cross__validation_period_1_2())
          div(
            paste0(
              "Résultats De La Simulation Sur La Période De Validation :::: ",
              "[Début : ", as.character(cross__validation_period_1_2()[1]),
              "  ----  Fin : ", as.character(cross__validation_period_1_2()[2]), "]", sep = ""
            ), style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        validation_1_filtered<-reactive({
          validation_1() %>%
            dplyr::select(DatesR=date, Precip=PmmObs, Qsim=q, Qobs) %>%
            dplyr::mutate(DatesR=as.POSIXlt(lubridate::ymd(DatesR)))
        })
        validation_1_filtered_lst<- reactive({
          temp<- as.list(validation_1_filtered())
          class(temp)<- c("GR", "daily")
          temp
        })

        output$plot_cross_validation<- renderPlot({
          req(validation_1_filtered_lst())
          plot_OutputsModel_wrap_hbv(
            validation_1_filtered_lst(), Qobs =  validation_1_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              validation_1_filtered_lst(), Qobs =  validation_1_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              validation_1_filtered_lst(), Qobs =  validation_1_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = validation_1() %>% dplyr::rename(Qsim = q) %>%
                  dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                  dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(validation_1()$q, validation_1()$Qobs),
                    hydroGOF::NSE(validation_1()$q, validation_1()$Qobs),
                    hydroGOF::rmse(validation_1()$q, validation_1()$Qobs),
                    hydroGOF::pbias(validation_1()$q, validation_1()$Qobs),
                    hydroGOF::rPearson(validation_1()$q, validation_1()$Qobs)**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

    })

    # CROSS VALIDATION 2
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$cross_validation_plot2, {
      req(cross__validation_period_2_1(), cross__validation_period_2_2(), calibration_2(), validation_2())

      # modal dialog
      showModal(modalDialog(
        tags$h4("CALAGE/VALIDATION CROISEE DU MODEL HBV PAR SIMULATION MONTE CARLO [RUN2]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        tags$hr(style="border-color:gray;"),
        uiOutput(ns("periode_plot")),
        tags$hr(style="border-color:gray;"),
        shinycssloaders::withSpinner(plotOutput(ns("plot_cross_validation"))),
        footer=tagList(
          fluidRow(align = "center",
                   column(2, actionButton(
                     ns("calage_plot"), label=span("Calib...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")
                   ),
                   column(2, actionButton(
                     ns("validation_plot"), label=span("Valid...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")),
                   column(2, downloadButton(ns("toExcel"), label="EXCEL", icon = icon("file-excel"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toJPEG"), label="JPEG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toSVG"), label="SVG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      #période
      output$periode_plot<- renderUI({
        req(cross__validation_period_2_1())
        div(
          paste0(
            "Résultats De La Simulation Sur La Période De Calage :::: ",
            "[Début : ", as.character(cross__validation_period_2_1()[1]),
            "  ----  Fin : ", as.character(cross__validation_period_2_1()[2]), "]", sep = ""
          ), style="color:#3474A7;family:Georgia;text-align:left;"
        )
      })

      # plot
      #* data
      calibration_2_filtered<-reactive({
        calibration_2() %>%
          dplyr::select(DatesR=date, Precip=PmmObs, Qsim=q, Qobs) %>%
          dplyr::mutate(DatesR=as.POSIXlt(lubridate::ymd(DatesR)))
      })
      calibration_2_filtered_lst<- reactive({
        temp<- as.list(calibration_2_filtered())
        class(temp)<- c("GR", "daily")
        temp
      })

      output$plot_cross_validation<- renderPlot({
        req(calibration_2_filtered_lst())
        plot_OutputsModel_wrap_hbv(
          calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
          cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
        )
      })

      #----------------------------------------------------------------------------------------------#
      ###  Exportation
      #* jpeg
      output$toJPEG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = calibration_2() %>% dplyr::rename(Qsim = q) %>%
                dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(calibration_2()$q, calibration_2()$Qobs),
                  hydroGOF::NSE(calibration_2()$q, calibration_2()$Qobs),
                  hydroGOF::rmse(calibration_2()$q, calibration_2()$Qobs),
                  hydroGOF::pbias(calibration_2()$q, calibration_2()$Qobs),
                  hydroGOF::rPearson(calibration_2()$q, calibration_2()$Qobs)**2
                )
              )
            ),
            path = file
          )
        }
      )

      # OBSERVE EVENTS
      # calibration period plot button
      observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage_plot, {
        req(calibration_2(), calibration_2_filtered_lst(), cross__validation_period_2_1())

        #période
        output$periode_plot<- renderUI({
          req(cross__validation_period_2_1())
          div(
            paste0(
              "Résultats De La Simulation Sur La Période De Calage :::: ",
              "[Début : ", as.character(cross__validation_period_2_1()[1]),
              "  ----  Fin : ", as.character(cross__validation_period_2_1()[2]), "]", sep = ""
            ), style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        output$plot_cross_validation<- renderPlot({
          req(calibration_2_filtered_lst())
          plot_OutputsModel_wrap_hbv(
            calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              calibration_2_filtered_lst(), Qobs =  calibration_2_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = calibration_2() %>% dplyr::rename(Qsim = q) %>%
                  dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                  dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(calibration_2()$q, calibration_2()$Qobs),
                    hydroGOF::NSE(calibration_2()$q, calibration_2()$Qobs),
                    hydroGOF::rmse(calibration_2()$q, calibration_2()$Qobs),
                    hydroGOF::pbias(calibration_2()$q, calibration_2()$Qobs),
                    hydroGOF::rPearson(calibration_2()$q, calibration_2()$Qobs)**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

      # validation period plot button
      observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$validation_plot, {
        req(validation_2())

        #période
        output$periode_plot<- renderUI({
          req(cross__validation_period_2_2())
          div(
            paste0(
              "Résultats De La Simulation Sur La Période De Validation :::: ",
              "[Début : ", as.character(cross__validation_period_2_2()[1]),
              "  ----  Fin : ", as.character(cross__validation_period_2_2()[2]), "]", sep = ""
            ), style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        validation_2_filtered<-reactive({
          validation_2() %>%
            dplyr::select(DatesR=date, Precip=PmmObs, Qsim=q, Qobs) %>%
            dplyr::mutate(DatesR=as.POSIXlt(lubridate::ymd(DatesR)))
        })
        validation_2_filtered_lst<- reactive({
          temp<- as.list(validation_2_filtered())
          class(temp)<- c("GR", "daily")
          temp
        })

        output$plot_cross_validation<- renderPlot({
          req(validation_2_filtered_lst())
          plot_OutputsModel_wrap_hbv(
            validation_2_filtered_lst(), Qobs =  validation_2_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              validation_2_filtered_lst(), Qobs =  validation_2_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap_hbv(
              validation_2_filtered_lst(), Qobs =  validation_2_filtered_lst()$Qobs,
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--HBV--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = validation_2() %>% dplyr::rename(Qsim = q) %>%
                  dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                  dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(validation_2()$q, validation_2()$Qobs),
                    hydroGOF::NSE(validation_2()$q, validation_2()$Qobs),
                    hydroGOF::rmse(validation_2()$q, validation_2()$Qobs),
                    hydroGOF::pbias(validation_2()$q, validation_2()$Qobs),
                    hydroGOF::rPearson(validation_2()$q, validation_2()$Qobs)**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

    })

    # SIMULATION
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$simulation_plot, {
      req(simulation__period(), simulation__output_result())

      # modal dialog
      showModal(modalDialog(
        tags$h4("RESULTATS DE LA SIMULATION SUR TOUTE LA PERIODE", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot_simulation"))),
          column(12, plotOutput(ns("plot_simulation")))
        ),
        footer=tagList(
          fluidRow(align = "center",
                   column(3, downloadButton(ns("toExcel3"), label="EXCEL", icon = icon("file-excel"), class= "butt", width = "100%")),
                   column(3, downloadButton(ns("toJPEG3"), label="JPEG", icon = icon("download"), class= "butt", width = "100%")),
                   column(3, downloadButton(ns("toSVG3"), label="SVG", icon = icon("download"), class= "butt", width = "100%")),
                   column(3, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      # période
      output$periode_plot_simulation<- renderUI({
        req(simulation__period())
        div(
          paste0(
            "Résultats De La Simulation Sur La Toute La Période :::: ",
            "[Début : ", as.character(simulation__period()[1]),
            "  ----  Fin : ", as.character(simulation__period()[2]), "]", sep = ""
          ), style="color:#3474A7;family:Georgia;text-align:left;"
        )
      })

      # plot
      #* data
      simulation_filtered<-reactive({
        simulation__output_result() %>%
          dplyr::select(DatesR=date, Precip=PmmObs, Qsim=q, Qobs) %>%
          dplyr::mutate(DatesR=as.POSIXlt(lubridate::ymd(DatesR)))
      })
      simulation_filtered_lst<- reactive({
        temp<- as.list(simulation_filtered())
        class(temp)<- c("GR", "daily")
        temp
      })

      output$plot_simulation <- renderPlot({
        tryCatch({
          plot_OutputsModel_wrap_hbv(
            simulation_filtered_lst(), Qobs =  simulation_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )}, error = function(e) {
            shinyalert("Error !", e$message, type = "error")
            return()
          }, warning = function(w) {
            shinyalert("Warning !", w$message, type = "warning")
            return(
              plot_OutputsModel_wrap_hbv(
                simulation_filtered_lst(), Qobs =  simulation_filtered_lst()$Qobs,
                cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
              )
            )
          }
        )
      })

      #----------------------------------------------------------------------------------------------#
      ###  Exportation
      #* jpeg
      output$toJPEG3 <-  downloadHandler(
        filename = function() {
          paste("Simulation-HBV-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            simulation_filtered_lst(), Qobs =  simulation_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG3 <-  downloadHandler(
        filename = function() {
          paste("Simulation-HBV-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap_hbv(
            simulation_filtered_lst(), Qobs =  simulation_filtered_lst()$Qobs,
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel3 <-  downloadHandler(
        filename = function() {
          paste("Simulation-HBV-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = simulation__output_result() %>% dplyr::rename(Qsim = q) %>%
                dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
                dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim),
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(simulation__output_result()$q, simulation__output_result()$Qobs),
                  hydroGOF::NSE(simulation__output_result()$q, simulation__output_result()$Qobs),
                  hydroGOF::rmse(simulation__output_result()$q, simulation__output_result()$Qobs),
                  hydroGOF::pbias(simulation__output_result()$q, simulation__output_result()$Qobs),
                  hydroGOF::rPearson(simulation__output_result()$q, simulation__output_result()$Qobs)**2
                )
              )
            ),
            path = file
          )
        }
      )

    })

    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    # RESULTATS TABULAIRES

    #période init
    output$tabular_out<- renderUI({
      req(cross__validation_period_1_1())
      fluidRow(
        column(12, tags$hr(style="border-color:blue;")),
        column(12,
               div(
                 paste0(
                   "Résultats De La Simulation Sur La Période De Calage 1 :::: ",
                   "[Début : ", as.character(cross__validation_period_1_1()[1]),
                   "  ----  Fin : ", as.character(cross__validation_period_1_1()[2]), "]", sep = ""
                 ), style="color:#3474A7;family:Georgia;text-align:left;"
               )
        )
      )
    })

    # rendering table
    output$model_out<- renderDT({
      req(calibration_1())
      tryCatch({
        calibration_1() %>% dplyr::rename(Qsim = q) %>%
          dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
          dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
      }, error = function(w) {
         # shinyalert("Error!", w$message, type = "error")
        return()
       }
      )
    }, options = list(pageLength = 100))

    # Calibration 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage1, {
      req(cross__validation_period_1_1(), calibration_1())

      #période
      output$tabular_out<- renderUI({
        req(cross__validation_period_1_1())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur La Période De Calage 1 :::: ",
                     "[Début : ", as.character(cross__validation_period_1_1()[1]),
                     "  ----  Fin : ", as.character(cross__validation_period_1_1()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # rendering table
      output$model_out<- renderDT({
        req(calibration_1())
        tryCatch({
          calibration_1() %>% dplyr::rename(Qsim = q) %>%
            dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
            dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
         }
        )
      }, options = list(pageLength = 100))

    })

    # Validation 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$validation1, {
      req(cross__validation_period_1_2(), validation_1())

      #période
      output$tabular_out<- renderUI({
        req(cross__validation_period_1_2())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur La Période De Validation 1 :::: ",
                     "[Début : ", as.character(cross__validation_period_1_2()[1]),
                     "  ----  Fin : ", as.character(cross__validation_period_1_2()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # rendering table
      output$model_out<- renderDT({
        req(validation_1())
        tryCatch({
          validation_1() %>% dplyr::rename(Qsim = q) %>%
            dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
            dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # Calibration 2
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage2, {
      req(cross__validation_period_2_1(), calibration_2())

      #période
      output$tabular_out<- renderUI({
        req(cross__validation_period_2_1())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur La Période De Calage 2 :::: ",
                     "[Début : ", as.character(cross__validation_period_2_1()[1]),
                     "  ----  Fin : ", as.character(cross__validation_period_2_1()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # rendering table
      output$model_out<- renderDT({
        req(calibration_2())
        tryCatch({
          calibration_2() %>% dplyr::rename(Qsim = q) %>%
            dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
            dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # Validation 2
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$validation2, {
      req(cross__validation_period_2_2(), validation_2())

      #période
      output$tabular_out<- renderUI({
        req(cross__validation_period_2_2())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur La Période De Validation 2 :::: ",
                     "[Début : ", as.character(cross__validation_period_2_2()[1]),
                     "  ----  Fin : ", as.character(cross__validation_period_2_2()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # rendering table
      output$model_out<- renderDT({
        req(validation_2())
        tryCatch({
          validation_2() %>% dplyr::rename(Qsim = q) %>%
            dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
            dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # simulation
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$simulation2, {
      req(simulation__output_result(), simulation__period())

      #période
      output$tabular_out<- renderUI({
        req(simulation__period())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur Toute La Période :::: ",
                     "[Début : ", as.character(simulation__period()[1]),
                     "  ----  Fin : ", as.character(simulation__period()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # rendering table
      output$model_out<- renderDT({
        req(simulation__output_result())
        tryCatch({
          simulation__output_result() %>% dplyr::rename(Qsim = q) %>%
            dplyr::mutate(Date = as.character(as.Date(date), format = "%d-%m-%Y")) %>%
            dplyr::select(Date, PmmObs, ETP, Temp, Qobs, qs, qi, qb, Storage, SWE, AET, SF, S1, S2, soil, w, Qsim) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

  })
}

## To be copied in the UI
# mod_hbv_results_graphs_n_exportation_ui("hbv_results_graphs_n_exportation_1")

## To be copied in the server
# mod_hbv_results_graphs_n_exportation_server("hbv_results_graphs_n_exportation_1")
