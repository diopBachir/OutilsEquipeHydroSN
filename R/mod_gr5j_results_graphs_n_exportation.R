#' gr5j_results_graphs_n_exportation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr5j_results_graphs_n_exportation_ui <- function(id){
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

#' gr5j_results_graphs_n_exportation Server Functions
#'
#' @noRd
mod_gr5j_results_graphs_n_exportation_server <- function(
    # id
  id,
  # période d'échaffement
  warm__up_period,
  # donnees utilisées
  cross__validation_data_first, cross__validation_data_second, overall__serie_data,
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
      req(
        cross__validation_data_first(), cross__validation_data_second(), cross__validation_period_1_1(),
        cross__validation_period_1_2(), calibration_1(), validation_1(), warm__up_period()
        # result_calibration_tb
      )

      # période d'exécution et d'échauffement
      # calibration 1
      ind_WarmUp_cal_cross1 <- seq(
        which(cross__validation_data_first()$date == min(cross__validation_data_first()$date, na.rm = TRUE)),
        which(cross__validation_data_first()$date == cross__validation_data_first()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_cal_cross1 <- seq(
        max(ind_WarmUp_cal_cross1) + 1,
        which(cross__validation_data_first()$date == max(cross__validation_data_first()$date, na.rm = TRUE))
      )
      #* validation 1
      ind_WarmUp_val_cross1 <- seq(
        which(cross__validation_data_second()$date == min(cross__validation_data_second()$date, na.rm = TRUE)),
        which(cross__validation_data_second()$date == cross__validation_data_second()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_val_cross1 <- seq(
        max(ind_WarmUp_val_cross1) + 1,
        which(cross__validation_data_second()$date == max(cross__validation_data_second()$date, na.rm = TRUE))
      )

      # modal dialog
      showModal(modalDialog(
        tags$h4("CALAGE/VALIDATION CROISEE [CROSS-VALIDATION]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("plot_cross_validation"))))
        ),
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
      output$plot_cross_validation<- renderPlot({
        req(calibration_1(), cross__validation_data_first(), ind_Run_cal_cross1)
        plot_OutputsModel_wrap(
          calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
          cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
        )
      })

      #----------------------------------------------------------------------------------------------#
      ###  Exportation
      #* jpeg
      output$toJPEG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = tibble::tibble(
                Date = as.character(as.Date(calibration_1()$DatesR), format = "%d-%m-%Y"),
                "PluiesObs (mm)" = calibration_1()$Precip, "ETPobs (mm)" = calibration_1()$PotEvap, "Pn (mm)" = calibration_1()$Pn,
                "En (mm)" = calibration_1()$En, "Ps (mm)" = calibration_1()$Ps, "S (mm)" = calibration_1()$Prod,
                "Perc (mm)" = calibration_1()$Perc, "Pr (mm)" = calibration_1()$PR, "UH1 (mm)" = calibration_1()$Q9,
                "UH2 (mm)" = calibration_1()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
                "Qsim (mm)" = calibration_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                "Qsim_cumul (mm)" = cumsum(calibration_1()$Qsim)
              ) ,
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                  hydroGOF::NSE(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                  hydroGOF::rmse(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                  hydroGOF::pbias(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                  hydroGOF::rPearson(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1])**2
                )
              )
            ),
            path = file
          )
        }
      )

      #===========================================================================================================#

      # click pour afficher le graph sur la période de calibration
      observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$calage_plot, {
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
        output$plot_cross_validation<- renderPlot({
          req(calibration_1(), cross__validation_data_first(), ind_Run_cal_cross1)
          plot_OutputsModel_wrap(
            calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              calibration_1(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = tibble::tibble(
                  Date = as.character(as.Date(calibration_1()$DatesR), format = "%d-%m-%Y"),
                  "PluiesObs (mm)" = calibration_1()$Precip, "ETPobs (mm)" = calibration_1()$PotEvap, "Pn (mm)" = calibration_1()$Pn,
                  "En (mm)" = calibration_1()$En, "Ps (mm)" = calibration_1()$Ps, "S (mm)" = calibration_1()$Prod,
                  "Perc (mm)" = calibration_1()$Perc, "Pr (mm)" = calibration_1()$PR, "UH1 (mm)" = calibration_1()$Q9,
                  "UH2 (mm)" = calibration_1()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
                  "Qsim (mm)" = calibration_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                  "Qsim_cumul (mm)" = cumsum(calibration_1()$Qsim)
                ) ,
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                    hydroGOF::NSE(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                    hydroGOF::rmse(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                    hydroGOF::pbias(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
                    hydroGOF::rPearson(calibration_1()$Qsim, cross__validation_data_first()$Qobs[ind_Run_cal_cross1])**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

      # click pour afficher le graph sur la période de validation
      observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$validation_plot, {
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
        # plot
        output$plot_cross_validation<- renderPlot({
          req(validation_1(), cross__validation_data_second(), ind_Run_val_cross1)
          plot_OutputsModel_wrap(
            validation_1(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_val_cross1],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              validation_1(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_val_cross1],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              validation_1(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_val_cross1],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation1--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = tibble::tibble(
                  Date = as.character(as.Date(validation_1()$DatesR), format = "%d-%m-%Y"),
                  "PluiesObs (mm)" = validation_1()$Precip, "ETPobs (mm)" = validation_1()$PotEvap, "Pn (mm)" = validation_1()$Pn,
                  "En (mm)" = validation_1()$En, "Ps (mm)" = validation_1()$Ps, "S (mm)" = validation_1()$Prod,
                  "Perc (mm)" = validation_1()$Perc, "Pr (mm)" = validation_1()$PR, "UH1 (mm)" = validation_1()$Q9,
                  "UH2 (mm)" = validation_1()$Q1, "Qobs (mm)" = cross__validation_data_second()$Qobs[ind_Run_val_cross1],
                  "Qsim (mm)" = validation_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
                  "Qsim_cumul (mm)" = cumsum(validation_1()$Qsim)
                ) ,
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(validation_1()$Qsim, cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
                    hydroGOF::NSE(validation_1()$Qsim, cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
                    hydroGOF::rmse(validation_1()$Qsim, cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
                    hydroGOF::pbias(validation_1()$Qsim, cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
                    hydroGOF::rPearson(validation_1()$Qsim, cross__validation_data_second()$Qobs[ind_Run_val_cross1])**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })
    })

    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

    # CROSS VALIDATION 2
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$cross_validation_plot2, {
      req(
        cross__validation_data_first(), cross__validation_data_second(), cross__validation_period_2_1(),
        cross__validation_period_2_2(), calibration_2(), validation_2(), warm__up_period()
        # result_calibration_tb
      )

      # période d'exécution et d'échauffement
      # calibration 1
      ind_WarmUp_cal_cross2 <- seq(
        which(cross__validation_data_second()$date == min(cross__validation_data_second()$date, na.rm = TRUE)),
        which(cross__validation_data_second()$date == cross__validation_data_second()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_cal_cross2 <- seq(
        max(ind_WarmUp_cal_cross2) + 1,
        which(cross__validation_data_second()$date == max(cross__validation_data_second()$date, na.rm = TRUE))
      )
      #* validation 1
      ind_WarmUp_val_cross2 <- seq(
        which(cross__validation_data_first()$date == min(cross__validation_data_first()$date, na.rm = TRUE)),
        which(cross__validation_data_first()$date == cross__validation_data_first()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_val_cross2 <- seq(
        max(ind_WarmUp_val_cross2) + 1,
        which(cross__validation_data_first()$date == max(cross__validation_data_first()$date, na.rm = TRUE))
      )

      # modal dialog
      showModal(modalDialog(
        tags$h4("CALAGE/VALIDATION CROISEE [CROSS-VALIDATION]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot2"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("plot_cross_validation2"))))
        ),
        footer=tagList(
          fluidRow(align = "center",
                   column(2, actionButton(
                     ns("calage_plot2"), label=span("Calib...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")
                   ),
                   column(2, actionButton(
                     ns("validation_plot2"), label=span("Valid...Period", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")),
                   column(2, downloadButton(ns("toExcel2"), label="EXCEL", icon = icon("file-excel"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toJPEG2"), label="JPEG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, downloadButton(ns("toSVG2"), label="SVG", icon = icon("download"), class= "butt", width = "100%")),
                   column(2, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      #période
      output$periode_plot2<- renderUI({
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
      output$plot_cross_validation2<- renderPlot({
        req(calibration_2(), cross__validation_data_second(), ind_Run_cal_cross2)
        plot_OutputsModel_wrap(
          calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
          cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
        )
      })

      #----------------------------------------------------------------------------------------------#
      ###  Exportation
      #* jpeg
      output$toJPEG2 <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG2 <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel2 <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = tibble::tibble(
                Date = as.character(as.Date(calibration_2()$DatesR), format = "%d-%m-%Y"),
                "PluiesObs (mm)" = calibration_2()$Precip, "ETPobs (mm)" = calibration_2()$PotEvap, "Pn (mm)" = calibration_2()$Pn,
                "En (mm)" = calibration_2()$En, "Ps (mm)" = calibration_2()$Ps, "S (mm)" = calibration_2()$Prod,
                "Perc (mm)" = calibration_2()$Perc, "Pr (mm)" = calibration_2()$PR, "UH1 (mm)" = calibration_2()$Q9,
                "UH2 (mm)" = calibration_2()$Q1, "Qobs (mm)" = cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
                "Qsim (mm)" = calibration_2()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                "Qsim_cumul (mm)" = cumsum(calibration_2()$Qsim)
              ) ,
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                  hydroGOF::NSE(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                  hydroGOF::rmse(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                  hydroGOF::pbias(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                  hydroGOF::rPearson(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2])**2
                )
              )
            ),
            path = file
          )
        }
      )

      #===========================================================================================================#

      # click pour afficher le graph sur la période de calibration
      observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$calage_plot2, {
        #période
        output$periode_plot2<- renderUI({
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
        output$plot_cross_validation2<- renderPlot({
          req(calibration_2(), cross__validation_data_second(), ind_Run_cal_cross2)
          plot_OutputsModel_wrap(
            calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              calibration_2(), Qobs =  cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Calibration_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = tibble::tibble(
                  Date = as.character(as.Date(calibration_2()$DatesR), format = "%d-%m-%Y"),
                  "PluiesObs (mm)" = calibration_2()$Precip, "ETPobs (mm)" = calibration_2()$PotEvap, "Pn (mm)" = calibration_2()$Pn,
                  "En (mm)" = calibration_2()$En, "Ps (mm)" = calibration_2()$Ps, "S (mm)" = calibration_2()$Prod,
                  "Perc (mm)" = calibration_2()$Perc, "Pr (mm)" = calibration_2()$PR, "UH1 (mm)" = calibration_2()$Q9,
                  "UH2 (mm)" = calibration_2()$Q1, "Qobs (mm)" = cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
                  "Qsim (mm)" = calibration_2()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                  "Qsim_cumul (mm)" = cumsum(calibration_2()$Qsim)
                ) ,
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                    hydroGOF::NSE(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                    hydroGOF::rmse(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                    hydroGOF::pbias(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
                    hydroGOF::rPearson(calibration_2()$Qsim, cross__validation_data_second()$Qobs[ind_Run_cal_cross2])**2
                  )
                )
              ),
              path = file
            )
          }
        )
      })

      # click pour afficher le graph sur la période de validation
      observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE, input$validation_plot2, {
        # période
        output$periode_plot2<- renderUI({
          req(cross__validation_period_2_2())
          div(
            paste0(
              "Résultats De La Simulation Sur La Période De Validation :::: ",
              "[Début : ", as.character(cross__validation_period_2_2()[1]),
              "  ----  Fin : ", as.character(cross__validation_period_2_2()[2]), "]", sep = ""
            ), style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        # plot
        output$plot_cross_validation2<- renderPlot({
          req(validation_2(), cross__validation_data_first(), ind_Run_val_cross2)
          plot_OutputsModel_wrap(
            validation_2(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_val_cross2],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
        })

        #----------------------------------------------------------------------------------------------#
        ###  Exportation
        #* jpeg
        output$toJPEG2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            # 1. Open jpeg file
            jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              validation_2(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_val_cross2],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #* svg
        output$toSVG2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            # 1. Open jpeg file
            svg(file=file,   width = 13.3, height = 7.05)
            # 2. Create the plot
            plot_OutputsModel_wrap(
              validation_2(), Qobs =  cross__validation_data_first()$Qobs[ind_Run_val_cross2],
              cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
            )
            # 3. Close the file
            dev.off()
          }
        )
        #*excel
        output$toExcel2 <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation2--GR5J--Validation_Period-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              list(
                Debits = tibble::tibble(
                  Date = as.character(as.Date(validation_2()$DatesR), format = "%d-%m-%Y"),
                  "PluiesObs (mm)" = validation_2()$Precip, "ETPobs (mm)" = validation_2()$PotEvap, "Pn (mm)" = validation_2()$Pn,
                  "En (mm)" = validation_2()$En, "Ps (mm)" = validation_2()$Ps, "S (mm)" = validation_2()$Prod,
                  "Perc (mm)" = validation_2()$Perc, "Pr (mm)" = validation_2()$PR, "UH1 (mm)" = validation_2()$Q9,
                  "UH2 (mm)" = validation_2()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_val_cross2],
                  "Qsim (mm)" = validation_2()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
                  "Qsim_cumul (mm)" = cumsum(validation_2()$Qsim)
                ) ,
                Evaluation = tibble::tibble(
                  Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                  Valeur = c(
                    hydroGOF::KGE(validation_2()$Qsim, cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
                    hydroGOF::NSE(validation_2()$Qsim, cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
                    hydroGOF::rmse(validation_2()$Qsim, cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
                    hydroGOF::pbias(validation_2()$Qsim, cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
                    hydroGOF::rPearson(validation_2()$Qsim, cross__validation_data_first()$Qobs[ind_Run_val_cross2])**2
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
      req(overall__serie_data(), simulation__period(), simulation__output_result(), warm__up_period())

      # modal dialog
      showModal(modalDialog(
        tags$h4("RESULTATS DE LA SIMULATION SUR TOUTE LA PERIODE", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot_simulation"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("plot_simulation"))))
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

      # période d'exécution et d'échauffement
      ind_WarmUp_simulation <- seq(
        which(overall__serie_data()$date == min(overall__serie_data()$date, na.rm = T)),
        which(overall__serie_data()$date == overall__serie_data()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_simulation <- seq(
        max(ind_WarmUp_simulation) + 1,
        which(overall__serie_data()$date ==  max(overall__serie_data()$date, na.rm = T))
      )

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
      output$plot_simulation<- renderPlot({
        req(simulation__output_result(), overall__serie_data(), ind_Run_simulation)
        tryCatch({
          plot_OutputsModel_wrap(
            simulation__output_result(), Qobs =  overall__serie_data()$Qobs[ind_Run_simulation],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )}, error = function(e) {
            shinyalert("Error !", e$message, type = "error")
            return()
          }, warning = function(w) {
            shinyalert("Warning !", w$message, type = "warning")
            return(
              plot_OutputsModel_wrap(
                simulation__output_result(), Qobs =  overall__serie_data()$Qobs[ind_Run_simulation],
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
          paste("Simulation-GR5J-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          # 1. Open jpeg file
          jpeg(file=file,   width = 13.3, height = 7.05, units = 'in', res = 300)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            simulation__output_result(), Qobs =  overall__serie_data()$Qobs[ind_Run_simulation],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #* svg
      output$toSVG3 <-  downloadHandler(
        filename = function() {
          paste("Simulation-GR5J-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          # 1. Open jpeg file
          svg(file=file,   width = 13.3, height = 7.05)
          # 2. Create the plot
          plot_OutputsModel_wrap(
            simulation__output_result(), Qobs =  overall__serie_data()$Qobs[ind_Run_simulation],
            cex.axis = 1.3, cex.lab = 1, cex.leg = 1.25, lwd = 1.4
          )
          # 3. Close the file
          dev.off()
        }
      )
      #*excel
      output$toExcel3 <-  downloadHandler(
        filename = function() {
          paste("Simulation-GR5J-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(
            list(
              Debits = tibble::tibble(
                Date = as.character(as.Date(simulation__output_result()$DatesR), format = "%d-%m-%Y"),
                "PluiesObs (mm)" = simulation__output_result()$Precip, "ETPobs (mm)" = simulation__output_result()$PotEvap, "Pn (mm)" = simulation__output_result()$Pn,
                "En (mm)" = simulation__output_result()$En, "Ps (mm)" = simulation__output_result()$Ps, "S (mm)" = simulation__output_result()$Prod,
                "Perc (mm)" = simulation__output_result()$Perc, "Pr (mm)" = simulation__output_result()$PR, "UH1 (mm)" = simulation__output_result()$Q9,
                "UH2 (mm)" = simulation__output_result()$Q1, "Qobs (mm)" = overall__serie_data()$Qobs[ind_Run_simulation],
                "Qsim (mm)" = simulation__output_result()$Qsim, "Qobs_cumul (mm)" = cumsum(overall__serie_data()$Qobs[ind_Run_simulation]),
                "Qsim_cumul (mm)" = cumsum(simulation__output_result()$Qsim)
              ) ,
              Evaluation = tibble::tibble(
                Critères = c("KGE", "NSE", "RMSE", "PBIAIS", "R2"),
                Valeur = c(
                  hydroGOF::KGE(simulation__output_result()$Qsim, overall__serie_data()$Qobs[ind_Run_simulation]),
                  hydroGOF::NSE(simulation__output_result()$Qsim, overall__serie_data()$Qobs[ind_Run_simulation]),
                  hydroGOF::rmse(simulation__output_result()$Qsim, overall__serie_data()$Qobs[ind_Run_simulation]),
                  hydroGOF::pbias(simulation__output_result()$Qsim, overall__serie_data()$Qobs[ind_Run_simulation]),
                  hydroGOF::rPearson(simulation__output_result()$Qsim, overall__serie_data()$Qobs[ind_Run_simulation])**2
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
      req(calibration_1(), cross__validation_data_first(), warm__up_period())

      # période d'exécution et d'échauffement
      ind_WarmUp_cal_cross1 <- seq(
        which(cross__validation_data_first()$date == min(cross__validation_data_first()$date, na.rm = TRUE)),
        which(cross__validation_data_first()$date == cross__validation_data_first()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_cal_cross1 <- seq(
        max(ind_WarmUp_cal_cross1) + 1,
        which(cross__validation_data_first()$date == max(cross__validation_data_first()$date, na.rm = TRUE))
      )
      tryCatch({
        tibble::tibble(
          Date = as.character(as.Date(calibration_1()$DatesR), format = "%d-%m-%Y"),
          "PluiesObs (mm)" = calibration_1()$Precip, "ETPobs (mm)" = calibration_1()$PotEvap, "Pn (mm)" = calibration_1()$Pn,
          "En (mm)" = calibration_1()$En, "Ps (mm)" = calibration_1()$Ps, "S (mm)" = calibration_1()$Prod,
          "Perc (mm)" = calibration_1()$Perc, "Pr (mm)" = calibration_1()$PR, "UH1 (mm)" = calibration_1()$Q9,
          "UH2 (mm)" = calibration_1()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
          "Qsim (mm)" = calibration_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
          "Qsim_cumul (mm)" = cumsum(calibration_1()$Qsim)
        ) %>%
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
      }, error = function(w) {
        # shinyalert("Error!", w$message, type = "error")
        return()
      }
      )
    }, options = list(pageLength = 100), future = TRUE)

    # Calibration 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage1, {
      req(cross__validation_data_first(), cross__validation_period_1_1(), calibration_1(), warm__up_period())

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

      # période d'exécution et d'échauffement
      ind_WarmUp_cal_cross1 <- seq(
        which(cross__validation_data_first()$date == min(cross__validation_data_first()$date, na.rm = TRUE)),
        which(cross__validation_data_first()$date == cross__validation_data_first()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_cal_cross1 <- seq(
        max(ind_WarmUp_cal_cross1) + 1,
        which(cross__validation_data_first()$date == max(cross__validation_data_first()$date, na.rm = TRUE))
      )

      # rendering table
      output$model_out<- renderDT({
        tryCatch({
          tibble::tibble(
            Date = as.character(as.Date(calibration_1()$DatesR), format = "%d-%m-%Y"),
            "PluiesObs (mm)" = calibration_1()$Precip, "ETPobs (mm)" = calibration_1()$PotEvap, "Pn (mm)" = calibration_1()$Pn,
            "En (mm)" = calibration_1()$En, "Ps (mm)" = calibration_1()$Ps, "S (mm)" = calibration_1()$Prod,
            "Perc (mm)" = calibration_1()$Perc, "Pr (mm)" = calibration_1()$PR, "UH1 (mm)" = calibration_1()$Q9,
            "UH2 (mm)" = calibration_1()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_cal_cross1],
            "Qsim (mm)" = calibration_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_cal_cross1]),
            "Qsim_cumul (mm)" = cumsum(calibration_1()$Qsim)
          ) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        },  error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # Validation 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$validation1, {
      req(cross__validation_data_second(), cross__validation_period_1_2(), validation_1(), warm__up_period())

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

      # période d'exécution et d'échauffement
      ind_WarmUp_val_cross1 <- seq(
        which(cross__validation_data_second()$date == min(cross__validation_data_second()$date, na.rm = TRUE)),
        which(cross__validation_data_second()$date == cross__validation_data_second()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_val_cross1 <- seq(
        max(ind_WarmUp_val_cross1) + 1,
        which(cross__validation_data_second()$date == max(cross__validation_data_second()$date, na.rm = TRUE))
      )

      # rendering table
      output$model_out<- renderDT({
        tryCatch({
          tibble::tibble(
            Date = as.character(as.Date(validation_1()$DatesR), format = "%d-%m-%Y"),
            "PluiesObs (mm)" = validation_1()$Precip, "ETPobs (mm)" = validation_1()$PotEvap, "Pn (mm)" = validation_1()$Pn,
            "En (mm)" = validation_1()$En, "Ps (mm)" = validation_1()$Ps, "S (mm)" = validation_1()$Prod,
            "Perc (mm)" = validation_1()$Perc, "Pr (mm)" = validation_1()$PR, "UH1 (mm)" = validation_1()$Q9,
            "UH2 (mm)" = validation_1()$Q1, "Qobs (mm)" = cross__validation_data_second()$Qobs[ind_Run_val_cross1],
            "Qsim (mm)" = validation_1()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_second()$Qobs[ind_Run_val_cross1]),
            "Qsim_cumul (mm)" = cumsum(validation_1()$Qsim)
          ) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # Calibration 1
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage2, {
      req(cross__validation_data_second(), cross__validation_period_2_1(), calibration_2(), warm__up_period())

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

      # période d'exécution et d'échauffement
      ind_WarmUp_cal_cross2 <- seq(
        which(cross__validation_data_second()$date == min(cross__validation_data_second()$date, na.rm = TRUE)),
        which(cross__validation_data_second()$date == cross__validation_data_second()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_cal_cross2 <- seq(
        max(ind_WarmUp_cal_cross2) + 1,
        which(cross__validation_data_second()$date == max(cross__validation_data_second()$date, na.rm = TRUE))
      )

      # rendering table
      output$model_out<- renderDT({
        tryCatch({
          tibble::tibble(
            Date = as.character(as.Date(calibration_2()$DatesR), format = "%d-%m-%Y"),
            "PluiesObs (mm)" = calibration_2()$Precip, "ETPobs (mm)" = calibration_2()$PotEvap, "Pn (mm)" = calibration_2()$Pn,
            "En (mm)" = calibration_2()$En, "Ps (mm)" = calibration_2()$Ps, "S (mm)" = calibration_2()$Prod,
            "Perc (mm)" = calibration_2()$Perc, "Pr (mm)" = calibration_2()$PR, "UH1 (mm)" = calibration_2()$Q9,
            "UH2 (mm)" = calibration_2()$Q1, "Qobs (mm)" = cross__validation_data_second()$Qobs[ind_Run_cal_cross2],
            "Qsim (mm)" = calibration_2()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_second()$Qobs[ind_Run_cal_cross2]),
            "Qsim_cumul (mm)" = cumsum(calibration_2()$Qsim)
          ) %>%
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
      req(cross__validation_data_first(), cross__validation_period_2_2(), validation_2(), warm__up_period())

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

      # période d'exécution et d'échauffement
      ind_WarmUp_val_cross2 <- seq(
        which(cross__validation_data_first()$date == min(cross__validation_data_first()$date, na.rm = TRUE)),
        which(cross__validation_data_first()$date == cross__validation_data_first()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_val_cross2 <- seq(
        max(ind_WarmUp_val_cross2) + 1,
        which(cross__validation_data_first()$date == max(cross__validation_data_first()$date, na.rm = TRUE))
      )

      # rendering table
      output$model_out<- renderDT({
        tryCatch({
          tibble::tibble(
            Date = as.character(as.Date(validation_2()$DatesR), format = "%d-%m-%Y"),
            "PluiesObs (mm)" = validation_2()$Precip, "ETPobs (mm)" = validation_2()$PotEvap, "Pn (mm)" = validation_2()$Pn,
            "En (mm)" = validation_2()$En, "Ps (mm)" = validation_2()$Ps, "S (mm)" = validation_2()$Prod,
            "Perc (mm)" = validation_2()$Perc, "Pr (mm)" = validation_2()$PR, "UH1 (mm)" = validation_2()$Q9,
            "UH2 (mm)" = validation_2()$Q1, "Qobs (mm)" = cross__validation_data_first()$Qobs[ind_Run_val_cross2],
            "Qsim (mm)" = validation_2()$Qsim, "Qobs_cumul (mm)" = cumsum(cross__validation_data_first()$Qobs[ind_Run_val_cross2]),
            "Qsim_cumul (mm)" = cumsum(validation_2()$Qsim)
          ) %>%
            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 4), .names = "{.col}"))
        }, error = function(w) {
          # shinyalert("Error!", w$message, type = "error")
          return()
        }
        )
      }, options = list(pageLength = 100))

    })

    # Simulation
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$simulation2, {
      req(overall__serie_data(), simulation__period(), simulation__output_result(), warm__up_period())

      #période
      output$tabular_out<- renderUI({
        req(simulation__period())
        fluidRow(
          column(12, tags$hr(style="border-color:blue;")),
          column(12,
                 div(
                   paste0(
                     "Résultats De La Simulation Sur La Toute La Serie :::: ",
                     "[Début : ", as.character(simulation__period()[1]),
                     "  ----  Fin : ", as.character(simulation__period()[2]), "]", sep = ""
                   ), style="color:#3474A7;family:Georgia;text-align:left;"
                 )
          )
        )
      })

      # période d'exécution et d'échauffement
      ind_WarmUp_simulation <- seq(
        which(overall__serie_data()$date == min(overall__serie_data()$date, na.rm = TRUE)),
        which(overall__serie_data()$date == overall__serie_data()$date[1]+(lubridate::days(warm__up_period())-lubridate::days(1)))
      )
      ind_Run_simulation <- seq(
        max(ind_WarmUp_simulation) + 1,
        which(overall__serie_data()$date == max(overall__serie_data()$date, na.rm = TRUE))
      )

      # rendering table
      output$model_out<- renderDT({
        tryCatch({
          tibble::tibble(
            Date = as.character(as.Date(simulation__output_result()$DatesR), format = "%d-%m-%Y"),
            "PluiesObs (mm)" = simulation__output_result()$Precip, "ETPobs (mm)" = simulation__output_result()$PotEvap, "Pn (mm)" = simulation__output_result()$Pn,
            "En (mm)" = simulation__output_result()$En, "Ps (mm)" = simulation__output_result()$Ps, "S (mm)" = simulation__output_result()$Prod,
            "Perc (mm)" = simulation__output_result()$Perc, "Pr (mm)" = simulation__output_result()$PR, "UH1 (mm)" = simulation__output_result()$Q9,
            "UH2 (mm)" = simulation__output_result()$Q1, "Qobs (mm)" = overall__serie_data()$Qobs[ind_Run_simulation],
            "Qsim (mm)" = simulation__output_result()$Qsim, "Qobs_cumul (mm)" = cumsum(overall__serie_data()$Qobs[ind_Run_simulation]),
            "Qsim_cumul (mm)" = cumsum(simulation__output_result()$Qsim)
          ) %>%
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
# mod_gr5j_results_graphs_n_exportation_ui("gr5j_results_graphs_n_exportation_1")

## To be copied in the server
# mod_gr5j_results_graphs_n_exportation_server("gr5j_results_graphs_n_exportation_1")
