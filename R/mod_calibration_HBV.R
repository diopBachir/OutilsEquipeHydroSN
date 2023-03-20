#' calibration_HBV UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calibration_HBV_ui <- function(id){
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

    br(),

    fluidRow(align="center",
             column(6,
                    actionButton(
                      ns("calage_validation"), icon = icon("wrench"), class = "btn-info", width = "100%",
                      label=span("Calage|Validation Croisé [Cross-Validation]", id=ns("calage_validation_animate1"),
                                 style = "font-size:110%;")
                    )
             ),
             column(6,
                    actionButton(
                      ns("simulation"), label=span("Simulation [Overall Serie]", id=ns("simulation_animate"),
                                                   style = "font-size:110%;"),
                      icon = icon("buromobelexperte"), class = "btn-info", width = "100%"
                    )
             )
    ),
    tags$hr(style="border-color:gray;"),

    fluidRow(
      column(12, uiOutput(ns("parameters_and_evaluation_criteria"))),
      column(12, uiOutput(ns("parameters_and_evaluation_criteria2")))
    )
  )
}

#' calibration_HBV Server Functions
#'
#' @noRd
mod_calibration_HBV_server <- function(id, donnees, options_de_mise_en_route, parametres_du_modele){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### fonction d'affichage du top des sets de paramètres
    monte_carlo_rank_10<- function(params, nb_iter){
      #set the min and max values for each parameter for the plot
      mins <- c(40, 1, 0.3 , 0.4, -1.5, 1, 0.05, 0.01, 0.001, 0, 0, 1, 0, 0)
      maxs <- c(400, 6, 1, 1.2, 1.2, 8, 0.5, 0.3, 0.15, 70 , 4, 3, 0, 0)
      #add maxes and mins to the parameter sets
      rankedpars <- rbind(params, mins, maxs)
      #Make KGE a character so the legend will work properly
      rankedpars <- rankedpars %>% mutate(KGE = as.character(round(KGE, 2)))
      #Make the KGE equal "Min/Max" for appropriate legend labels
      rankedpars$KGE[rankedpars$KGE == "0"] <- "Min/Max"

      #Make the ranks a factor datatype
      rankedpars <- rankedpars %>% mutate(Rank = factor(Rank))

      #Create parallel coordinate plot
      print(
        rankedpars %>% GGally::ggparcoord(columns = 1:11,
                                          groupColumn = 13,
                                          showPoints = TRUE,
                                          scale = "uniminmax") +
          ggplot2::theme_minimal() +
          # ggplot2::ggtitle(paste0("Top 5 des Ensembles de Paramètres Optimisés |  ", nb_iter, " itérations Monte Carlo"))+
          # ggplot2::ylab("Valeurs Optimisées")+
          # ggplot2::xlab("Paramètres")+
          ggplot2::theme(plot.title = ggplot2::element_text(size=10, face="bold"))+
          ggplot2::labs(
            x="Valeurs Optimisées", y="Paramètres",
            title=(paste0("Top 5 des Ensembles de Paramètres Optimisés |  ", nb_iter, " itérations Monte Carlo"))
          )
      )
    }

    ## DECOUPAGE DE LA SERIE EN DEUX POUR LA VALIDATION CROISEE

    # première période
    dataObsFirst<- reactive({
      req(donnees, options_de_mise_en_route)
      # controler le choix des début et fin de période
      if(options_de_mise_en_route$startValidationDate1() >= options_de_mise_en_route$endValidationDate1()){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début [", options_de_mise_en_route$startValidationDate1(), "]",
            " soit strictement inférieure à la date de fin de période [", options_de_mise_en_route$endValidationDate1(), "] !"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      # controler si les dates entrées sont valides
      if(
        options_de_mise_en_route$startValidationDate1() < min(donnees$date, na.rm=TRUE) |  options_de_mise_en_route$startValidationDate1() > max(donnees$date, na.rm=TRUE) |
        options_de_mise_en_route$endValidationDate1() < min(donnees$date, na.rm=TRUE) |  options_de_mise_en_route$endValidationDate1() > max(donnees$date, na.rm=TRUE)
      ){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Les plages de dates autorisées se trouvent entre la date [", min(donnees$date, na.rm=TRUE), "]",
            " et la date [", max(donnees$date, na.rm=TRUE), "] ! NB : Cette restriction est déduite de l'analyse de la ",
            "série chronologique chargée."
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      req(options_de_mise_en_route$startValidationDate1() < options_de_mise_en_route$endValidationDate1() &
            options_de_mise_en_route$startValidationDate1() < options_de_mise_en_route$startValidationDate2() &
            options_de_mise_en_route$endValidationDate1() < options_de_mise_en_route$endValidationDate2() &
            options_de_mise_en_route$startValidationDate1() >= min(donnees$date, na.rm=TRUE) &
            options_de_mise_en_route$startValidationDate1() <= max(donnees$date, na.rm=TRUE) &
            options_de_mise_en_route$endValidationDate1() >= min(donnees$date, na.rm=TRUE) &
            options_de_mise_en_route$endValidationDate1() <= max(donnees$date, na.rm=TRUE)
      )

      donnees %>%
        dplyr::filter(
          (date >= options_de_mise_en_route$startValidationDate1()) & (date <= options_de_mise_en_route$endValidationDate1())
        )
    })

    # seconde periode
    dataObsSecond<-  reactive({
      req(donnees, options_de_mise_en_route)
      # contrôle des périodes
      if(options_de_mise_en_route$startValidationDate2() >= options_de_mise_en_route$endValidationDate2()){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début [", options_de_mise_en_route$startValidationDate2(), "]",
            " soit strictement inférieure à la date de fin de période [", options_de_mise_en_route$endValidationDate2(), "] !"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }
      if(options_de_mise_en_route$startValidationDate1() >= options_de_mise_en_route$startValidationDate2()){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début de la première période [", options_de_mise_en_route$startValidationDate1(), "]",
            " soit strictement inférieure à la date de début de la seconde période [", options_de_mise_en_route$startValidationDate2(), "] !"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      # controler si les dates entrées sont valides
      if(
        options_de_mise_en_route$startValidationDate2() < min(donnees$date, na.rm=TRUE) | options_de_mise_en_route$startValidationDate2() > max(donnees$date, na.rm=TRUE) |
        options_de_mise_en_route$endValidationDate2() < min(donnees$date, na.rm=TRUE) | options_de_mise_en_route$endValidationDate2() > max(donnees$date, na.rm=TRUE)
      ){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Les plages de dates autorisées se trouvent entre la date [", min(donnees$date, na.rm=TRUE), "]",
            " et la date [", max(donnees$date, na.rm=TRUE), "] ! NB : Cette restriction est déduite de l'analyse de la ",
            "série chronologique chargée."
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }

      req(
        options_de_mise_en_route$startValidationDate2() < options_de_mise_en_route$endValidationDate2() &
          options_de_mise_en_route$startValidationDate1() < options_de_mise_en_route$startValidationDate2() &
          options_de_mise_en_route$endValidationDate1() < options_de_mise_en_route$endValidationDate2() &
          options_de_mise_en_route$startValidationDate2() >= min(donnees$date, na.rm=TRUE) &
          options_de_mise_en_route$startValidationDate2() <= max(donnees$date, na.rm=TRUE) &
          options_de_mise_en_route$endValidationDate2() >= min(donnees$date, na.rm=TRUE) &
          options_de_mise_en_route$endValidationDate2() <= max(donnees$date, na.rm=TRUE)
      )
      donnees %>%
        dplyr::filter(
          (date >= options_de_mise_en_route$startValidationDate2()) & (date <= options_de_mise_en_route$endValidationDate2())
        )
    })

    # global serie
    dataOverAll<-  reactive({
      req(donnees, options_de_mise_en_route, dataObsFirst(), dataObsSecond())
      # controle des périodes
      if(options_de_mise_en_route$startValidationDate1() >= options_de_mise_en_route$endValidationDate2()){
        shinyalert::shinyalert(
          "OUPS !",
          paste0(
            "Intervalle de Date Incorrecte. Assurez-vous que la date de début de la première période [", options_de_mise_en_route$startValidationDate1(), "]",
            " soit strictement inférieure à la date de fin de la seconde période [", options_de_mise_en_route$startValidationDate2(), "] !",
            " NB : La période de simulation est déduite par défaut à partir des bornes de périodes définies dans la validation croisée"
          ),
          type = "", imageUrl = "www/calendar_13_512.webp"
        )
      }
      donnees %>%
        dplyr::filter(
          (date >= options_de_mise_en_route$startValidationDate1()) & (date <= options_de_mise_en_route$endValidationDate2())
        )
    })

    #=========================================================================================#
    # STOCKAGE POUR TRANSFERT
    # validation_croisée 1
    calibration_output_1<- reactiveVal()
    validation_output_1<- reactiveVal()
    # validation_croisée 2
    calibration_output_2<- reactiveVal()
    validation_output_2<- reactiveVal()
    # résultats de la simulation
    simulation_output<- reactiveVal()

    ### calage validation croisée
    #----------------------------------------------------------|
    cross_validation_param_result1<- reactiveVal()
    cross_validation_param_result1_rank10<- reactiveVal()
    cross_validation_cal_result1<- reactiveVal()
    cross_validation_val_result1<- reactiveVal()
    periode_cross1_1<- reactiveVal()
    periode_cross1_2<- reactiveVal()
    results_cross_validation1<- reactiveVal()
    #----------------------------------------------------------|
    #----------------------------------------------------------|
    cross_validation_param_result2<- reactiveVal()
    cross_validation_param_result2_rank10<- reactiveVal()
    cross_validation_cal_result2<- reactiveVal()
    cross_validation_val_result2<- reactiveVal()
    periode_cross2_1<- reactiveVal()
    periode_cross2_2<- reactiveVal()
    results_cross_validation2<- reactiveVal()
    #----------------------------------------------------------|
    # best parameters values
    cross_validation_best_values<- reactiveVal()
    #----------------------------------------------------------|
    simulation_param<- reactiveVal()
    simulation_evaluation_result<- reactiveVal()
    periode_simulation<- reactiveVal()

    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage_validation, {

      shiny::req(dataObsFirst(), dataObsSecond(), options_de_mise_en_route)

      testWarmup<- reactiveVal(TRUE)
      if(options_de_mise_en_route$nbWarmUpYear() >= nrow(dataObsFirst())){
        shinyalert::shinyalert(
          "Erreur !",
          "La période d'échaffement du modèle ne peut pas être supérieur ou égal à la période de Calibration.",
          type = "error"
        )
        testWarmup(FALSE)
      }

      req(testWarmup())


      # setting buttons with shinyjs
      shinyjs::addClass(id = "calage_validation_animate1", class = "loading dots")
      shinyjs::disable("calage_validation")

      #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      ### CALIBRATION VALIDATION 1 : Sur La Première Division Des données

      #### Calibration --------------------------------------------------------------#
      #------------------------------------------------------------------------------#
      ggplot2::theme_set(ggplot2::theme_classic())

      # Monte Carlo step 1: generate random parameter sets

      #number of runs
      N <- options_de_mise_en_route$nb_iteration()

      # PARAMETERS RANGE and generate set
      FC    <- runif(N, min = 40   , max = 400)  #Max soil moisture storage, field capacity
      beta  <- runif(N, min = 1    , max = 6)    #Shape coefficient governing fate of water input to soil moisture storage
      LP    <- runif(N, min = 0.3   , max = 1)    #Threshold for reduction of evap
      SFCF  <- runif(N, min = 0.4  , max = 1.2)  #Snowfall correction factor
      TT    <- runif(N, min = -1.5 , max = 1.2)  #Threshold temperature
      CFMAX <- runif(N, min = 1    , max = 8)    #Degree-day factor
      k0    <- runif(N, min = 0.05 , max = 0.5)  #Recession constant (upper storage, near surface)
      k1    <- runif(N, min = 0.01 , max = 0.3)  #Recession constant (upper storage)
      k2    <- runif(N, min = 0.001, max = 0.15) #Recession constant (lower storage)
      UZL   <- runif(N, min = 0    , max = 70)   #Threshold for shallow storage
      PERC  <- runif(N, min = 0    , max = 4)    #Percolation, max flow from upper to lower storage
      MAXBAS<- rep(1, N)   #base of the triangular routing function, days
      #MAXBAS is just 1's because routing will be set to zero, so the parameter isn't used

      KGE <- rep(NA, N) # create NSE column, to be filled in for loop

      # routing <- 0

      pars <- cbind(FC, beta, LP, SFCF, TT, CFMAX, k0, k1, k2, UZL, PERC, MAXBAS, KGE)

      #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
      #| CROSS VALIDATION / SAMPLING 2

      # Run the model for each parameter set
      #trim the first 40% (warm up) of Qobs off for NSE calculation
      EvalStart1_1 <- floor(length(dataObsFirst()$Qobs) * (options_de_mise_en_route$nbWarmUpYear()/100))
      EvalEnd1_1 <- length(dataObsFirst()$Qobs)

      shiny::withProgress(message = 'Cross-Validation [Sample 1] : Méthode Monte Carlo', value = 0, {
        for (i in 1:N){
          # Increment the progress bar, and update the detail text.
          shiny::incProgress(1/N, detail = paste0("Itération No : ", i))
          # call model with i parameter set generated above P, Temp, PET, routing
          results <- HBV(
            pars[i,1:12], dataObsFirst()$PmmObs, dataObsFirst()$Temp,
            dataObsFirst()$ETP, 0
          )
          #add the Qobs to results
          results <- cbind(results, dataObsFirst())
          #trim the first 40% of the record so it isn't included in the NSE calculation
          results <- results[EvalStart1_1:EvalEnd1_1,]
          #Calculate NSE and add to parameter set
          pars[i,13]  <- hydroGOF::KGE(results$q, results$Qobs)
        }
      })

      # Find the best parameter set
      pars <- tibble::as_tibble(pars)
      bestparams <- pars %>% dplyr::filter(KGE == max(KGE)) %>%
        dplyr::slice(1) %>%
        as.numeric()
      cross_validation_param_result1(bestparams)
      cross_validation_param_result1_rank10(
        data.frame(
          pars %>% dplyr::arrange(desc(KGE)) %>% slice(1:5) %>%
            dplyr::mutate(Rank = 1:5)
        )
      )

      #------------------------------------------------------------------------------------|

      #------------------------------------------------------------------------------------|
      # run du modèle sur la période calibration
      #run with best parameters
      modeloutput_calib1 <-  HBV(
        cross_validation_param_result1(), dataObsFirst()$PmmObs, dataObsFirst()$Temp,
        dataObsFirst()$ETP, 0
      )
      #add observations for plotting
      modeloutput_calib1 <- dplyr::bind_cols(modeloutput_calib1, dataObsFirst())
      #trim out warm up period for plotting
      OutputTrim_calib1 <- modeloutput_calib1[EvalStart1_1:EvalEnd1_1,]

      #Calculate evaluation criteria
      cross_validation_cal_result1(c(
        "KGE_calib_period" = hydroGOF::KGE(OutputTrim_calib1$q, OutputTrim_calib1$Qobs),
        "NSE_calib_period" = hydroGOF::NSE(OutputTrim_calib1$q, OutputTrim_calib1$Qobs),
        "RMSE_calib_period" = hydroGOF::rmse(OutputTrim_calib1$q, OutputTrim_calib1$Qobs)
      ))

      # début et fin période 1
      periode_cross1_1(
        c(
          "debut" = options_de_mise_en_route$startValidationDate1(),
          "fin" = options_de_mise_en_route$endValidationDate1()
        )
      )

      #------------------------------------------------------------------------------------|
      # run du modèle sur la période validation
      EvalStart1_2 <- floor(length(dataObsSecond()$Qobs) * (options_de_mise_en_route$nbWarmUpYear()/100))
      EvalEnd1_2 <- length(dataObsSecond()$Qobs)
      #run with best parameters
      modeloutput_valid1 <-  HBV(
        cross_validation_param_result1(), dataObsSecond()$PmmObs, dataObsSecond()$Temp,
        dataObsSecond()$ETP, 0
      )
      #add observations for plotting
      modeloutput_valid1 <- dplyr::bind_cols(modeloutput_valid1, dataObsSecond())
      #trim out warm up period for plotting
      OutputTrim_valid1 <- modeloutput_valid1[EvalStart1_2:EvalEnd1_2,]

      #Calculate evaluation criteria
      cross_validation_val_result1(c(
        "KGE_valib_period" = hydroGOF::KGE(OutputTrim_valid1$q, OutputTrim_valid1$Qobs),
        "NSE_valib_period" = hydroGOF::NSE(OutputTrim_valid1$q, OutputTrim_valid1$Qobs),
        "RMSE_valib_period" = hydroGOF::rmse(OutputTrim_valid1$q, OutputTrim_valid1$Qobs)
      ))

      # début et fin période 1
      periode_cross1_2(
        c(
          "debut" = options_de_mise_en_route$startValidationDate2(),
          "fin" = options_de_mise_en_route$endValidationDate2()
        )
      )

      # affichage des résultats de la cross-validation 1
      output$parameters_and_evaluation_criteria<- renderUI({
        req(cross_validation_cal_result2())
        fluidRow(align = "left",
                 column(12, h4("CROSS-VALIDATION : ECHANTILLONNAGE 1",
                               style="color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, h4("Simulation Sur La Période De Calage",
                              style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, h4("Simulation Sur La Période De Validation",
                              style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, verbatimTextOutput(ns("cross_validation_1_1"))),
                 column(6, verbatimTextOutput(ns("cross_validation_1_2")))
        )
      })

      # résultats simulation sur la période de calibration 1
      output$cross_validation_1_1 <- renderPrint({
        req(periode_cross1_1(), cross_validation_param_result1(), cross_validation_cal_result1())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross1_1()[1]), "\n",
          "\tFin::", as.character(periode_cross1_1()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_param_result1()[13], "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "FC::::", cross_validation_param_result1()[1],   " | beta::::", cross_validation_param_result1()[2],  "\n",
          "LP::::", cross_validation_param_result1()[3],   " | SFCF::::", cross_validation_param_result1()[4],  "\n",
          "TT::::", cross_validation_param_result1()[5],   " | CFMAX::::", cross_validation_param_result1()[6],  "\n",
          "k0::::", cross_validation_param_result1()[7],   " | k1::::", cross_validation_param_result1()[8],  "\n",
          "k2::::", cross_validation_param_result1()[9],   " | UZL::::", cross_validation_param_result1()[10], "\n",
          "PERC::::", cross_validation_param_result1()[11],  " | MAXBAS::::", cross_validation_param_result1()[12], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tKGE:::", cross_validation_cal_result1()[1], "\n",
          "\tNSE:::", cross_validation_cal_result1()[2], "\n",
          "\tRMSE:::", cross_validation_cal_result1()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # résultats simulation sur la période de calibration 1
      output$cross_validation_1_2 <- renderPrint({
        req(periode_cross1_2(), cross_validation_param_result1(), cross_validation_val_result1())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross1_2()[1]), "\n",
          "\tFin::", as.character(periode_cross1_2()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_param_result1()[13], "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "FC::::", cross_validation_param_result1()[1],   " | beta::::", cross_validation_param_result1()[2],  "\n",
          "LP::::", cross_validation_param_result1()[3],   " | SFCF::::", cross_validation_param_result1()[4],  "\n",
          "TT::::", cross_validation_param_result1()[5],   " | CFMAX::::", cross_validation_param_result1()[6],  "\n",
          "k0::::", cross_validation_param_result1()[7],   " | k1::::", cross_validation_param_result1()[8],  "\n",
          "k2::::", cross_validation_param_result1()[9],   " | UZL::::", cross_validation_param_result1()[10], "\n",
          "PERC::::", cross_validation_param_result1()[11],  " | MAXBAS::::", cross_validation_param_result1()[12], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tKGE::::", cross_validation_val_result1()[1], "\n",
          "\tNSE:::", cross_validation_val_result1()[2], "\n",
          "\tRMSE:::", cross_validation_val_result1()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # send value for graphs
      calibration_output_1(OutputTrim_calib1)
      validation_output_1(OutputTrim_valid1)

      #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
      #| CROSS VALIDATION / SAMPLING 2

      # PARAMETERS RANGE and generate set
      FC2    <- runif(N, min = 40   , max = 400)  #Max soil moisture storage, field capacity
      beta2  <- runif(N, min = 1    , max = 6)    #Shape coefficient governing fate of water input to soil moisture storage
      LP2    <- runif(N, min = 0.3   , max = 1)    #Threshold for reduction of evap
      SFCF2  <- runif(N, min = 0.4  , max = 1.2)  #Snowfall correction factor
      TT2    <- runif(N, min = -1.5 , max = 1.2)  #Threshold temperature
      CFMAX2 <- runif(N, min = 1    , max = 8)    #Degree-day factor
      k02    <- runif(N, min = 0.05 , max = 0.5)  #Recession constant (upper storage, near surface)
      k12    <- runif(N, min = 0.01 , max = 0.3)  #Recession constant (upper storage)
      k22    <- runif(N, min = 0.001, max = 0.15) #Recession constant (lower storage)
      UZL2   <- runif(N, min = 0    , max = 70)   #Threshold for shallow storage
      PERC2  <- runif(N, min = 0    , max = 4)    #Percolation, max flow from upper to lower storage
      MAXBAS2<- rep(1, N)   #base of the triangular routing function, days
      #MAXBAS is just 1's because routing will be set to zero, so the parameter isn't used

      KGE2 <- rep(NA, N) # create NSE column, to be filled in for loop

      routage <- 0

      pars2 <- cbind(FC2, beta2, LP2, SFCF2, TT2, CFMAX2, k02, k12, k22, UZL2, PERC2, MAXBAS2, KGE2)

      #trim the first 40% (warm up) of Qobs off for NSE calculation
      EvalStart2_1 <- floor(length(dataObsSecond()$Qobs) * (options_de_mise_en_route$nbWarmUpYear()/100))
      EvalEnd2_1 <- length(dataObsSecond()$Qobs)

      # Monte Carlo simulation
      shiny::withProgress(message = 'Cross-Validation [Sample 2] : Méthode Monte Carlo', value = 0, {
        for (i in 1:N){
          # Increment the progress bar, and update the detail text.
          shiny::incProgress(1/N, detail = paste0("Itération No : ", i))
          # call model with i parameter set generated above P, Temp, PET, routing
          results2 <- HBV(
            pars2[i,1:12], dataObsSecond()$PmmObs, dataObsSecond()$Temp,
            dataObsSecond()$ETP, 0
          )
          #add the Qobs to results
          results2 <- cbind(results2, dataObsSecond())
          #trim the first 40% of the record so it isn't included in the NSE calculation
          results2 <- results2[EvalStart2_1:EvalEnd2_1,]
          #Calculate NSE and add to parameter set
          pars2[i,13]  <- hydroGOF::KGE(results2$q, results2$Qobs)
        }
      })

      # Find the best parameter set
      pars2 <- tibble::as_tibble(pars2)
      bestparams2 <- pars2 %>% dplyr::filter(KGE2 == max(KGE2)) %>%
        dplyr::slice(1) %>%
        as.numeric()
      cross_validation_param_result2(bestparams2)
      cross_validation_param_result2_rank10(
        data.frame(
          pars2 %>% dplyr::arrange(desc(KGE2)) %>% slice(1:5) %>%
            dplyr::mutate(Rank = 1:5) %>% rename(KGE = KGE2)
        )
      )
      #------------------------------------------------------------------------------------|
      #------------------------------------------------------------------------------------|
      # run du modèle sur la période calibration
      # run with best parameters
      modeloutput_calib2 <-  HBV(
        cross_validation_param_result2(), dataObsSecond()$PmmObs, dataObsSecond()$Temp,
        dataObsSecond()$ETP, 0
      )

      #add observations for plotting
      modeloutput_calib2 <- dplyr::bind_cols(modeloutput_calib2, dataObsSecond())
      #trim out warm up period for plotting
      OutputTrim_calib2 <- modeloutput_calib2[EvalStart2_1:EvalEnd2_1,]

      # Calculate evaluation criteria
      cross_validation_cal_result2(c(
        "KGE_calib_period" = hydroGOF::KGE(OutputTrim_calib2$q, OutputTrim_calib2$Qobs),
        "NSE_calib_period" = hydroGOF::NSE(OutputTrim_calib2$q, OutputTrim_calib2$Qobs),
        "RMSE_calib_period" = hydroGOF::rmse(OutputTrim_calib2$q, OutputTrim_calib2$Qobs)
      ))

      # début et fin période 1
      periode_cross2_1(
        c(
          "debut" = options_de_mise_en_route$startValidationDate2(),
          "fin" = options_de_mise_en_route$endValidationDate2()
        )
      )

      #------------------------------------------------------------------------------------|
      # run du modèle sur la période validation
      EvalStart2_2 <- floor(length(dataObsFirst()$Qobs) * (options_de_mise_en_route$nbWarmUpYear()/100))
      EvalEnd2_2 <- length(dataObsFirst()$Qobs)
      #run with best parameters
      modeloutput_valid2 <-  HBV(
        cross_validation_param_result2(), dataObsFirst()$PmmObs, dataObsFirst()$Temp,
        dataObsFirst()$ETP, 0
      )

      #add observations for plotting
      modeloutput_valid2 <- dplyr::bind_cols(modeloutput_valid2, dataObsFirst())
      #trim out warm up period for plotting
      OutputTrim_valid2 <- modeloutput_valid2[EvalStart2_2:EvalEnd2_2,]

      #Calculate evaluation criteria
      cross_validation_val_result2(c(
        "KGE_valib_period" = hydroGOF::KGE(OutputTrim_valid2$q, OutputTrim_valid2$Qobs),
        "NSE_valib_period" = hydroGOF::NSE(OutputTrim_valid2$q, OutputTrim_valid2$Qobs),
        "RMSE_valib_period" = hydroGOF::rmse(OutputTrim_valid2$q, OutputTrim_valid2$Qobs)
      ))

      # début et fin période 1
      periode_cross2_2(
        c(
          "debut" = options_de_mise_en_route$startValidationDate1(),
          "fin" = options_de_mise_en_route$endValidationDate1()
        )
      )

      # affichage des résultats de la cross-validation 2
      output$parameters_and_evaluation_criteria2<- renderUI({
        req(cross_validation_cal_result2())
        fluidRow(align = "left",
                 column(12, h4("CROSS-VALIDATION : ECHANTILLONNAGE 2",
                               style="color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, h4("Simulation Sur La Période De Calage",
                              style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, h4("Simulation Sur La Période De Validation",
                              style="color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
                 )),
                 column(6, verbatimTextOutput(ns("cross_validation_2_1"))),
                 column(6, verbatimTextOutput(ns("cross_validation_2_2")))
        )
      })

      # résultats simulation sur la période de calibration 2
      output$cross_validation_2_1 <- renderPrint({
        req(periode_cross2_1(), cross_validation_param_result2(), cross_validation_cal_result2())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross2_1()[1]), "\n",
          "\tFin::", as.character(periode_cross2_1()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_param_result2()[13], "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "FC::::", cross_validation_param_result2()[1],   " | beta::::", cross_validation_param_result2()[2],  "\n",
          "LP::::", cross_validation_param_result2()[3],   " | SFCF::::", cross_validation_param_result2()[4],  "\n",
          "TT::::", cross_validation_param_result2()[5],   " | CFMAX::::", cross_validation_param_result2()[6],  "\n",
          "k0::::", cross_validation_param_result2()[7],   " | k1::::", cross_validation_param_result2()[8],  "\n",
          "k2::::", cross_validation_param_result2()[9],   " | UZL::::", cross_validation_param_result2()[10], "\n",
          "PERC::::", cross_validation_param_result2()[11],  " | MAXBAS::::", cross_validation_param_result2()[12], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tKGE:::", cross_validation_cal_result2()[1], "\n",
          "\tNSE:::", cross_validation_cal_result2()[2], "\n",
          "\tRMSE:::", cross_validation_cal_result2()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # résultats simulation sur la période de calibration 1
      output$cross_validation_2_2 <- renderPrint({
        req(periode_cross2_2(), cross_validation_param_result2(), cross_validation_val_result2())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross2_2()[1]), "\n",
          "\tFin::", as.character(periode_cross2_2()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_param_result2()[13], "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "FC::::", cross_validation_param_result2()[1],   " | beta::::", cross_validation_param_result2()[2],  "\n",
          "LP::::", cross_validation_param_result2()[3],   " | SFCF::::", cross_validation_param_result2()[4],  "\n",
          "TT::::", cross_validation_param_result2()[5],   " | CFMAX::::", cross_validation_param_result2()[6],  "\n",
          "k0::::", cross_validation_param_result2()[7],   " | k1::::", cross_validation_param_result2()[8],  "\n",
          "k2::::", cross_validation_param_result2()[9],   " | UZL::::", cross_validation_param_result2()[10], "\n",
          "PERC::::", cross_validation_param_result2()[11],  " | MAXBAS::::", cross_validation_param_result2()[12], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tKGE:::", cross_validation_val_result2()[1], "\n",
          "\tNSE:::", cross_validation_val_result2()[2], "\n",
          "\tRMSE:::", cross_validation_val_result2()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # send value for graphs
      calibration_output_2(OutputTrim_calib2)
      validation_output_2(OutputTrim_valid2)

      # setting buttons with shinyjs
      shinyjs::removeClass(id = "calage_validation_animate1", class = "loading dots")
      shinyjs::enable("calage_validation")

      # modal dialog
      showModal(modalDialog(
        tags$h4("CALAGE/VALIDATION CROISEE [CROSS-VALIDATION BY MONTE CARLO METHOD]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot"))),
          # verbatimTextOutput(ns("test")),
          column(12, shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_cross_validation"))))
        ),
        footer=tagList(
          fluidRow(align = "center",
                   column(3, actionButton(
                     ns("calage_plot1"), label=span("MonteCarlo [Sample 1]", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")
                   ),
                   column(3, actionButton(
                     ns("calage_plot2"), label=span("MonteCarlo [Sample 2]", style="font-size:95%;font-family:georgia;"),
                     icon = icon("poll"), class= "btn-info", width = "100%")
                   ),
                   column(3, downloadButton(ns("toExcel"), label="EXCEL", icon = icon("file-excel"), class= "butt", width = "100%")),
                   column(3, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      # render default graph
      output$plot_cross_validation <- plotly::renderPlotly({
        req(cross_validation_param_result1_rank10())
        monte_carlo_rank_10(cross_validation_param_result1_rank10(), N)
      })
      output$periode_plot<- renderUI({
        req()
        tags$h5(
          paste0("Période de Calage : [",   as.character(periode_cross1_1()[1]), " --- ",  as.character(periode_cross1_1()[2]), "]"),
          style="color:#3474A7;family:Georgia;text-align:left;"
        )
      })

      #* exporter les résultats de la CALIBRATION : MonteCarlo Run 1
      # cross validation pamaters set 1
      output$toExcel <-  downloadHandler(
        filename = function() {
          paste("Cross-Validation-MonteCarlo-Run1-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(pars %>% dplyr::arrange(desc(KGE)) %>% dplyr::mutate(Rang = 1:nrow(pars)), file)
        }
      )

      # cross validation pamaters set 1
      observeEvent(ignoreNULL=TRUE, ignoreInit=TRUE, input$calage_plot1, {
        req(cross_validation_param_result1_rank10())
        # render default graph
        output$plot_cross_validation <- plotly::renderPlotly({
          req(cross_validation_param_result1_rank10())
          monte_carlo_rank_10(cross_validation_param_result1_rank10(), N)
        })
        output$periode_plot<- renderUI({
          req()
          tags$h5(
            paste0("Période de Calage : [",   as.character(periode_cross1_1()[1]), " --- ",  as.character(periode_cross1_1()[2]), "]"),
            style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        #* exporter les résultats de la CALIBRATION : MonteCarlo Run 2
        # cross validation pamaters set 1
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation-MonteCarlo-Run1-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(pars %>% dplyr::arrange(desc(KGE)) %>% dplyr::mutate(Rang = 1:nrow(pars)), file)
          }
        )

      })

      # cross validation pamaters set 2
      observeEvent(ignoreNULL=TRUE, ignoreInit=TRUE, input$calage_plot2, {
        req(cross_validation_param_result2_rank10())
        # render default graph
        output$plot_cross_validation <- plotly::renderPlotly({
          req(cross_validation_param_result2_rank10())
          monte_carlo_rank_10(cross_validation_param_result2_rank10(), N)
        })
        output$periode_plot<- renderUI({
          req()
          tags$h5(
            paste0("Période de Calage : [",   as.character(periode_cross2_1()[1]), " --- ",  as.character(periode_cross2_1()[2]), "]"),
            style="color:#3474A7;family:Georgia;text-align:left;"
          )
        })

        #* exporter les résultats de la CALIBRATION : MonteCarlo Run 2
        # cross validation pamaters set 2
        output$toExcel <-  downloadHandler(
          filename = function() {
            paste("Cross-Validation-MonteCarlo-Run2-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
          },
          content = function(file) {
            writexl::write_xlsx(
              pars2 %>% dplyr::rename(KGE = KGE2) %>% dplyr::arrange(desc(KGE)) %>% dplyr::mutate(Rang = 1:nrow(pars)), file
            )
          }
        )
      })

      # choix des paramètres de la simulation
      # transfert des paramètres
      if(cross_validation_param_result2()[13] > cross_validation_param_result1()[13]){
        cross_validation_best_values(cross_validation_param_result2()[-13])
      }else{
        cross_validation_best_values(cross_validation_param_result1()[-13])
      }

    })

    output$test<- renderPrint({
      req(cross_validation_best_values())
    })
    #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    #| SIMULATION
    #| run du modèle sur toute la serie
    observeEvent(ignoreNULL=TRUE, ignoreInit=TRUE, input$simulation, {
      req(dataOverAll(), parametres_du_modele)

      # setting buttons with shinyjs
      shinyjs::addClass(id = "simulation", class = "loading dots")
      shinyjs::disable("simulation_animate")

      #------------------------------------------------------------------------------------|
      # run du modèle sur la période validation
      simulationStart <- floor(length(dataOverAll()$Qobs) * (options_de_mise_en_route$nbWarmUpYear()/100))
      simulationEvalEnd <- length(dataOverAll()$Qobs)
      #run with best parameters
      modeloutput_simulation <-  HBV(
        cross_validation_best_values(), dataOverAll()$PmmObs, dataOverAll()$Temp, dataOverAll()$ETP, 0
      )

      #add observations for plotting
      modeloutput_simulation <- dplyr::bind_cols(modeloutput_simulation, dataOverAll())
      #trim out warm up period for plotting
      OutputTrim_simulation <- modeloutput_simulation[simulationStart:simulationEvalEnd,]

      #Calculate evaluation criteria
      simulation_evaluation_result(c(
        "KGE_valib_period" = hydroGOF::KGE(OutputTrim_simulation$q, OutputTrim_simulation$Qobs),
        "NSE_valib_period" = hydroGOF::NSE(OutputTrim_simulation$q, OutputTrim_simulation$Qobs),
        "RMSE_valib_period" = hydroGOF::rmse(OutputTrim_simulation$q, OutputTrim_simulation$Qobs)
      ))

      # début et fin période simulation
      periode_simulation(
        c(
          "debut" = min(dataOverAll()$date, na.rm = T),
          "fin" = max(dataOverAll()$date, na.rm = T)
        )
      )

      # setting buttons with shinyjs
      shinyjs::removeClass(id = "simulation", class = "loading dots")
      shinyjs::enable("simulation_animate")

      # show result
      # modal dialog
      showModal(modalDialog(
        tags$h4("SIMULATION SUR TOUTE LA SERIE", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, uiOutput(ns("periode_plot"))),
          # verbatimTextOutput(ns("test")),
          column(12, shinycssloaders::withSpinner(verbatimTextOutput(ns("simulation_eval_result_verbose"))))
        ),
        footer=tagList(
          modalButton('Fermer', icon = icon("power-off"))
        ),
        size = "m"
      ))

      # evaluation criteria
      output$simulation_eval_result_verbose<- renderPrint({
        req(periode_simulation(), parametres_du_modele(), simulation_evaluation_result())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_simulation()[1]), "\n",
          "\tFin::", as.character(periode_simulation()[2]), "\n",
          "..........................................\n",
          "..........................................\n",
          "PARAMETRES UTILISES::\n",
          "FC::::", parametres_du_modele()[1],   " | beta::::", parametres_du_modele()[2],  "\n",
          "LP::::", parametres_du_modele()[3],   " | SFCF::::", parametres_du_modele()[4],  "\n",
          "TT::::", parametres_du_modele()[5],   " | CFMAX::::", parametres_du_modele()[6],  "\n",
          "k0::::", parametres_du_modele()[7],   " | k1::::", parametres_du_modele()[8],  "\n",
          "k2::::", parametres_du_modele()[9],   " | UZL::::", parametres_du_modele()[10], "\n",
          "PERC::::", parametres_du_modele()[11],  " | MAXBAS::::", parametres_du_modele()[12], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION DE LA SIMULATION : \n",
          "\tKGE:::", simulation_evaluation_result()[1], "\n",
          "\tNSE:::", simulation_evaluation_result()[2], "\n",
          "\tRMSE:::", simulation_evaluation_result()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # send result
      simulation_output(OutputTrim_simulation)

    })


    return(
      list(
        # cross_validation 1
        cross_validation_period_1_1 = reactive({ periode_cross1_1() }),
        cross_validation_period_1_2 = reactive({ periode_cross1_2() }),
        calibration_1 = reactive({ calibration_output_1() }),
        validation_1 = reactive({ validation_output_1() }),
        #----------------------------------------------------------------------|
        # cross_validation 2
        cross_validation_period_2_1 = reactive({ periode_cross2_1() }),
        cross_validation_period_2_2 = reactive({ periode_cross2_2() }),
        calibration_2 = reactive({ calibration_output_2() }),
        validation_2 = reactive({ validation_output_2() }),
        #----------------------------------------------------------------------|
        # simulation
        cross_valid_best_params_values = reactive ({ cross_validation_best_values() }),
        simulation_period = reactive({ periode_simulation() }),
        simulation_output_result = reactive({ simulation_output() })
      )
    )

  })
}

## To be copied in the UI
# mod_calibration_HBV_ui("calibration_HBV_1")

## To be copied in the server
# mod_calibration_HBV_server("calibration_HBV_1")
