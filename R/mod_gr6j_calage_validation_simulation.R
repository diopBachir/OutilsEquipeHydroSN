#' gr6j_calage_validation_simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gr6j_calage_validation_simulation_ui <- function(id){
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

    verbatimTextOutput(ns("test")),

    fluidRow(
      column(12, uiOutput(ns("parameters_and_evaluation_criteria"))),
      column(12, uiOutput(ns("parameters_and_evaluation_criteria2")))
    )
  )
}

#' gr6j_calage_validation_simulation Server Functions
#'
#' @noRd
mod_gr6j_calage_validation_simulation_server <- function(id, donnees, options_de_mise_en_route, parametres_du_modele){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## DECOUPAGE DE LA SERIE EN DEUX POUR LA VALIDATION CROISEE

    # première période
    dataObsFirst<- reactive({
      req(donnees)
      donnees %>%
        dplyr::filter(
          (date >= options_de_mise_en_route$startValidationDate1()) & (date <= options_de_mise_en_route$endValidationDate1())
        )
    })

    # seconde periode
    dataObsSecond<-  reactive({
      req(donnees)
      donnees %>%
        dplyr::filter(
          (date >= options_de_mise_en_route$startValidationDate2()) & (date <= options_de_mise_en_route$endValidationDate2())
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
    # best parameters values
    cross_validation_best_values<- reactiveVal()
    # résultats de la simulation
    simulation_output<- reactiveVal()

    ### calage validation croisée
    #* stockage du résultat
    cross_validation_critere_result1<- reactiveVal()
    #---------------------------------------------
    cross_validation_param_result1<- reactiveVal()
    cross_validation_cal_result1<- reactiveVal()
    cross_validation_val_result1<- reactiveVal()
    periode_cross1_1<- reactiveVal()
    periode_cross1_2<- reactiveVal()
    results_cross_validation1<- reactiveVal()
    #---------------------------------------------
    cross_validation_critere_result2<- reactiveVal()
    #---------------------------------------------
    cross_validation_param_result2<- reactiveVal()
    cross_validation_cal_result2<- reactiveVal()
    cross_validation_val_result2<- reactiveVal()
    periode_cross2_1<- reactiveVal()
    periode_cross2_2<- reactiveVal()
    results_cross_validation2<- reactiveVal()
    #---------------------------------------------
    #---------------------------------------------
    simulation_param<- reactiveVal()
    simulation_evaluation_result<- reactiveVal()
    periode_simulation<- reactiveVal()

    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$calage_validation, {
      shiny::req(dataObsFirst(), dataObsSecond())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "calage_validation_animate1", class = "loading dots")
      shinyjs::disable("calage_validation")

      #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      ### CALIBRATION VALIDATION 1 : Sur La Première Division Des données

      #### Calibration --------------------------------------------------------------#
      #------------------------------------------------------------------------------#
      ###* InputsModel object

      #* Pour exécuter un modèle hydrologique GR ou CemaNeige, l'utilisateur doit
      #* préparer les données d'entrée avec la fonction CreateInputsModel(). Comme
      #* arguments, cette fonction a besoin du nom de la fonction correspondant au
      #* modèle que l'utilisateur souhaite exécuter, d'un vecteur de dates, d'un
      #* vecteur de précipitations et d'un vecteur d'évapotranspiration potentielle.
      InputsModel_cal_cross1 <- airGR::CreateInputsModel(
        FUN_MOD = airGR::RunModel_GR6J,        # modèle GR
        DatesR = dataObsFirst()$date,         # vecteur Date
        Precip = dataObsFirst()$PmmObs,          # vecteur Précipitations
        PotEvap = dataObsFirst()$ETP          # vecteur évapotranspiration potentielle
      )

      #------------------------------------------------------------------------------#
      ###* RUnOptions Object

      #* La fonction CreateRunOptions() permet de préparer les options nécessaires aux
      #* fonctions RunModel*(), qui sont les fonctions réelles des modèles.

      #* L'utilisateur doit au moins définir les arguments suivants :
      #*********** FUN_MOD : the name of the model function to run
      #*********** InputsModel : the associated input data
      #*********** IndPeriod_Run : the period on which the model is run

      #* L'initialisation des modèles hydrologiques est de la plus haute importance.
      #* En effet, une initialisation imprécise entraîne des simulations de décharge
      #* de mauvaise qualité pendant les premières étapes de la période de
      #* fonctionnement. Par exemple, dans les modèles GR, par défaut, les niveaux
      #* des réservoirs de production et de routage sont respectivement fixés à 30%
      #* et 50% de leur capacité, ce qui peut être loin de leur valeur idéale. Deux
      #* solutions sont proposées pour initialiser avec précision les modèles GR dans
      #* airGR : prédéfinir manuellement les états initiaux (par exemple à partir
      #* d'une exécution précédente) ou exécuter les modèles pendant une période de
      #* préchauffage avant la période d'exécution réelle. Il est généralement
      #* conseillé de paramétrer cette période de préchauffage = ou > 1 an.
      ind_WarmUp_cal_cross1 <- seq(
        which(dataObsFirst()$date == min(dataObsFirst()$date, na.rm = TRUE)),
        which(dataObsFirst()$date == dataObsFirst()$date[1]+(lubridate::days(options_de_mise_en_route$nbWarmUpYear())-lubridate::days(1)))
      )

      #* Pour sélectionner une période pour laquelle l'utilisateur souhaite exécuter
      #* le modèle, sélectionnez les index correspondants pour différentes périodes
      #* de temps (pas les dates POSIXt), comme suit :
      ind_Run_cal_cross1 <- seq(
        max(ind_WarmUp_cal_cross1) + 1,
        which(dataObsFirst()$date == max(dataObsFirst()$date, na.rm = TRUE))
      )

      #* As a consequence, it is possible to define in CreateRunOptions() the
      #* following arguments:
      RunOptions_cal_cross1 <- airGR::CreateRunOptions(
        #* Le modèle à utiliser
        FUN_MOD = airGR::RunModel_GR6J,
        #* Les entrées à considérées (d'ores et déjà définies avec la fonction
        #* CreateInputsModel() et stockées dans l'objet InputsModel ;
        InputsModel = InputsModel_cal_cross1,
        #* Les indices des pas de temps à considérer pour l'initialisation des
        #* réservoirs du modèle (déjà définis et stockés dans l'objet ind_WarmUp_CAL)
        IndPeriod_WarmUp = ind_WarmUp_cal_cross1,
        #* Les indices des pas de temps à considérer pour le calage du modèle (déjà
        #* définis et stockés dans l'objet ind_Run_CAL
        IndPeriod_Run = ind_Run_cal_cross1
      )

      #------------------------------------------------------------------------------#
      ###* InputsCrit object

      #* La fonction CreateInputsCrit() permet de rassembler les données et
      #* informations nécessaires pour le calcul du critère de calage, avec notamment
      if(options_de_mise_en_route$calibrationType() == "Fonction Objective Unique [KGE[Q]]"){
        InputsCrit_cal_cross1<- airGR::CreateInputsCrit(
          # the name of the error criterion function :
          #* ErrorCrit_RMSE(): Root mean square error (RMSE)
          #* ErrorCrit_NSE(): Nash-Sutcliffe model efficiency coefficient (NSE)
          #* ErrorCrit_KGE(): Kling-Gupta efficiency criterion (KGE)
          #* ErrorCrit_KGE2(): modified Kling-Gupta efficiency criterion (KGE’)
          FUN_CRIT = airGR::ErrorCrit_KGE,
          #* the inputs of the hydrological model previously prepared by the
          #* CreateInputsModel() function
          InputsModel = InputsModel_cal_cross1,
          #* the options of the hydrological model previously prepared by the
          #* CreateRunOptions() function
          RunOptions = RunOptions_cal_cross1,
          # the name of the considered variable (by default "Q" for the discharge)
          VarObs = "Q",
          # observed variable time serie (e.g. the discharge expressed in mm/time step)
          Obs = dataObsFirst()$Qobs[ind_Run_cal_cross1]
        )
      }else{
        ## Definir un critere composite
        InputsCrit_cal_cross1<- airGR::CreateInputsCrit(
          FUN_CRIT = list(airGR::ErrorCrit_KGE, airGR::ErrorCrit_NSE),
          InputsModel = InputsModel_cal_cross1,
          RunOptions = RunOptions_cal_cross1,
          Obs = list(dataObsFirst()$Qobs[ind_Run_cal_cross1], dataObsFirst()$Qobs[ind_Run_cal_cross1]),
          VarObs = list("Q", "Q"), transfo = list("", "sqrt"),
          Weights = list(0.5, 0.5)
        )
      }

      #------------------------------------------------------------------------------#
      ###* CalibOptions object

      #* Avant d'utiliser l'outil de calibration automatique, l'utilisateur doit
      #* préparer les options de calibrage avec la fonction CreateCalibOptions().
      CalibOptions_cal_cross1 <- airGR::CreateCalibOptions(
        FUN_MOD = airGR::RunModel_GR6J,        # the name of the model function
        FUN_CALIB = airGR::Calibration_Michel  # the name of the calibration algorithm
      )

      #------------------------------------------------------------------------------#
      ###* Calibration

      OutputsCalib_cross1 <- airGR::Calibration_Michel(
        #* the inputs of the hydrological model previously prepared by the
        #* CreateInputsModel() function
        InputsModel = InputsModel_cal_cross1,
        #* the options of the hydrological model previously prepared by the
        #* CreateRunOptions() function
        RunOptions = RunOptions_cal_cross1,
        #* the options of the hydrological model previously prepared by the
        #* CreateInputsCrit() function
        InputsCrit = InputsCrit_cal_cross1,
        #* the options of the hydrological calibration model previously prepared by
        #* the CreateCalibOptions() function
        CalibOptions = CalibOptions_cal_cross1,
        #* Le modèle à utiliser
        FUN_MOD = airGR::RunModel_GR6J
      )

      #* The Calibration_Michel() function returns a vector with the parameters of
      #* the chosen model, which means that the number of values can differ depending
      #* on the model that is used. It is possible to use the Calibration_Michel()
      #* function with user-implemented hydrological models.
      ####* Stockage de la valeur des paramètres et du critère de calage
      #* paramètres
      param_cal_cross1 <- OutputsCalib_cross1$ParamFinalR
      cross_validation_param_result1(OutputsCalib_cross1$ParamFinalR)
      #* critères
      cross_validation_critere_result1(OutputsCalib_cross1$CritFinal)

      #------------------------------------------------------------------------------#
      ###* Run du modèle sur la période de calibration
      OutputsModel_cal_cross1 <- airGR::RunModel_GR6J(
        InputsModel = InputsModel_cal_cross1, RunOptions = RunOptions_cal_cross1, Param = param_cal_cross1
      )

      #------------------------------------------------------------------------------#
      ## Critères d'evaluation sur la periode de validation : RMSE, NSE, KGE
      InputsCritMulti_cal_cross1 <- airGR::CreateInputsCrit(
        FUN_CRIT = list(airGR::ErrorCrit_RMSE, airGR::ErrorCrit_NSE, airGR::ErrorCrit_KGE),
        InputsModel = InputsModel_cal_cross1, RunOptions = RunOptions_cal_cross1,
        Obs = list(dataObsFirst()$Qobs[ind_Run_cal_cross1],
                   dataObsFirst()$Qobs[ind_Run_cal_cross1],
                   dataObsFirst()$Qobs[ind_Run_cal_cross1]),
        VarObs = list("Q", "Q","Q"), transfo = list("", "sqrt","sqrt"),
        Weights = NULL
      )

      #### Validation --------------------------------------------------------------#
      #------------------------------------------------------------------------------#

      #------------------------------------------------------------------------------#
      ###* InputsModel object
      InputsModel_val_cross1 <-  CreateInputsModel(
        FUN_MOD = airGR::RunModel_GR6J, DatesR = dataObsSecond()$date,
        Precip = dataObsSecond()$PmmObs, PotEvap = dataObsSecond()$ETP
      )

      #------------------------------------------------------------------------------#
      ###* RUnOptions Object
      #* mise en route
      ind_WarmUp_val_cross1 <- seq(
        which(dataObsSecond()$date == min(dataObsSecond()$date, na.rm = TRUE)),
        which(dataObsSecond()$date == dataObsSecond()$date[1]+(lubridate::days(options_de_mise_en_route$nbWarmUpYear())-lubridate::days(1)))
      )
      #* période d'exécution
      ind_Run_val_cross1 <- seq(
        max(ind_WarmUp_val_cross1) + 1,
        which(dataObsSecond()$date == max(dataObsSecond()$date, na.rm = TRUE))
      )

      #* création des options d'éxécution
      RunOptions_val_cross1 <- airGR::CreateRunOptions(
        FUN_MOD = airGR::RunModel_GR6J, InputsModel = InputsModel_val_cross1,
        IndPeriod_WarmUp = ind_WarmUp_val_cross1, IndPeriod_Run = ind_Run_val_cross1
      )

      #------------------------------------------------------------------------------#
      ###* Run du modèle sur la période de validation
      OutputsModel_val_cross1 <-  airGR::RunModel_GR6J(
        InputsModel = InputsModel_val_cross1, RunOptions = RunOptions_val_cross1, Param = param_cal_cross1
      )

      #------------------------------------------------------------------------------#
      ## Critères d'evaluation sur la periode de validation : RMSE, NSE, KGE
      InputsCritMulti_val_cross1 <- airGR::CreateInputsCrit(
        FUN_CRIT = list(airGR::ErrorCrit_RMSE, airGR::ErrorCrit_NSE, airGR::ErrorCrit_KGE),
        InputsModel = InputsModel_val_cross1, RunOptions = RunOptions_val_cross1,
        Obs = list(dataObsSecond()$Qobs[ind_Run_val_cross1],
                   dataObsSecond()$Qobs[ind_Run_val_cross1],
                   dataObsSecond()$Qobs[ind_Run_val_cross1]),
        VarObs = list("Q", "Q","Q"), transfo = list("", "sqrt","sqrt"),
        Weights = NULL
      )

      #------------------------------------------------------------------------------#
      ## Critère d'évaluation

      #* période de calage
      ErrorCritMulti_cal_cross1 <- airGR::ErrorCrit(
        InputsCrit = InputsCritMulti_cal_cross1, OutputsModel = OutputsModel_cal_cross1
      )

      #* période de validation
      ErrorCritMulti_val_cross1 <- airGR::ErrorCrit(
        InputsCrit = InputsCritMulti_val_cross1, OutputsModel = OutputsModel_val_cross1
      )

      # transfert des résultats
      # Critères d'evaluation sur la periode de calage : RMSE, NSE, KGE
      cross_validation_cal_result1(
        c(
          "RMSE [Q]" = ErrorCritMulti_cal_cross1[[1]][[1]],
          "NSE[sqrt(Q)]" = ErrorCritMulti_cal_cross1[[2]][[1]],
          "KGE[sqrt(Q)]" = ErrorCritMulti_cal_cross1[[3]][[1]]
        )
      )
      # Critères d'evaluation sur la periode de validation : RMSE, NSE, KGE
      cross_validation_val_result1(
        c(
          "RMSE [Q]" = ErrorCritMulti_val_cross1[[1]][[1]],
          "NSE[sqrt(Q)]" = ErrorCritMulti_val_cross1[[2]][[1]],
          "KGE[sqrt(Q)]" = ErrorCritMulti_val_cross1[[3]][[1]]
        )
      )
      # début et fin période 1
      periode_cross1_1(
        c(
          "debut" = options_de_mise_en_route$startValidationDate1(),
          "fin" = options_de_mise_en_route$endValidationDate1()
        )
      )
      periode_cross1_2(
        c(
          "debut" = options_de_mise_en_route$startValidationDate2(),
          "fin" = options_de_mise_en_route$endValidationDate2()
        )
      )

      # affichage des résultats
      output$parameters_and_evaluation_criteria<- renderUI({
        req(ErrorCritMulti_cal_cross1, ErrorCritMulti_val_cross1)
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

      # résultats simulation sur la période de calibration
      output$cross_validation_1_1 <- renderPrint({
        req(periode_cross1_2(), cross_validation_param_result1(), cross_validation_cal_result1())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross1_1()[1]), "\n",
          "\tFin::", as.character(periode_cross1_1()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_critere_result1(), "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "\tX1::", cross_validation_param_result1()[1], "\n",
          "\tX2::", cross_validation_param_result1()[2], "\n",
          "\tX3::", cross_validation_param_result1()[3], "\n",
          "\tX4::", cross_validation_param_result1()[4], "\n",
          "\tX5::", cross_validation_param_result1()[5], "\n",
          "\tX6::", cross_validation_param_result1()[6], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tRMSE[Q]-------: ", cross_validation_cal_result1()[1], "\n",
          "\tNSE[sqrt(Q)]--: ", cross_validation_cal_result1()[2], "\n",
          "\tKGE[sqrt(Q)]--: ", cross_validation_cal_result1()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # résultats simulation sur la période de validation
      output$cross_validation_1_2 <- renderPrint({
        req(periode_cross1_2(), cross_validation_param_result1(), cross_validation_val_result1())
        # formattage et affichage des résultats de la calibration
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross1_2()[1]), "\n",
          "\tFin::", as.character(periode_cross1_2()[2]), "\n",
          "..........................................\n",
          "..........................................\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "\tX1::", cross_validation_param_result1()[1], "\n",
          "\tX2::", cross_validation_param_result1()[2], "\n",
          "\tX3::", cross_validation_param_result1()[3], "\n",
          "\tX4::", cross_validation_param_result1()[4], "\n",
          "\tX5::", cross_validation_param_result1()[5], "\n",
          "\tX6::", cross_validation_param_result1()[6], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tRMSE[Q]-------: ", cross_validation_val_result1()[1], "\n",
          "\tNSE[sqrt(Q)]--: ", cross_validation_val_result1()[2], "\n",
          "\tKGE[sqrt(Q)]--: ", cross_validation_val_result1()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      ### CALIBRATION VALIDATION 2 : Sur La Seconde Division Des données

      #### Calibration --------------------------------------------------------------#
      #------------------------------------------------------------------------------#
      ###* InputsModel object

      #* Pour exécuter un modèle hydrologique GR ou CemaNeige, l'utilisateur doit
      #* préparer les données d'entrée avec la fonction CreateInputsModel(). Comme
      #* arguments, cette fonction a besoin du nom de la fonction correspondant au
      #* modèle que l'utilisateur souhaite exécuter, d'un vecteur de dates, d'un
      #* vecteur de précipitations et d'un vecteur d'évapotranspiration potentielle.
      InputsModel_cal_cross2 <- airGR::CreateInputsModel(
        FUN_MOD = airGR::RunModel_GR6J,        # modèle GR
        DatesR = dataObsSecond()$date,         # vecteur Date
        Precip = dataObsSecond()$PmmObs,       # vecteur Précipitations
        PotEvap = dataObsSecond()$ETP          # vecteur évapotranspiration potentielle
      )

      #------------------------------------------------------------------------------#
      ###* RUnOptions Object

      #* La fonction CreateRunOptions() permet de préparer les options nécessaires aux
      #* fonctions RunModel*(), qui sont les fonctions réelles des modèles.

      #* L'utilisateur doit au moins définir les arguments suivants :
      #*********** FUN_MOD : the name of the model function to run
      #*********** InputsModel : the associated input data
      #*********** IndPeriod_Run : the period on which the model is run

      #* L'initialisation des modèles hydrologiques est de la plus haute importance.
      #* En effet, une initialisation imprécise entraîne des simulations de décharge
      #* de mauvaise qualité pendant les premières étapes de la période de
      #* fonctionnement. Par exemple, dans les modèles GR, par défaut, les niveaux
      #* des réservoirs de production et de routage sont respectivement fixés à 30%
      #* et 50% de leur capacité, ce qui peut être loin de leur valeur idéale. Deux
      #* solutions sont proposées pour initialiser avec précision les modèles GR dans
      #* airGR : prédéfinir manuellement les états initiaux (par exemple à partir
      #* d'une exécution précédente) ou exécuter les modèles pendant une période de
      #* préchauffage avant la période d'exécution réelle. Il est généralement
      #* conseillé de paramétrer cette période de préchauffage = ou > 1 an.
      ind_WarmUp_cal_cross2 <- seq(
        which(dataObsSecond()$date == min(dataObsSecond()$date, na.rm = TRUE)),
        which(dataObsSecond()$date == dataObsSecond()$date[1]+(lubridate::days(options_de_mise_en_route$nbWarmUpYear())-lubridate::days(1)))
      )

      #* Pour sélectionner une période pour laquelle l'utilisateur souhaite exécuter
      #* le modèle, sélectionnez les index correspondants pour différentes périodes
      #* de temps (pas les dates POSIXt), comme suit :
      ind_Run_cal_cross2 <- seq(
        max(ind_WarmUp_cal_cross2) + 1,
        which(dataObsSecond()$date == max(dataObsSecond()$date, na.rm = TRUE))
      )

      #* As a consequence, it is possible to define in CreateRunOptions() the
      #* following arguments:
      RunOptions_cal_cross2 <- airGR::CreateRunOptions(
        #* Le modèle à utiliser
        FUN_MOD = airGR::RunModel_GR6J,
        #* Les entrées à considérées (d'ores et déjà définies avec la fonction
        #* CreateInputsModel() et stockées dans l'objet InputsModel ;
        InputsModel = InputsModel_cal_cross2,
        #* Les indices des pas de temps à considérer pour l'initialisation des
        #* réservoirs du modèle (déjà définis et stockés dans l'objet ind_WarmUp_CAL)
        IndPeriod_WarmUp = ind_WarmUp_cal_cross2,
        #* Les indices des pas de temps à considérer pour le calage du modèle (déjà
        #* définis et stockés dans l'objet ind_Run_CAL
        IndPeriod_Run = ind_Run_cal_cross2
      )

      #------------------------------------------------------------------------------#
      ###* InputsCrit object

      #* La fonction CreateInputsCrit() permet de rassembler les données et
      #* informations nécessaires pour le calcul du critère de calage, avec notamment
      if(options_de_mise_en_route$calibrationType() == "Fonction Objective Unique [KGE[Q]]"){
        InputsCrit_cal_cross2<- airGR::CreateInputsCrit(
          # the name of the error criterion function :
          #* ErrorCrit_RMSE(): Root mean square error (RMSE)
          #* ErrorCrit_NSE(): Nash-Sutcliffe model efficiency coefficient (NSE)
          #* ErrorCrit_KGE(): Kling-Gupta efficiency criterion (KGE)
          #* ErrorCrit_KGE2(): modified Kling-Gupta efficiency criterion (KGE’)
          FUN_CRIT = airGR::ErrorCrit_KGE,
          #* the inputs of the hydrological model previously prepared by the
          #* CreateInputsModel() function
          InputsModel = InputsModel_cal_cross2,
          #* the options of the hydrological model previously prepared by the
          #* CreateRunOptions() function
          RunOptions = RunOptions_cal_cross2,
          # the name of the considered variable (by default "Q" for the discharge)
          VarObs = "Q",
          # observed variable time serie (e.g. the discharge expressed in mm/time step)
          Obs = dataObsSecond()$Qobs[ind_Run_cal_cross2]
        )
      }else{
        ## Definir un critere composite
        InputsCrit_cal_cross2<- airGR::CreateInputsCrit(
          FUN_CRIT = list(airGR::ErrorCrit_KGE, airGR::ErrorCrit_NSE),
          InputsModel = InputsModel_cal_cross2,
          RunOptions = RunOptions_cal_cross2,
          Obs = list(dataObsSecond()$Qobs[ind_Run_cal_cross2], dataObsSecond()$Qobs[ind_Run_cal_cross2]),
          VarObs = list("Q", "Q"), transfo = list("", "sqrt"),
          Weights = list(0.5, 0.5)
        )
      }

      #------------------------------------------------------------------------------#
      ###* CalibOptions object

      #* Avant d'utiliser l'outil de calibration automatique, l'utilisateur doit
      #* préparer les options de calibrage avec la fonction CreateCalibOptions().
      CalibOptions_cal_cross2 <- airGR::CreateCalibOptions(
        FUN_MOD = airGR::RunModel_GR6J,        # the name of the model function
        FUN_CALIB = airGR::Calibration_Michel  # the name of the calibration algorithm
      )

      #------------------------------------------------------------------------------#
      ###* Calibration

      OutputsCalib_cross2 <- airGR::Calibration_Michel(
        #* the inputs of the hydrological model previously prepared by the
        #* CreateInputsModel() function
        InputsModel = InputsModel_cal_cross2,
        #* the options of the hydrological model previously prepared by the
        #* CreateRunOptions() function
        RunOptions = RunOptions_cal_cross2,
        #* the options of the hydrological model previously prepared by the
        #* CreateInputsCrit() function
        InputsCrit = InputsCrit_cal_cross2,
        #* the options of the hydrological calibration model previously prepared by
        #* the CreateCalibOptions() function
        CalibOptions = CalibOptions_cal_cross2,
        #* Le modèle à utiliser
        FUN_MOD = airGR::RunModel_GR6J
      )

      #* The Calibration_Michel() function returns a vector with the parameters of
      #* the chosen model, which means that the number of values can differ depending
      #* on the model that is used. It is possible to use the Calibration_Michel()
      #* function with user-implemented hydrological models.

      ####* Stockage de la valeur des paramètres et du critère de calage
      #* paramètres
      param_cal_cross2 <- OutputsCalib_cross2$ParamFinalR
      cross_validation_param_result2(OutputsCalib_cross2$ParamFinalR)
      #* critères
      cross_validation_critere_result2(OutputsCalib_cross2$CritFinal)

      #------------------------------------------------------------------------------#
      ###* Run du modèle sur la période de calibration
      OutputsModel_cal_cross2 <- airGR::RunModel_GR6J(
        InputsModel = InputsModel_cal_cross2, RunOptions = RunOptions_cal_cross2, Param = param_cal_cross2
      )

      #------------------------------------------------------------------------------#
      ## Critères d'evaluation sur la periode de : RMSE, NSE, KGE
      InputsCritMulti_cal_cross2 <- airGR::CreateInputsCrit(
        FUN_CRIT = list(airGR::ErrorCrit_RMSE, airGR::ErrorCrit_NSE, airGR::ErrorCrit_KGE),
        InputsModel = InputsModel_cal_cross2, RunOptions = RunOptions_cal_cross2,
        Obs = list(dataObsSecond()$Qobs[ind_Run_cal_cross2],
                   dataObsSecond()$Qobs[ind_Run_cal_cross2],
                   dataObsSecond()$Qobs[ind_Run_cal_cross2]),
        VarObs = list("Q", "Q","Q"), transfo = list("", "sqrt","sqrt"),
        Weights = NULL
      )

      #### Validation ---------------------------------------------------------------#
      #------------------------------------------------------------------------------#

      #------------------------------------------------------------------------------#
      ###* InputsModel object
      InputsModel_val_cross2 <-  CreateInputsModel(
        FUN_MOD = airGR::RunModel_GR6J, DatesR = dataObsFirst()$date,
        Precip = dataObsFirst()$PmmObs, PotEvap = dataObsFirst()$ETP
      )

      #------------------------------------------------------------------------------#
      ###* RUnOptions Object
      #* mise en route
      ind_WarmUp_val_cross2 <-  seq(
        which(dataObsFirst()$date == min(dataObsFirst()$date, na.rm = TRUE)),
        which(dataObsFirst()$date == dataObsFirst()$date[1]+(lubridate::days(options_de_mise_en_route$nbWarmUpYear())-lubridate::days(1)))
      )
      #* période d'exécution
      ind_Run_val_cross2 <- seq(
        max(ind_WarmUp_val_cross2) + 1,
        which(dataObsFirst()$date == max(dataObsFirst()$date, na.rm = TRUE))
      )

      #* création des options d'éxécution
      RunOptions_val_cross2 <- airGR::CreateRunOptions(
        FUN_MOD = airGR::RunModel_GR6J, InputsModel = InputsModel_val_cross2,
        IndPeriod_WarmUp = ind_WarmUp_val_cross2, IndPeriod_Run = ind_Run_val_cross2
      )

      #------------------------------------------------------------------------------#
      ###* Run du modèle sur la période de validation
      OutputsModel_val_cross2 <-  airGR::RunModel_GR6J(
        InputsModel = InputsModel_val_cross2, RunOptions = RunOptions_val_cross2, Param = param_cal_cross2
      )

      #------------------------------------------------------------------------------#
      ## Critères d'evaluation : RMSE, NSE, KGE
      InputsCritMulti_val_cross2 <- airGR::CreateInputsCrit(
        FUN_CRIT = list(airGR::ErrorCrit_RMSE, airGR::ErrorCrit_NSE, airGR::ErrorCrit_KGE),
        InputsModel = InputsModel_val_cross2, RunOptions = RunOptions_val_cross2,
        Obs = list(dataObsFirst()$Qobs[ind_Run_val_cross2],
                   dataObsFirst()$Qobs[ind_Run_val_cross2],
                   dataObsFirst()$Qobs[ind_Run_val_cross2]),
        VarObs = list("Q", "Q","Q"), transfo = list("", "sqrt","sqrt"),
        Weights = NULL
      )

      #------------------------------------------------------------------------------#
      ## Critère d'évaluation

      #* période de calage
      ErrorCritMulti_cal_cross2 <- airGR::ErrorCrit(
        InputsCrit = InputsCritMulti_cal_cross2, OutputsModel = OutputsModel_cal_cross2
      )

      #* période de validation
      ErrorCritMulti_val_cross2 <- airGR::ErrorCrit(
        InputsCrit = InputsCritMulti_val_cross2, OutputsModel = OutputsModel_val_cross2
      )

      # transfert des résultats
      # Critères d'evaluation sur la periode de calage : RMSE, NSE, KGE
      cross_validation_cal_result2(
        c(
          "RMSE [Q]" = ErrorCritMulti_cal_cross2[[1]][[1]],
          "NSE[sqrt(Q)]" = ErrorCritMulti_cal_cross2[[2]][[1]],
          "KGE[sqrt(Q)]" = ErrorCritMulti_cal_cross2[[3]][[1]]
        )
      )
      # Critères d'evaluation sur la periode de validation : RMSE, NSE, KGE
      cross_validation_val_result2(
        c(
          "RMSE [Q]" = ErrorCritMulti_val_cross2[[1]][[1]],
          "NSE[sqrt(Q)]" = ErrorCritMulti_val_cross2[[2]][[1]],
          "KGE[sqrt(Q)]" = ErrorCritMulti_val_cross2[[3]][[1]]
        )
      )
      # début et fin période 2
      periode_cross2_1(
        c(
          "debut" = options_de_mise_en_route$startValidationDate2(),
          "fin" = options_de_mise_en_route$endValidationDate2()
        )
      )
      periode_cross2_2(
        c(
          "debut" = options_de_mise_en_route$startValidationDate1(),
          "fin" = options_de_mise_en_route$endValidationDate1()
        )
      )

      # affichage des résultats
      output$parameters_and_evaluation_criteria2<- renderUI({
        req(ErrorCritMulti_cal_cross2, ErrorCritMulti_val_cross2)
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

      # résultats simulation sur la période de calibration
      output$cross_validation_2_1 <- renderPrint({
        req(periode_cross2_1(), cross_validation_param_result2(), cross_validation_cal_result2())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross2_1()[1]), "\n",
          "\tFin::", as.character(periode_cross2_1()[2]), "\n",
          "..........................................\n",
          "CALIBRATION:: ",
          "Crit. De Perf.::", cross_validation_critere_result2(), "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "\tX1::", cross_validation_param_result2()[1], "\n",
          "\tX2::", cross_validation_param_result2()[2], "\n",
          "\tX3::", cross_validation_param_result2()[3], "\n",
          "\tX4::", cross_validation_param_result2()[4], "\n",
          "\tX5::", cross_validation_param_result2()[5], "\n",
          "\tX6::", cross_validation_param_result2()[6], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tRMSE[Q]-------: ", cross_validation_cal_result2()[1], "\n",
          "\tNSE[sqrt(Q)]--: ", cross_validation_cal_result2()[2], "\n",
          "\tKGE[sqrt(Q)]--: ", cross_validation_cal_result2()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # résultats simulation sur la période de validation
      output$cross_validation_2_2 <- renderPrint({
        req(periode_cross2_2(), cross_validation_param_result2(), cross_validation_val_result2())
        # formattage et affichage des résultats de la calibration
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_cross2_2()[1]), "\n",
          "\tFin::", as.character(periode_cross2_2()[2]), "\n",
          "..........................................\n",
          "..........................................\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "\tX1::", cross_validation_param_result2()[1], "\n",
          "\tX2::", cross_validation_param_result2()[2], "\n",
          "\tX3::", cross_validation_param_result2()[3], "\n",
          "\tX4::", cross_validation_param_result2()[4], "\n",
          "\tX5::", cross_validation_param_result2()[5], "\n",
          "\tX6::", cross_validation_param_result2()[6], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tRMSE[Q]-------: ", cross_validation_val_result2()[1], "\n",
          "\tNSE[sqrt(Q)]--: ", cross_validation_val_result2()[2], "\n",
          "\tKGE[sqrt(Q)]--: ", cross_validation_val_result2()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # transfert des paramètres
      if(cross_validation_critere_result2() > cross_validation_critere_result1()){
        cross_validation_best_values(cross_validation_param_result2())
      }else{
        cross_validation_best_values(cross_validation_param_result1())
      }

      # transfert des données
      calibration_output_1(OutputsModel_cal_cross1)
      calibration_output_2(OutputsModel_cal_cross2)
      validation_output_1(OutputsModel_val_cross1)
      validation_output_2(OutputsModel_val_cross2)

      # setting buttons with shinyjs
      shinyjs::removeClass(id = "calage_validation_animate1", class = "loading dots")
      shinyjs::enable("calage_validation")


    })

    #||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    ### SIMULATION SUR TOUTE LA PERIODE
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$simulation, {
      shiny::req(donnees, parametres_du_modele())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "simulation", class = "loading dots")
      shinyjs::disable("simulation_animate")

      #------------------------------------------------------------------------------#
      ###* InputsModel object
      InputsModel_simulation <-  CreateInputsModel(
        FUN_MOD = airGR::RunModel_GR6J, DatesR = donnees$date,
        Precip = donnees$PmmObs, PotEvap = donnees$ETP
      )

      #------------------------------------------------------------------------------#
      ###* RUnOptions Object
      #* mise en route
      ind_WarmUp_simulation <-  seq(
        which(donnees$date == min(donnees$date, na.rm = TRUE)),
        which(donnees$date == donnees$date[1]+(lubridate::days(options_de_mise_en_route$nbWarmUpYear())-lubridate::days(1)))
      )
      #* période d'exécution
      ind_Run_simulation <- seq(
        max(ind_WarmUp_simulation) + 1,
        which(donnees$date == max(donnees$date, na.rm = TRUE))
      )

      #* création des options d'éxécution
      RunOptions_simulation <- airGR::CreateRunOptions(
        FUN_MOD = airGR::RunModel_GR6J, InputsModel = InputsModel_simulation,
        IndPeriod_WarmUp = ind_WarmUp_simulation, IndPeriod_Run = ind_Run_simulation
      )

      #------------------------------------------------------------------------------#
      ###* Run du modèle sur la période de validation
      OutputsModel_simulation <-  airGR::RunModel_GR6J(
        InputsModel = InputsModel_simulation, RunOptions = RunOptions_simulation, Param = parametres_du_modele()
      )

      #------------------------------------------------------------------------------#
      ## Critères d'evaluation sur la periode de validation : RMSE, NSE, KGE
      InputsCritMulti_simulation <- airGR::CreateInputsCrit(
        FUN_CRIT = list(airGR::ErrorCrit_RMSE, airGR::ErrorCrit_NSE, airGR::ErrorCrit_KGE),
        InputsModel = InputsModel_simulation, RunOptions = RunOptions_simulation,
        Obs = list(donnees$Qobs[ind_Run_simulation],
                   donnees$Qobs[ind_Run_simulation],
                   donnees$Qobs[ind_Run_simulation]),
        VarObs = list("Q", "Q","Q"), transfo = list("", "sqrt","sqrt"),
        Weights = NULL
      )

      #------------------------------------------------------------------------------#
      # Critère d'évaluation
      ErrorCritMulti_simulation <- airGR::ErrorCrit(
        InputsCrit = InputsCritMulti_simulation, OutputsModel = OutputsModel_simulation
      )

      # transfert des résultats
      # Critères d'evaluation sur la periode de simulation : RMSE, NSE, KGE
      simulation_evaluation_result(
        c(
          "RMSE [Q]" = ErrorCritMulti_simulation[[1]][[1]],
          "NSE[sqrt(Q)]" = ErrorCritMulti_simulation[[2]][[1]],
          "KGE[sqrt(Q)]" = ErrorCritMulti_simulation[[3]][[1]]
        )
      )

      # paramètre de la simulation
      simulation_param(parametres_du_modele())

      # début et fin période 1
      periode_simulation(
        c(
          "debut" = min(donnees$date, na.rm = T),
          "fin" = max(donnees$date, na.rm = T)
        )
      )

      req(OutputsModel_simulation)

      # modal dialog
      showModal(modalDialog(
        tags$h4("RESULTATS DE LA SIMULATION [OVERALL SERIE]", style="color:#3474A7;family:Georgia;text-align:left;"),
        # affichage des résultats
        fluidRow(
          column(12, verbatimTextOutput(ns("simulation_out")))
        ),
        footer=tagList(
          modalButton('Fermer', icon = icon("power-off"))
        ),
        size = "m"
      ))

      # résultats simulation sur la période de calibration
      output$simulation_out <- renderPrint({
        req(periode_simulation(), simulation_param(), simulation_evaluation_result())
        cat(
          # formattage et affichage des résultats de la calibration
          "PERIODE::\n",
          "\tDébut::", as.character(periode_simulation()[1]), "\n",
          "\tFin::", as.character(periode_simulation()[2]), "\n",
          "..........................................\n",
          "PARAMETRES::\n",
          "\tX1::", simulation_param()[1], "\n",
          "\tX2::", simulation_param()[2], "\n",
          "\tX3::", simulation_param()[3], "\n",
          "\tX4::", simulation_param()[4], "\n",
          "\tX5::", simulation_param()[5], "\n",
          "\tX6::", simulation_param()[6], "\n",
          "..........................................\n",
          "CRITERES D'EVALUATION : \n",
          "\tRMSE[Q]-------: ", simulation_evaluation_result()[1], "\n",
          "\tNSE[sqrt(Q)]--: ", simulation_evaluation_result()[2], "\n",
          "\tKGE[sqrt(Q)]--: ", simulation_evaluation_result()[3], "\n",
          "..........................................",
          sep = ""
        )
      })

      # transfert des données
      simulation_output(OutputsModel_simulation)

      # setting buttons with shinyjs
      shinyjs::removeClass(id = "simulation", class = "loading dots")
      shinyjs::enable("simulation_animate")

    })

    return(
      list(
        cross_valid_best_params = reactive({ cross_validation_best_values() }),
        warm_up_period = reactive({ options_de_mise_en_route$nbWarmUpYear() }),
        #----------------------------------------------------------------------|
        # donnees
        cross_validation_data_first = reactive({
          req(donnees)
          donnees %>%
            dplyr::filter(
              (date >= options_de_mise_en_route$startValidationDate1()) & (date <= options_de_mise_en_route$endValidationDate1())
            )
        }),
        cross_validation_data_second = reactive({
          req(donnees)
          donnees %>%
            dplyr::filter(
              (date >= options_de_mise_en_route$startValidationDate2()) & (date <= options_de_mise_en_route$endValidationDate2())
            )
        }),
        overall_serie_data = reactive({ donnees }),
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
        simulation_period = reactive({ periode_simulation() }),
        simulation_output_result = reactive({ simulation_output() })
      )
    )

  })
}

## To be copied in the UI
# mod_gr6j_calage_validation_simulation_ui("gr6j_calage_validation_simulation_1")

## To be copied in the server
# mod_gr6j_calage_validation_simulation_server("gr6j_calage_validation_simulation_1")
