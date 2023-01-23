#' MatrixFacetsMultivariateBoxplotPlotDataPreparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_ui <- function(id, label){
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
             column(6, actionButtonStyled(ns("showData"), span("ReCharger", id=ns("showDataAnimate")), class= "", type="warning")),
             column(6, actionButtonStyled(ns("recoderVariables"), span("Recoder Noms Variables", id=ns("recoderAnimate")), class= "", type="primary")),
    ),

    tags$hr(style="border-color:gray;"),

    # Noms des variables
    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("OldVarNames"), label = "Noms Originaux Des variables Dans L'axe Des Abscisses",
                      value = "", placeholder = "i.e., oldVar1;oldVar2;oldVar3;etc.",
                      width = "100%"
                    )
             ),
             column(12,
                    textAreaInput(
                      ns("NewVarNames"), label = "Nouveaux Noms Des variables Dans L'axe Des Abscisses",
                      value = "", placeholder = "i.e., newVar1;newVar2;newVar3;etc.",
                      width = "100%"
                    )
             )
    ),
    fluidRow(align = "left",
             column(6, h4(
               "Noms Originaux",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(6, h4(
               "Noms Recodés",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
    ),
    fluidRow(align = "left",
             column(6, verbatimTextOutput(ns("defaultVariableName"))),
             column(6, verbatimTextOutput(ns("recodedVariableName")))
    ),

    tags$hr(style="border-color:gray;"),

    # Noms des facets

    actionButtonStyled(ns("recoderFacetsX"), span("Recoder Noms Facets-X", id=ns("recoderFacetsXAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("OldFacetsXNames"), label = "Noms Originaux Des Facets-X (Colonne {FacetX})",
                      value = "", placeholder = "i.e., oldFacet1;oldFacet2;oldFacet3;etc.",
                      width = "100%"
                    )
             ),
             column(6,
                    textInput(
                      ns("NewFacetsXNames"), label = "Nouveaux Noms Des Facets-X (Colonne {FacetX})",
                      value = "", placeholder = "i.e., newFacet1;newFacet2;newFacet3;etc.",
                      width = "100%"
                    )
             )
    ),
    fluidRow(align = "left",
             column(6, h4(
               "Noms Originaux",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(6, h4(
               "Noms Recodés",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
    ),
    fluidRow(align = "left",
             column(6, verbatimTextOutput(ns("defaultFacetXName"))),
             column(6, verbatimTextOutput(ns("recodedFacetXName")))
    ),

    tags$hr(style="border-color:gray;"),

    # Noms des facets

    actionButtonStyled(ns("recoderFacetsY"), span("Recoder Noms Facets-Y", id=ns("recoderFacetsYAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("OldFacetsYNames"), label = "Noms Originaux Des Facets-Y (Colonne {FacetY})",
                      value = "", placeholder = "i.e., oldFacet1;oldFacet2;oldFacet3;etc.",
                      width = "100%"
                    )
             ),
             column(6,
                    textInput(
                      ns("NewFacetsYNames"), label = "Nouveaux Noms Des Facets-Y (Colonne {FacetY})",
                      value = "", placeholder = "i.e., newFacet1;newFacet2;newFacet3;etc.",
                      width = "100%"
                    )
             )
    ),
    fluidRow(align = "left",
             column(6, h4(
               "Noms Originaux",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(6, h4(
               "Noms Recodés",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
    ),
    fluidRow(align = "left",
             column(6, verbatimTextOutput(ns("defaultFacetYName"))),
             column(6, verbatimTextOutput(ns("recodedFacetYName")))
    ),

    tags$hr(style="border-color:gray;"),

    # Noms des Groupes

    actionButtonStyled(ns("recoderGroup"), span("Recoder Noms Groupes", id=ns("recoderGroupAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("OldGroupNames"), label = "Noms Originaux Des Groupes (Colonne {Groupes})",
                      value = "", placeholder = "i.e., oldGroup1;oldGroup2;oldGroup3;etc.",
                      width = "100%"
                    )
             ),
             column(6,
                    textInput(
                      ns("NewGroupNames"), label = "Nouveaux Noms Des Groupes (Colonne {Groupes})",
                      value = "", placeholder = "i.e., newGroup1;newGroup2;newGroup3;etc.",
                      width = "100%"
                    )
             )
    ),
    fluidRow(align = "left",
             column(6, h4(
               "Noms Originaux",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
             column(6, h4(
               "Noms Recodés",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             )),
    ),
    fluidRow(align = "left",
             column(6, verbatimTextOutput(ns("defaultGroupName"))),
             column(6, verbatimTextOutput(ns("recodedGroupName")))
    ),

    tags$hr(style="border-color:gray;"),

    # Levels du factor des variables

    actionButtonStyled(ns("recoderVarLevels"), span("Recoder Levels des Variables", id=ns("recoderVarLevelsAnimate")), class= "", type="primary"),


    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("NewVarFactorLevel"), label = "Nouveaux Levels (Niveaux) du Facteur De L'axe Des Abscisses",
                      value = "", placeholder = "i.e., level1;level2;level3;etc.",
                      width = "100%"
                    )
             )
    ),
    fluidRow(align = "left",
             column(12, h4(
               "Levels Actuels Des Variables [Ordre dans l'axe des Abscisses]",
               style=paste0(
                 "color:#3474A7;text-align:left;font-family:Georgia;background-color:lightgray;"
               )
             ))
    ),
    fluidRow(align = "left",
             column(12, verbatimTextOutput(ns("NewVarFactorLevelVect")))
    ),

    tags$hr(style="border-color:gray;"),

    # Levels du factor des facets

    actionButtonStyled(ns("recoderFacetXLevels"), span("Recoder Levels des Facets-X", id=ns("recoderFacetXLevelsAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("NewFacetsXFactorLevel"), label = "Levels Actuels Des Variables du Facteur Des Facets-X",
                      value = "", placeholder = "i.e., level1;level2;level3;etc.",
                      width = "100%"
                    )
             )
    ),

    fluidRow(align = "left",
             column(12, verbatimTextOutput(ns("NewFacetsXFactorLevelVect")))
    ),

    tags$hr(style="border-color:gray;"),

    # Levels du factor des facets

    actionButtonStyled(ns("recoderFacetYLevels"), span("Recoder Levels des Facets-Y", id=ns("recoderFacetYLevelsAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("NewFacetsYFactorLevel"), label = "Levels Actuels Des Variables du Facteur Des Facets-Y",
                      value = "", placeholder = "i.e., level1;level2;level3;etc.",
                      width = "100%"
                    )
             )
    ),

    fluidRow(align = "left",
             column(12, verbatimTextOutput(ns("NewFacetsYFactorLevelVect")))
    ),

    tags$hr(style="border-color:gray;"),

    # Levels du factor des Groupes

    actionButtonStyled(ns("recoderGroupLevels"), span("Recoder Niveaux des Groups", id=ns("recoderGroupLevelsAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("NewGroupFactorLevel"), label = "Levels Actuels du Facteur Des Groupes (Colonne {Group})",
                      value = "", placeholder = "i.e., level1;level2;level3;etc.",
                      width = "100%"
                    )
             )
    ),

    fluidRow(align = "left",
             column(12, verbatimTextOutput(ns("NewGroupFactorLevelVect")))
    ),
    br(),br()
  )
}

#' MatrixFacetsMultivariateBoxplotPlotDataPreparation Server Functions
#'
#' @noRd
mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # initatizing result
    result_recode <- reactiveVal()

    event_trigger <- reactive({ list(data, input$showData) })
    # processing
    observeEvent(event_trigger(), {
      req(data)

      # setting buttons with shinyjs
      shinyjs::addClass(id = "showDataAnimate", class = "loading dots")
      shinyjs::disable("showData")

      # variable Names
      output$defaultVariableName<- renderPrint({
        req(data)
        unique(data$Variable)
      })
      output$recodedVariableName<- renderPrint({
        req(data)
        unique(data$Variable)
      })

      # FacetX Names
      output$defaultFacetXName<- renderPrint({
        req(data)
        unique(data$FacetX)
      })
      output$recodedFacetXName<- renderPrint({
        req(data)
        unique(data$FacetX)
      })

      # FacetY Names
      output$defaultFacetYName<- renderPrint({
        req(data)
        unique(data$FacetY)
      })
      output$recodedFacetYName<- renderPrint({
        req(data)
        unique(data$FacetY)
      })

      # Group Names
      output$defaultGroupName<- renderPrint({
        req(data)
        unique(data$Group)
      })
      output$recodedGroupName<- renderPrint({
        req(data)
        unique(data$Group)
      })

      # Variable Levels
      output$OldVarFactorLevelVect<- renderPrint({
        req(data)
        levels(data$Variable)
      })
      output$NewVarFactorLevelVect<- renderPrint({
        req(data)
        levels(data$Variable)
      })

      # Facet Levels
      # output$OldFacetsXFactorLevelVect<- renderPrint({
      #   req(data)
      #   levels(data$FacetX)
      # })
      output$NewFacetsXFactorLevelVect<- renderPrint({
        req(data)
        levels(data$FacetX)
      })

      # output$OldFacetsYFactorLevelVect<- renderPrint({
      #   req(data)
      #   levels(data$FacetY)
      # })
      output$NewFacetsYFactorLevelVect<- renderPrint({
        req(data)
        levels(data$FacetY)
      })


      # Group levels
      output$NewGroupFactorLevelVect<- renderPrint({
        req(data)
        levels(data$Group)
      })

      # Button settings
      shinyjs::enable("showData")
      shinyjs::removeClass(id = "showDataAnimate", class = "loading dots")

      result_recode(data)
    })

    #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
    # gestion des entrées vecteurs
    extract_variables_entries<-  function(text){
      # text<-  gsub(" ", "", text)
      split<-  strsplit(text, ";", fixed = FALSE)[[1]]
      split
    }

    # # ------------------------------------------------------------------------------------------------------#
    # Renommer les noms de variables
    observeEvent(input$recoderVariables, {

      req(result_recode())

      if(input$OldVarNames!="" & input$NewVarNames!=""){
        original_values<- extract_variables_entries(input$OldVarNames)
        new_values<- extract_variables_entries(input$NewVarNames)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("OldVarNames")
          shinyFeedback::hideFeedback("NewVarNames")

          donnees<- data.frame(result_recode())

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% unique(donnees$Variable))
          shinyFeedback::feedbackWarning("OldVarNames", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Noms Originaux Des variables Dans L'axe Des Abscisses } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Variable<-  plyr::mapvalues(donnees$Variable, original_values, new_values)
          result_recode(donnees)

          # New variable Names
          output$recodedVariableName<- renderPrint({
            req(result_recode())
            unique(result_recode()$Variable)
          })

          # New Facet Names
          output$recodedFacetXName<- renderPrint({
            req(result_recode())
            unique(result_recode()$FacetX)
          })
          output$recodedFacetYName<- renderPrint({
            req(result_recode())
            unique(result_recode()$FacetY)
          })

          # Variable Levels
          output$OldVarFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Variable)
          })

          # New Variable Levels
          output$NewVarFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Variable)
          })

          # New Facet Levels
          output$NewFacetsFactorXLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetX)
          })
          output$NewFacetsYFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetY)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("OldVarNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("NewVarNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { unique(column{{Variables}}) } !")
          )

        }
      }

    })

    # # ------------------------------------------------------------------------------------------------------#
    # Renommer les noms de Facets-X
    observeEvent(input$recoderFacetsX, {

      req(result_recode())

      if(input$OldFacetsXNames!="" & input$NewFacetsXNames!=""){
        original_values<- extract_variables_entries(input$OldFacetsXNames)
        new_values<- extract_variables_entries(input$NewFacetsXNames)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("OldFacetsXNames")
          shinyFeedback::hideFeedback("NewFacetsXNames")

          donnees<- data.frame(result_recode())

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% unique(donnees$FacetX))
          shinyFeedback::feedbackWarning("OldFacetsXNames", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Noms Originaux Des Facets-X (Colonne {FacetX}) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$FacetX<-  plyr::mapvalues(donnees$FacetX, original_values, new_values)
          result_recode(donnees)

          # New Facet Names
          output$recodedFacetXName<- renderPrint({
            req(result_recode())
            unique(result_recode()$FacetX)
          })

          # Facet Levels
          # output$OldFacetsXFactorLevelVect<- renderPrint({
          #   req(result_recode())
          #   levels(result_recode()$FacetX)
          # })
          output$NewFacetsXFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetX)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("OldFacetsXNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("NewFacetsXNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { unique(column{{FacetX}}) } !")
          )

        }
      }
    })

    # # ------------------------------------------------------------------------------------------------------#
    # Renommer les noms de Facets-Y
    observeEvent(input$recoderFacetsY, {

      req(result_recode())

      if(input$OldFacetsYNames!="" & input$NewFacetsYNames!=""){
        original_values<- extract_variables_entries(input$OldFacetsYNames)
        new_values<- extract_variables_entries(input$NewFacetsYNames)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("OldFacetsYNames")
          shinyFeedback::hideFeedback("NewFacetsYNames")

          donnees<- data.frame(result_recode())

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% unique(donnees$FacetY))
          shinyFeedback::feedbackWarning("OldFacetsYNames", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Noms Originaux Des Facets-Y (Colonne {FacetX}) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$FacetY<-  plyr::mapvalues(donnees$FacetY, original_values, new_values)
          result_recode(donnees)

          # New Facet Names
          output$recodedFacetYName<- renderPrint({
            req(result_recode())
            unique(result_recode()$FacetY)
          })

          # Facet Levels
          # output$OldFacetsYFactorLevelVect<- renderPrint({
          #   req(result_recode())
          #   levels(result_recode()$FacetY)
          # })
          output$NewFacetsYFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetY)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("OldFacetsYNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("NewFacetsYNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { unique(column{{FacetY}}) } !")
          )

        }
      }
    })

    # # ------------------------------------------------------------------------------------------------------#
    # Renommer les noms de Groupes
    observeEvent(input$recoderGroup, {

      req(result_recode())

      if(input$OldGroupNames!="" & input$NewGroupNames!=""){
        original_values<- extract_variables_entries(input$OldGroupNames)
        new_values<- extract_variables_entries(input$NewGroupNames)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("OldGroupNames")
          shinyFeedback::hideFeedback("NewGroupNames")

          donnees<- data.frame(result_recode())

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% unique(donnees$Group))
          shinyFeedback::feedbackWarning("OldGroupNames", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Noms Originaux Des Groupes (Colonne {Group}) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Group<-  plyr::mapvalues(donnees$Group, original_values, new_values)
          result_recode(donnees)

          # New Group Names
          output$recodedGroupName<- renderPrint({
            req(result_recode())
            unique(result_recode()$Group)
          })

          # Group levels
          output$NewGroupFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Group)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("OldGroupNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("NewGroupNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { unique(column{{Group}}) } !")
          )

        }
      }
    })

    # # ------------------------------------------------------------------------------------------------------#
    # Rcoder les levels du facteur des noms de Variables
    observeEvent(input$recoderVarLevels, {

      req(result_recode())

      if(input$NewVarFactorLevel!=""){
        new_values<- extract_variables_entries(input$NewVarFactorLevel)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(new_values) == length(unique(result_recode()$Variable))
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("NewVarFactorLevel")

          donnees<- data.frame(result_recode())

          # tester si les noms fournis sont présents dans la table à recoder
          testVar<- length(new_values) == sum(new_values %in% levels(donnees$Variable))
          shinyFeedback::feedbackWarning("NewVarFactorLevel", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Nouveaux Niveaux (Levels) du Facteur De L'axe Des Abscisses } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Variable<- factor(donnees$Variable, levels = new_values)
          result_recode(donnees)

          # New Variable Levels
          output$NewVarFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Variable)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("NewVarFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{Variable}}) } !")
          )

        }
      }

    })

    # # ------------------------------------------------------------------------------------------------------#
    # Rcoder les levels du facteur des noms des FacetX
    observeEvent(input$recoderFacetXLevels, {

      req(result_recode())

      if(input$NewFacetsXFactorLevel!=""){
        new_values<- extract_variables_entries(input$NewFacetsXFactorLevel)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(new_values) == length(unique(result_recode()$FacetX))
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("NewFacetsFactorXLevel")

          donnees<- data.frame(result_recode())

          # tester si les noms fournis sont présents dans la table à recoder
          testVar<- length(new_values) == sum(new_values %in% levels(donnees$FacetX))
          shinyFeedback::feedbackWarning("NewFacetsXFactorLevel", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Levels Actuels Des Variables du Facteur Des Facets-X } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$FacetX<- factor(donnees$FacetX, levels = new_values)
          result_recode(donnees)

          # New Variable Levels
          output$NewFacetsXFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetX)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("NewFacetsXFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{FacetX}}) } !")
          )

        }
      }

    })

    # # ------------------------------------------------------------------------------------------------------#
    # Rcoder les levels du facteur des noms des FacetY
    observeEvent(input$recoderFacetYLevels, {

      req(result_recode())

      if(input$NewFacetsYFactorLevel!=""){
        new_values<- extract_variables_entries(input$NewFacetsYFactorLevel)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(new_values) == length(levels(result_recode()$FacetY))
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("NewFacetsFactorYLevel")

          donnees<- data.frame(result_recode())

          # tester si les noms fournis sont présents dans la table à recoder
          testVar<- length(new_values) == sum(new_values %in% levels(donnees$FacetY))
          shinyFeedback::feedbackWarning("NewFacetsYFactorLevel", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Levels Actuels Des Variables du Facteur Des Facets-Y } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$FacetY<- factor(donnees$FacetY, levels = new_values)
          result_recode(donnees)

          # New Variable Levels
          output$NewFacetsYFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$FacetY)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("NewFacetsYFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{FacetY}}) } !")
          )

        }
      }

    })

    # # ------------------------------------------------------------------------------------------------------#
    # Rcoder les levels du facteur des noms des Groupes
    observeEvent(input$recoderGroupLevels, {

      req(result_recode())

      if(input$NewGroupFactorLevel!=""){
        new_values<- extract_variables_entries(input$NewGroupFactorLevel)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(new_values) == length(unique(result_recode()$Group))
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("NewGroupFactorLevel")

          donnees<- data.frame(result_recode())

          # tester si les noms fournis sont présents dans la table à recoder
          testVar<- length(new_values) == sum(new_values %in% levels(donnees$Group))
          shinyFeedback::feedbackWarning("NewGroupFactorLevel", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Levels Actuels du Facteur Des Groupes (Colonne {Group}) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Group<- factor(donnees$Group, levels = new_values)
          result_recode(donnees)

          # New Variable Levels
          output$NewGroupFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Group)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("NewGroupFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{Group}}) } !")
          )

        }
      }

    })

    return(list(data_4_multivariatefacets_boxplots = reactive({ result_recode() })))

  })
}

## To be copied in the UI
# mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_ui("MatrixFacetsMultivariateBoxplotPlotDataPreparation_1")

## To be copied in the server
# mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_server("MatrixFacetsMultivariateBoxplotPlotDataPreparation_1")
