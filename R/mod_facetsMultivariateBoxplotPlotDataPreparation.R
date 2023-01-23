#' facetsMultivariateBoxplotPlotDataPreparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_facetsMultivariateBoxplotPlotDataPreparation_ui <- function(id, label = ""){
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

    actionButtonStyled(ns("recoderFacets"), span("Recoder Noms Facets", id=ns("recoderFacetsAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("OldFacetsNames"), label = "Noms Originaux Des Facets (Colonne {Facet})",
                      value = "", placeholder = "i.e., oldFacet1;oldFacet2;oldFacet3;etc.",
                      width = "100%"
                    )
             ),
             column(6,
                    textInput(
                      ns("NewFacetsNames"), label = "Nouveaux Noms Des Facets (Colonne {Facet})",
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
             column(6, verbatimTextOutput(ns("defaultFacetName"))),
             column(6, verbatimTextOutput(ns("recodedFacetName")))
    ),

    tags$hr(style="border-color:gray;"),

    # Noms des Groupes

    actionButtonStyled(ns("recoderGroup"), span("Recoder Noms Groupes", id=ns("recoderGroupAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(6,
                    textInput(
                      ns("OldGroupNames"), label = "Noms Originaux Des Groupes (Colonne {Group})",
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

    actionButtonStyled(ns("recoderVarLevels"), span("Recoder Levels (Ordre) des Variables", id=ns("recoderVarLevelsAnimate")), class= "", type="primary"),


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

    actionButtonStyled(ns("recoderFacetLevels"), span("Recoder Levels (Ordre) des Facets", id=ns("recoderFacetLevelsAnimate")), class= "", type="primary"),

    fluidRow(align = "left",
             column(12,
                    textAreaInput(
                      ns("NewFacetsFactorLevel"), label = "Levels Actuels Des Variables du Facteur Des Facets (Nom des Box)",
                      value = "", placeholder = "i.e., level1;level2;level3;etc.",
                      width = "100%"
                    )
             )
    ),

    fluidRow(align = "left",
             column(12, verbatimTextOutput(ns("NewFacetsFactorLevelVect")))
    ),

    tags$hr(style="border-color:gray;"),

    # Levels du factor des Groupes

    actionButtonStyled(ns("recoderGroupLevels"), span("Recoder Levels (Ordre) des Groups", id=ns("recoderGroupLevelsAnimate")), class= "", type="primary"),

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

#' facetsMultivariateBoxplotPlotDataPreparation Server Functions
#'
#' @noRd
mod_facetsMultivariateBoxplotPlotDataPreparation_server <- function(id, data){
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

      # Facet Names
      output$defaultFacetName<- renderPrint({
        req(data)
        unique(data$Facet)
      })
      output$recodedFacetName<- renderPrint({
        req(data)
        unique(data$Facet)
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
      output$OldFacetsFactorLevelVect<- renderPrint({
        req(data)
        levels(data$Facet)
      })
      output$NewFacetsFactorLevelVect<- renderPrint({
        req(data)
        levels(data$Facet)
      })

      # New Group Levels
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
          output$recodedFacetName<- renderPrint({
            req(result_recode())
            unique(result_recode()$Facet)
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
          output$NewFacetsFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Facet)
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
    # Renommer les noms de Facets
    observeEvent(input$recoderFacets, {

      req(result_recode())

      if(input$OldFacetsNames!="" & input$NewFacetsNames!=""){
        original_values<- extract_variables_entries(input$OldFacetsNames)
        new_values<- extract_variables_entries(input$NewFacetsNames)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(original_values) == length(new_values)
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("OldFacetsNames")
          shinyFeedback::hideFeedback("NewFacetsNames")

          donnees<- data.frame(result_recode())

          # tester si les noms originaux fournis sont présents dans la table à recoder
          testVar<- length(original_values) == sum(original_values %in% unique(donnees$Facet))
          shinyFeedback::feedbackWarning("OldFacetsNames", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Noms Originaux Des Facets (Colonne {Facet}) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Facet<-  plyr::mapvalues(donnees$Facet, original_values, new_values)
          result_recode(donnees)

          # New Facet Names
          output$recodedFacetName<- renderPrint({
            req(result_recode())
            unique(result_recode()$Facet)
          })

          # New Facet Levels
          output$NewFacetsFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Facet)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("OldFacetsNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyFeedback::feedbackWarning("NewFacetsNames", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { unique(column{{Facet}}) } !")
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

          # New Facet Names
          output$recodedGroupName<- renderPrint({
            req(result_recode())
            unique(result_recode()$Group)
          })

          # New Group Levels
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
                "{ Nouveaux Levels (Niveaux) du Facteur De L'axe Des Abscisses } sont biens présents dans la table des ",
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
    # Rcoder les levels du facteur des noms des Facets
    observeEvent(input$recoderFacetLevels, {

      req(result_recode())

      if(input$NewFacetsFactorLevel!=""){
        new_values<- extract_variables_entries(input$NewFacetsFactorLevel)

        # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
        len_vect<- length(new_values) == length(unique(result_recode()$Facet))
        if(len_vect){# si les longueurs sont identiques

          shinyFeedback::hideFeedback("NewFacetsFactorLevel")

          donnees<- data.frame(result_recode())

          # tester si les noms fournis sont présents dans la table à recoder
          testVar<- length(new_values) == sum(new_values %in% levels(donnees$Facet))
          shinyFeedback::feedbackWarning("NewFacetsFactorLevel", !testVar, "Valeurs Incorrectes !")
          if(testVar==FALSE){
            shinyalert::shinyalert(
              "Valeurs d'entrée Incorrectes !",
              paste0(
                "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                "{ Levels Actuels Des Variables du Facteur Des Facets (Nom des Box) } sont biens présents dans la table des ",
                "données chargées !"
              )
            )
          }

          req(testVar)
          donnees$Facet<- factor(donnees$Facet, levels = new_values)
          result_recode(donnees)

          # New Variable Levels
          output$NewFacetsFactorLevelVect<- renderPrint({
            req(result_recode())
            levels(result_recode()$Facet)
          })

        }else{# si les longueurs ne sont pas identiques

          shinyFeedback::feedbackWarning("NewFacetsFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
          shinyalert::shinyalert(
            "Longueurs De Vecteurs !",
            paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{Facet}}) } !")
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

          # New Groups Levels
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
# mod_facetsMultivariateBoxplotPlotDataPreparation_ui("facetsMultivariateBoxplotPlotDataPreparation_1")

## To be copied in the server
# mod_facetsMultivariateBoxplotPlotDataPreparation_server("facetsMultivariateBoxplotPlotDataPreparation_1")
