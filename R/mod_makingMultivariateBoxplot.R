#' makingMultivariateBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingMultivariateBoxplot_ui <- function(id, label){
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
             column(4, actionButtonStyled(ns("boxplot"), span("Boxplots", id=ns("boxplotAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("multivariateBoxplot")), type=5),
    # stats
    div(dataTableOutput(ns("statsSummary")), style="font-size:85%")
  )
}

#' makingMultivariateBoxplot Server Functions
#'
#' @noRd
mod_makingMultivariateBoxplot_server <- function(id, cleanedData, multivariateBoxplotOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Interpolation data results
    dataPlot<- reactive({
      req(cleanedData)
      cleanedData[[1]]()
    })

    #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
    # gestion des entrées vecteurs
    extract_variables_entries<-  function(text){
      text<-  gsub(" ", "", text)
      split<-  strsplit(text, ";", fixed = FALSE)[[1]]
      split
    }

    multivariateBoxplot <- eventReactive(input$boxplot, {
      req(dataPlot(), multivariateBoxplotOptions)

      # Couleur des variables
      # ------------------------------------------------------------------------------------------------------#
      manualColor<-  function(){
        # Validation des entrées vecteurs
        if(multivariateBoxplotOptions$variableColors()!=""){
          variables_colors<- extract_variables_entries(multivariateBoxplotOptions$variableColors())

          # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
          len_vect<- length(variables_colors) == length(unique(dataPlot()$Group))
          if(len_vect){# si les longueurs sont identiques

            # tester si les noms originaux fournis sont présents dans la table es colors
            testColor<- length(variables_colors) == sum(variables_colors %in% colors())
            if(testColor==FALSE){
              shinyalert::shinyalert(
                "Erreur dans le vecteur de couleurs fournis !",
                paste0(
                  "Vous devez vous assurer que tous les noms de Couleurs fournis dans le champs ",
                  "{ Couleur des Catégories|Types } sont biens présents dans la table des couleurs définis dans { colors() } !"
                )
              )

              return(ggplot2::scale_fill_brewer(palette = "Spectral"))

            }else{
              return(
                ggplot2::scale_fill_manual(values = variables_colors)
              )
            }

          }else{# si les longueurs ne sont pas identiques

            shinyalert::shinyalert(
              "Erreur dans le vecteur de couleurs fournis !",
              paste0("OUPS !! Vous devez spécifier autant de couleurs qu'il y'a de catégories|types dans le champ {{Group}}!")
            )

            return(ggplot2::scale_fill_brewer(palette = "Spectral"))
          }
        }else{
          return(ggplot2::scale_fill_brewer(palette = "Spectral"))
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      valeurSeuil<-function(){
        if(multivariateBoxplotOptions$threshold()!=""){
          thresholdValue<-  as.numeric(multivariateBoxplotOptions$threshold())
          if(is.na(thresholdValue)){
            shinyalert::shinyalert(
              "Valeur Seuil Incorrecte !",
              paste0("OUPS !! Vous devez saisir une valeur numérique dans le champ { Valeur Seuil } !")
            )

            return(ggplot2::geom_blank())

          }else{
            # tracé de la valeur seuil
            return(
              ggplot2::geom_hline(
                ggplot2::aes(yintercept=thresholdValue), color=multivariateBoxplotOptions$thresholdColor(),
                size=as.numeric(multivariateBoxplotOptions$thresholdSize()),
                linetype = ifelse(length(multivariateBoxplotOptions$thresholdLineType()),
                                  as.numeric(multivariateBoxplotOptions$thresholdLineType()),
                                  multivariateBoxplotOptions$thresholdLineType())
              )
            )
          }
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### TYPE DE POSITIONNEMENT DE LA LEGENDE
      if(multivariateBoxplotOptions$legPosType() == "Côté"){
        leg_pos <- multivariateBoxplotOptions$legLoc()
      }else{
        leg_pos <- c(as.numeric(multivariateBoxplotOptions$legPosXcoord()), as.numeric(multivariateBoxplotOptions$legPosYcoord()))
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      flipCoord <- function(){

        if(multivariateBoxplotOptions$flipAxis() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      orderingPlot <- function(){

        if(multivariateBoxplotOptions$reorderGprah() == "Descendant"){
          return(
            dataPlot()  %>%
              dplyr::mutate(Variable = forcats::fct_reorder(Variable, desc(Valeur)))
          )
        }else{
          return(
            dataPlot()  %>%
              dplyr::mutate(Variable = forcats::fct_reorder(Variable, Valeur))
          )
        }
      }

      # dataPlot
      donnees <- data.frame(orderingPlot())

      # plot
      multivariateBplt <-
        ggplot2::ggplot(
          data=donnees,
          ggplot2::aes(x=Variable, y=Valeur, fill=Group)
        )+
        # barres d'erreur
        ggplot2::stat_boxplot(geom = "errorbar")+
        # boites à moustaches
        ggplot2::geom_boxplot(
          outlier.shape = as.integer(multivariateBoxplotOptions$outlierShape()), outlier.alpha = 1,
          outlier.color = multivariateBoxplotOptions$outlierColor(), outlier.size = as.numeric(multivariateBoxplotOptions$outlierSize()),
          color = multivariateBoxplotOptions$boxplotColor(),  alpha =  as.numeric(multivariateBoxplotOptions$boxplotAlpha()),
          width = as.numeric(multivariateBoxplotOptions$boxplotsWidth())
        )+
        ggplot2::scale_y_continuous(
          limits = c(min(dataPlot()$Valeur), max(dataPlot()$Valeur)),
          breaks = seq(min(dataPlot()$Valeur), max(dataPlot()$Valeur), length.out=as.numeric(multivariateBoxplotOptions$yAxisNbreaks())),
          labels = round(seq(min(dataPlot()$Valeur), max(dataPlot()$Valeur), length.out=as.numeric(multivariateBoxplotOptions$yAxisNbreaks())), 2)
        )+
        # légende des Catégories|Type
        manualColor()+

        # valeur Seuil
        valeurSeuil() +

        # pivotage du graphique
        flipCoord() +

        # personnalisation de la légende [nbre de lignes, taille des symboles, etc.]
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            nrow = as.numeric(multivariateBoxplotOptions$legendNrow()), byrow = multivariateBoxplotOptions$legendByRow(),
            keywidth = grid::unit(as.numeric(multivariateBoxplotOptions$legendKeyWidth()), "cm"),
            keyheight = grid::unit(as.numeric(multivariateBoxplotOptions$legendKeyHeight()), "cm")
          )
        )+

        # personnalisation des éléments du graphique
        ggplot2::theme(
          legend.direction =  multivariateBoxplotOptions$legDir(),

          legend.text = ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateBoxplotOptions$LegTextSize()), color =  multivariateBoxplotOptions$LegTextColor(),
            margin = ggplot2::margin(
              b=as.numeric(multivariateBoxplotOptions$legTextMarginB()), l=as.numeric(multivariateBoxplotOptions$legTextMarginL()),
              t=as.numeric(multivariateBoxplotOptions$legTextMarginT()), r=as.numeric(multivariateBoxplotOptions$legTextMarginR()),
              unit = "cm"
            ),
            face = "bold"
          ),

          legend.position =  leg_pos,

          legend.margin = ggplot2::margin(
            b=as.numeric(multivariateBoxplotOptions$legMarginB()), l=as.numeric(multivariateBoxplotOptions$legMarginL()),
            t=as.numeric(multivariateBoxplotOptions$legMarginT()), r=as.numeric(multivariateBoxplotOptions$legMarginR()), unit = "cm"
          ),

          legend.box.background = ggplot2::element_rect(fill = "white", color = "white"),
          legend.key = ggplot2::element_blank(),

          # axes
          axis.text.x = ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateBoxplotOptions$xAxisTextSize()),
            color = multivariateBoxplotOptions$axisTextColor(),
            angle=as.numeric(multivariateBoxplotOptions$xAxisTextAngle()),
            hjust = as.numeric(multivariateBoxplotOptions$xAxisTextHjust()),
            vjust = as.numeric(multivariateBoxplotOptions$xAxisTextVjust())
          ),
          axis.ticks = ggplot2::element_line(
            colour=multivariateBoxplotOptions$axisTicksColor(), size=as.numeric(multivariateBoxplotOptions$axisTicksSize())
          ),
          axis.title = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(multivariateBoxplotOptions$AxisTitleSize())
          ),
          # les grilles
          panel.grid.major = ggplot2::element_line(
            linetype = ifelse(length(multivariateBoxplotOptions$panelGridLineType()),
                              as.numeric(multivariateBoxplotOptions$panelGridLineType()),
                              multivariateBoxplotOptions$panelGridLineType()),
            color = multivariateBoxplotOptions$panelGridColor(), size=grid::unit(.23, "cm")
          ),
          panel.grid.minor = ggplot2::element_blank(),
          # texte de l'axe des y
          axis.text.y= ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateBoxplotOptions$yAxisTextSize()), color =  multivariateBoxplotOptions$axisTextColor()
          ),
          # contours
          panel.background = ggplot2::element_rect(
            color=multivariateBoxplotOptions$panelBackgroundLineColor(), fill="white", size=as.numeric(multivariateBoxplotOptions$panelBackgroundLineSize())
          )
        )+

        ggplot2::labs(
          x = multivariateBoxplotOptions$xAxisTitle(), y = multivariateBoxplotOptions$yAxisTitle(), fill = NULL, color = NULL
        )
      #
      # résummé statistique
      statSum <- dataPlot() %>%
        dplyr::group_by(Group, Variable) %>%
        dplyr::summarise(
          Min. = round(min(Valeur, na.rm = T), 2), Quart1 = round(quantile(Valeur, .25), 2),
          Médianne = round(median(Valeur), 2), Quart3 = round(quantile(Valeur, .75), 2),
          Moyenne = round(mean(Valeur), 2), Max = round(max(Valeur), 2),
          "Ecart type" = round(sd(Valeur), 2)
        )

      return(list(multivariateBplt, statSum))
    })

    output$multivariateBoxplot <- renderPlot({
      req(multivariateBoxplot())
      multivariateBoxplot()[[1]]
    })

    output$statsSummary <- renderDataTable({
      req(multivariateBoxplot())
      multivariateBoxplot()[[2]]
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("BoxplotMultivarié-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggsave(file, multivariateBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("BoxplotMultivarié-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggsave(file, multivariateBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )
  })
}

## To be copied in the UI
# mod_makingMultivariateBoxplot_ui("makingMultivariateBoxplot_1")

## To be copied in the server
# mod_makingMultivariateBoxplot_server("makingMultivariateBoxplot_1")
