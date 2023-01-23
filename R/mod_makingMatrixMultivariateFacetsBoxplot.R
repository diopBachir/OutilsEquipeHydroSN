#' makingMatrixMultivariateFacetsBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingMatrixMultivariateFacetsBoxplot_ui <- function(id, label){
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
    withSpinner(plotOutput(ns("multivariateMatrixFacetsBoxplot")), type=5),
    # stats
    div(dataTableOutput(ns("statsSummary")), style="font-size:85%"),

    br(),br(),br()
  )
}

#' makingMatrixMultivariateFacetsBoxplot Server Functions
#'
#' @noRd
mod_makingMatrixMultivariateFacetsBoxplot_server <- function(id, cleanedData, multivariateMatrixFacetsBoxplotOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # data
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

    # parsing des codes
    parseCode<- function(text_to_parse){
      parseResult<- eval(parse(text = text_to_parse))
      parseResult
    }

    multivariateMatrixFacetsBoxplot <- eventReactive(input$boxplot, {
      req(dataPlot(), multivariateMatrixFacetsBoxplotOptions)

      # Couleur des variables
      # ------------------------------------------------------------------------------------------------------#
      manualColor<-  function(){
        # Validation des entrées vecteurs
        if(multivariateMatrixFacetsBoxplotOptions$variableColors()!=""){
          variables_colors<- extract_variables_entries(multivariateMatrixFacetsBoxplotOptions$variableColors())

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
              paste0("OUPS !! Vous devez spécifier autant de couleurs qu'il y'a de catégories|types !")
            )

            return(ggplot2::scale_fill_brewer(palette = "Spectral"))
          }
        }else{
          return(ggplot2::scale_fill_brewer(palette = "Spectral"))
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      # La valeur seuil
      valeurSeuil<-function(donnees, seuil){
        thresholdValue<-  multivariateMatrixFacetsBoxplotOptions$threshold()
        if(thresholdValue == FALSE){
          showNotification("La ligne matérialisant la valeur seuil a été désactivée !")

          return(ggplot2::geom_blank())

        }else{
          # tracé de la valeur seuil
          return(
            ggplot2::geom_hline(
              data=donnees,  ggplot2::aes_string(yintercept=seuil),
              color=multivariateMatrixFacetsBoxplotOptions$thresholdColor(),
              size=as.numeric(multivariateMatrixFacetsBoxplotOptions$thresholdSize()),
              linetype = ifelse(length(multivariateMatrixFacetsBoxplotOptions$thresholdLineType()),
                                as.numeric(multivariateMatrixFacetsBoxplotOptions$thresholdLineType()),
                                multivariateMatrixFacetsBoxplotOptions$thresholdLineType())
            )
          )
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### TYPE DE POSITIONNEMENT DE LA LEGENDE
      if(multivariateMatrixFacetsBoxplotOptions$legPosType() == "Côté"){
        leg_pos <- multivariateMatrixFacetsBoxplotOptions$legLoc()
      }else{
        leg_pos <- c(
          as.numeric(multivariateMatrixFacetsBoxplotOptions$legPosXcoord()),
          as.numeric(multivariateMatrixFacetsBoxplotOptions$legPosYcoord())
        )
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Liberté ou non des axes
      facetScale <- function(){

        if(multivariateMatrixFacetsBoxplotOptions$FacetScale() == "fixed"){
          return(
            ggh4x::facet_grid2(
              donnees[["FacetY"]] ~ donnees[["FacetX"]]
            )
          )
        }else if(multivariateMatrixFacetsBoxplotOptions$FacetScale() == "free_y"){
          return(
            ggh4x::facet_grid2(
              donnees[["FacetY"]] ~ donnees[["FacetX"]], scales = "free_y",  independent =  "y"
            )
          )
        }else{
          return(
            ggh4x::facet_grid2(
              donnees[["FacetY"]] ~ donnees[["FacetX"]], scales = "free_x",  independent =  "x"
            )
          )
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      flipCoord <- function(){

        if(multivariateMatrixFacetsBoxplotOptions$flipAxis() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      orderingPlot <- function(){

        if(multivariateMatrixFacetsBoxplotOptions$reorderGprah() == "Descendant"){
          return(
            dataPlot()  %>%
              dplyr::group_by(FacetY) %>%
              dplyr::mutate(Variable = forcats::fct_reorder(Variable, desc(Valeur)))
          )
        }else{
          return(
            dataPlot()  %>%
              dplyr::group_by(FacetY) %>%
              dplyr::mutate(Variable = forcats::fct_reorder(Variable, Valeur))
          )
        }
      }

      # plot
      donnees <- data.frame(orderingPlot())
      multivariateBplt <-
        donnees  %>%
        ggplot2::ggplot(
           ggplot2::aes(x=Variable, y=Valeur, fill=Group)
        )+
        # barres d'erreur
        ggplot2::stat_boxplot(geom = "errorbar")+
        # boites à moustaches
        ggplot2::geom_boxplot(
          outlier.shape = as.integer(multivariateMatrixFacetsBoxplotOptions$outlierShape()), outlier.alpha = 1,
          outlier.color = multivariateMatrixFacetsBoxplotOptions$outlierColor(), outlier.size = as.numeric(multivariateMatrixFacetsBoxplotOptions$outlierSize()),
          color = multivariateMatrixFacetsBoxplotOptions$boxplotColor(),  alpha =  as.numeric(multivariateMatrixFacetsBoxplotOptions$boxplotAlpha()),
          width = as.numeric(multivariateMatrixFacetsBoxplotOptions$boxplotsWidth())
        )+

        # foramt axis
        ggplot2::scale_y_continuous(n.breaks = 6) +

        # test Facets Scales Override
        facetScale() +

        # légende des Catégories|Group
        manualColor()+

        # valeur Seuil
        valeurSeuil(donnees, "Seuil") +

        # pivotage du graphique
        flipCoord() +

        # personnalisation de la légende [nbre de lignes, taille des symboles, etc.]
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            nrow = as.numeric(multivariateMatrixFacetsBoxplotOptions$legendNrow()),
            byrow = multivariateMatrixFacetsBoxplotOptions$legendByRow(),
            keywidth = grid::unit(as.numeric(multivariateMatrixFacetsBoxplotOptions$legendKeyWidth()), "cm"),
            keyheight = grid::unit(as.numeric(multivariateMatrixFacetsBoxplotOptions$legendKeyHeight()), "cm")
          )
        )+

        # personnalisation des éléments du graphique
        ggplot2::theme(
          legend.direction =  multivariateMatrixFacetsBoxplotOptions$legDir(),

          legend.text = ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateMatrixFacetsBoxplotOptions$LegTextSize()), color =  multivariateMatrixFacetsBoxplotOptions$LegTextColor(),
            margin = ggplot2::margin(
              b=as.numeric(multivariateMatrixFacetsBoxplotOptions$legTextMarginB()),
              l=as.numeric(multivariateMatrixFacetsBoxplotOptions$legTextMarginL()),
              t=as.numeric(multivariateMatrixFacetsBoxplotOptions$legTextMarginT()),
              r=as.numeric(multivariateMatrixFacetsBoxplotOptions$legTextMarginR()),
              unit = "cm"
            ),
            face = "bold"
          ),

          legend.position =  leg_pos,

          legend.margin = ggplot2::margin(
            b=as.numeric(multivariateMatrixFacetsBoxplotOptions$legMarginB()), l=as.numeric(multivariateMatrixFacetsBoxplotOptions$legMarginL()),
            t=as.numeric(multivariateMatrixFacetsBoxplotOptions$legMarginT()), r=as.numeric(multivariateMatrixFacetsBoxplotOptions$legMarginR()), unit = "cm"
          ),

          legend.box.background = ggplot2::element_rect(fill = "white", color = "white"),
          legend.key =ggplot2::element_blank(),

          # axes
          axis.text.x = ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateMatrixFacetsBoxplotOptions$xAxisTextSize()),
            color = multivariateMatrixFacetsBoxplotOptions$axisTextColor(),
            angle=as.numeric(multivariateMatrixFacetsBoxplotOptions$xAxisTextAngle()),
            hjust = as.numeric(multivariateMatrixFacetsBoxplotOptions$xAxisTextHjust()),
            vjust = as.numeric(multivariateMatrixFacetsBoxplotOptions$xAxisTextVjust()), face = "bold"
          ),
          axis.ticks = ggplot2::element_line(
            colour=multivariateMatrixFacetsBoxplotOptions$axisTicksColor(), size=as.numeric(multivariateMatrixFacetsBoxplotOptions$axisTicksSize())
          ),
          axis.title = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(multivariateMatrixFacetsBoxplotOptions$AxisTitleSize())
          ),
          # les grilles
          panel.grid.major = ggplot2::element_line(
            linetype = ifelse(length(multivariateMatrixFacetsBoxplotOptions$panelGridLineType()),
                              as.numeric(multivariateMatrixFacetsBoxplotOptions$panelGridLineType()),
                              multivariateMatrixFacetsBoxplotOptions$panelGridLineType()),
            color = multivariateMatrixFacetsBoxplotOptions$panelGridColor(), size=grid::unit(.23, "cm")
          ),
          panel.grid.minor = ggplot2::element_blank(),
          # texte de l'axe des y
          axis.text.y= ggplot2::element_text(
            family = "Times", size=as.numeric(multivariateMatrixFacetsBoxplotOptions$yAxisTextSize()),
            color =  multivariateMatrixFacetsBoxplotOptions$axisTextColor(), face = "bold"
          ),
          # contours
          panel.background = ggplot2::element_rect(
            color=multivariateMatrixFacetsBoxplotOptions$panelBackgroundLineColor(), fill="white", size=as.numeric(multivariateMatrixFacetsBoxplotOptions$panelBackgroundLineSize())
          ),
          # arrière plan du graphique
          # panel.border = element_rect(color="gray", fill=NA, size = 1.3),
          # titre des facets
          strip.background = ggplot2::element_blank(),
          strip.text.x.top = ggplot2::element_text(
            family = "Times", size = as.numeric(multivariateMatrixFacetsBoxplotOptions$stripTextSize()), face="bold",
            margin = ggplot2::margin(
              b = multivariateMatrixFacetsBoxplotOptions$stripMarginB(),  t = multivariateMatrixFacetsBoxplotOptions$stripMarginT(),
              l = multivariateMatrixFacetsBoxplotOptions$stripMarginL(),  r = multivariateMatrixFacetsBoxplotOptions$stripMarginR(),
              unit =  multivariateMatrixFacetsBoxplotOptions$stripMarginUnit()
            )
          ),
          strip.text.y.right = ggplot2::element_text(
            family = "Times", size = as.numeric(multivariateMatrixFacetsBoxplotOptions$stripTextSize()), face="bold",
            margin = ggplot2::margin(
              b = multivariateMatrixFacetsBoxplotOptions$stripMarginB(),  t = multivariateMatrixFacetsBoxplotOptions$stripMarginT(),
              l = multivariateMatrixFacetsBoxplotOptions$stripMarginL(),  r = multivariateMatrixFacetsBoxplotOptions$stripMarginR(),
              unit =  multivariateMatrixFacetsBoxplotOptions$stripMarginUnit()
            ), angle = 90
          ),
          # legend.key = ggplot2::element_rect(color = NA, fill = NA),
          panel.spacing.y = grid::unit(as.numeric(multivariateMatrixFacetsBoxplotOptions$panelYspacing()), "cm"),
          panel.spacing.x = grid::unit(as.numeric(multivariateMatrixFacetsBoxplotOptions$panelXspacing()), "cm")
        )+

        ggplot2::labs(
          x = multivariateMatrixFacetsBoxplotOptions$xAxisTitle(), y = multivariateMatrixFacetsBoxplotOptions$yAxisTitle(), fill = NULL, color = NULL
        )

      # résummé statistique
      statSum <- donnees %>%
        dplyr::group_by(FacetX, FacetY, Group, Variable) %>%
        dplyr::summarise(
          Min. = min(Valeur, na.rm = T), Q1 = quantile(Valeur, .25),
          Médianne = median(Valeur), Q3 = quantile(Valeur, .75),
          Moyenne = mean(Valeur), Max = max(Valeur),
          "Ec. type" = sd(Valeur)
        )  %>%
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.numeric), ~round(., 2), .names = "{.col}")
        )

      return(list(multivariateBplt, statSum))
    })

    output$multivariateMatrixFacetsBoxplot <- renderPlot({
      req(multivariateMatrixFacetsBoxplot())
      multivariateMatrixFacetsBoxplot()[[1]]
    })

    output$statsSummary <- renderDataTable({
      req(multivariateMatrixFacetsBoxplot())
      multivariateMatrixFacetsBoxplot()[[2]]
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("BoxplotsMultivarieAvecFacets_2D-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, multivariateMatrixFacetsBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("BoxplotsMultivarieAvecFacets_2D-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, multivariateMatrixFacetsBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )
  })
}

## To be copied in the UI
# mod_makingMatrixMultivariateFacetsBoxplot_ui("makingMatrixMultivariateFacetsBoxplot_1")

## To be copied in the server
# mod_makingMatrixMultivariateFacetsBoxplot_server("makingMatrixMultivariateFacetsBoxplot_1")
