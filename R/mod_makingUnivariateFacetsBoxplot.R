#' makingUnivariateFacetsBoxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_makingUnivariateFacetsBoxplot_ui <- function(id){
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

    fluidRow(
      column(12, textAreaInput(ns("FacetsScalesOverride"), label = div("Override de l'Axe des Ordonnées {Code R Valable Requis}", style="family:Georgia;text-align:left;font-size:100%"),
                               value = "", placeholder = "scale_override(1, scale_y_continuous(breaks = c(-1, 1)))\nscale_override(2, scale_y_continuous(breaks = c(-10, 0)))", width = "100%"))
    ),

    tags$hr(style="border-color:gray;"),

    # plot
    withSpinner(plotOutput(ns("univariateFacetsBoxplot")), type=5),
    # stats
    div(dataTableOutput(ns("statsSummary")), style="font-size:85%"),

    br(),br(),br()

  )
}

#' makingUnivariateFacetsBoxplot Server Functions
#'
#' @noRd
mod_makingUnivariateFacetsBoxplot_server <- function(id, cleanedData, univariateFacetsBoxplotOptions){
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

    # parsing des codes
    parseCode<- function(text_to_parse){
      parseResult<- eval(parse(text = text_to_parse))
      parseResult
    }

    univariateFacetsBoxplot <- eventReactive(input$boxplot, {
      req(dataPlot(), univariateFacetsBoxplotOptions)

      # Couleur des variables
      # ------------------------------------------------------------------------------------------------------#
      manualColor<-  function(){
        # Validation des entrées vecteurs
        if(univariateFacetsBoxplotOptions$variableColors()!=""){
          variables_colors<- extract_variables_entries(univariateFacetsBoxplotOptions$variableColors())

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

              return(ggplot2::scale_fill_brewer(palette = "Set3"))

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

            return(ggplot2::scale_fill_brewer(palette = "Set3"))
          }
        }else{
          return(ggplot2::scale_fill_brewer(palette = "Set3"))
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      # La valeur seuil
      valeurSeuil<-function(donnees, seuil){
        thresholdValue<-  univariateFacetsBoxplotOptions$threshold()
        if(thresholdValue == FALSE){
          showNotification("La ligne matérialisant la valeur seuil a été désactivée !")

          return(ggplot2::geom_blank())

        }else{
          # tracé de la valeur seuil
          return(
            ggplot2::geom_hline(
              data=donnees, ggplot2::aes_string(yintercept=seuil),
              color=univariateFacetsBoxplotOptions$thresholdColor(),
              size=as.numeric(univariateFacetsBoxplotOptions$thresholdSize()),
              linetype = ifelse(length(univariateFacetsBoxplotOptions$thresholdLineType()),
                                as.numeric(univariateFacetsBoxplotOptions$thresholdLineType()),
                                univariateFacetsBoxplotOptions$thresholdLineType())
            )
          )
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### TYPE DE POSITIONNEMENT DE LA LEGENDE
      if(univariateFacetsBoxplotOptions$legPosType() == "Côté"){
        leg_pos <- univariateFacetsBoxplotOptions$legLoc()
      }else{
        leg_pos <- c(
          as.numeric(univariateFacetsBoxplotOptions$legPosXcoord()),
          as.numeric(univariateFacetsBoxplotOptions$legPosYcoord())
        )
      }

      # ------------------------------------------------------------------------------------------------------#
      # Modification de l'axe des ordonnées
      facetScaleOverriding <- function(){

        shinyFeedback::hideFeedback("FacetsScalesOverride")
        correctCode <- FALSE

        if(input$FacetsScalesOverride != ""){
          scaleOverrided<- tryCatch(
            { lapply(extract_variables_entries(input$FacetsScalesOverride), parseCode) },
            error = function(e) {
              return(scale_override(1, ggplot2::scale_y_continuous(n.breaks = 6)))
            }
          )

          # tester si toutes les lignes ont été parsées
          for(scaleFacet in scaleOverrided){
            if(class(scaleFacet)[1] != "scale_override"){
              shinyFeedback::feedbackWarning(
                "FacetsScalesOverride", class(scaleFacet) != "scale_override",
                "L'expression entrée doit être un Objet de Classe {{ scale_override }}"
              )
              correctCode <- FALSE
              return(scale_override(1, ggplot2::scale_y_continuous(n.breaks = 6)))
              break
            }else{
              correctCode <- TRUE
            }
          }

        }else{
          return(scale_override(1, ggplot2::scale_y_continuous(n.breaks = 6)))
        }

        # if code is correct
        if(correctCode==TRUE){

          shinyFeedback::feedbackSuccess(
            "FacetsScalesOverride", correctCode==TRUE,
            paste0( "Code {R} OK !!" )
          )

          return(scaleOverrided)
        }

      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      flipCoord <- function(){

        if(univariateFacetsBoxplotOptions$flipAxis() == TRUE){
          return(ggplot2::coord_flip())
        }else{
          return(ggplot2::geom_blank())
        }
      }

      # ------------------------------------------------------------------------------------------------------#
      ### Transposer ou non le graphique
      orderingPlot <- function(){

        if(univariateFacetsBoxplotOptions$reorderGprah() == "Descendant"){
          return(
            dataPlot()  %>%
              dplyr::group_by(Group) %>%
              dplyr::mutate(Variable = fct_reorder(Variable, desc(Valeur)))
          )
        }else{
          return(
            dataPlot()  %>%
              dplyr::group_by(Group) %>%
              dplyr::mutate(Variable = forcats::fct_reorder(Variable, Valeur))
          )
        }
      }

      # dataPlot
      donnees <- data.frame(orderingPlot())

      # ------------------------------------------------------------------------------------------------------#
      ### Liberté ou non des axes
      facetScale <- function(){

        if(univariateFacetsBoxplotOptions$FacetScale() == "fixed"){
          return(
            facet_wrap_custom(
              ggplot2::vars(donnees[["Group"]]), ncol = as.numeric(univariateFacetsBoxplotOptions$facetNcol()), scales = "fixed",
              scale_overrides = facetScaleOverriding()
            )
          )
        }else if(univariateFacetsBoxplotOptions$FacetScale() == "free"){
          return(
            facet_wrap_custom(
              ggplot2::vars(donnees[["Group"]]), ncol = as.numeric(univariateFacetsBoxplotOptions$facetNcol()), scales = "free",
              scale_overrides = facetScaleOverriding()
            )
          )
        }else if(univariateFacetsBoxplotOptions$FacetScale() == "free_y"){
          return(
            facet_wrap_custom(
              ggplot2::vars(donnees[["Group"]]), ncol = as.numeric(univariateFacetsBoxplotOptions$facetNcol()), scales = "free_y",
              scale_overrides = facetScaleOverriding()
            )
          )
        }else{
          return(
            facet_wrap_custom(
              ggplot2::vars(donnees[["Group"]]), ncol = as.numeric(univariateFacetsBoxplotOptions$facetNcol()), scales = "free_x",
              scale_overrides = facetScaleOverriding()
            )
          )
        }
      }

      # plot
      multivariateBplt <-
        ggplot2::ggplot(
          data = donnees, ggplot2::aes(x=Variable, y=Valeur, fill=Group)
        )+
        # barres d'erreur
        ggplot2::stat_boxplot(geom = "errorbar")+
        # boites à moustaches
        ggplot2::geom_boxplot(
          outlier.shape = as.integer(univariateFacetsBoxplotOptions$outlierShape()), outlier.alpha = 1,
          outlier.color = univariateFacetsBoxplotOptions$outlierColor(),
          outlier.size = as.numeric(univariateFacetsBoxplotOptions$outlierSize()),
          color = univariateFacetsBoxplotOptions$boxplotColor(),
          alpha =  as.numeric(univariateFacetsBoxplotOptions$boxplotAlpha()),
          width = as.numeric(univariateFacetsBoxplotOptions$boxplotsWidth())
        )+

        # Facets Scales
        facetScale()+

        # couleurs des variables
        manualColor() +

        # valeur Seuil
        valeurSeuil(donnees, "Seuil") +

        # pivotage du graphique
        flipCoord() +

        # personnalisation de la légende [nbre de lignes, taille des symboles, etc.]
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            nrow = as.numeric(univariateFacetsBoxplotOptions$legendNrow()),
            byrow = univariateFacetsBoxplotOptions$legendByRow(),
            keywidth = grid::unit(as.numeric(univariateFacetsBoxplotOptions$legendKeyWidth()), "cm"),
            keyheight = grid::unit(as.numeric(univariateFacetsBoxplotOptions$legendKeyHeight()), "cm")
          )
        )+

        # personnalisation des éléments du graphique
        ggplot2::theme(
          legend.direction =  univariateFacetsBoxplotOptions$legDir(),

          legend.text = ggplot2::element_text(
            family = "Times", size=as.numeric(univariateFacetsBoxplotOptions$LegTextSize()),
            color =  univariateFacetsBoxplotOptions$LegTextColor(),
            margin = ggplot2::margin(
              b=as.numeric(univariateFacetsBoxplotOptions$legTextMarginB()),
              l=as.numeric(univariateFacetsBoxplotOptions$legTextMarginL()),
              t=as.numeric(univariateFacetsBoxplotOptions$legTextMarginT()),
              r=as.numeric(univariateFacetsBoxplotOptions$legTextMarginR()),
              unit = "cm"
            ),
            face = "bold"
          ),

          legend.position =  leg_pos,
          #
          legend.margin = ggplot2::margin(
            b=as.numeric(univariateFacetsBoxplotOptions$legMarginB()),
            l=as.numeric(univariateFacetsBoxplotOptions$legMarginL()),
            t=as.numeric(univariateFacetsBoxplotOptions$legMarginT()),
            r=as.numeric(univariateFacetsBoxplotOptions$legMarginR()), unit = "cm"
          ),

          legend.box.background = ggplot2::element_rect(fill = "white", color = "white"),
          legend.key = ggplot2::element_blank(),

          # axes
          axis.text.x = ggplot2::element_text(
            family = "Times", size=as.numeric(univariateFacetsBoxplotOptions$xAxisTextSize()),
            color = univariateFacetsBoxplotOptions$axisTextColor(),
            angle=as.numeric(univariateFacetsBoxplotOptions$xAxisTextAngle()),
            hjust = as.numeric(univariateFacetsBoxplotOptions$xAxisTextHjust()),
            vjust = as.numeric(univariateFacetsBoxplotOptions$xAxisTextVjust())
          ),
          axis.ticks = ggplot2::element_line(
            colour=univariateFacetsBoxplotOptions$axisTicksColor(), size=as.numeric(univariateFacetsBoxplotOptions$axisTicksSize())
          ),
          axis.title = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(univariateFacetsBoxplotOptions$AxisTitleSize())
          ),
          # les grilles
          panel.grid.major = ggplot2::element_line(
            linetype = ifelse(length(univariateFacetsBoxplotOptions$panelGridLineType()),
                              as.numeric(univariateFacetsBoxplotOptions$panelGridLineType()),
                              univariateFacetsBoxplotOptions$panelGridLineType()),
            color = univariateFacetsBoxplotOptions$panelGridColor(), size=grid::unit(.23, "cm")
          ),
          panel.grid.minor = ggplot2::element_blank(),
          # texte de l'axe des y
          axis.text.y= ggplot2::element_text(
            family = "Times", size=as.numeric(univariateFacetsBoxplotOptions$yAxisTextSize()),
            color =  univariateFacetsBoxplotOptions$axisTextColor()
          ),
          # contours
          panel.background = ggplot2::element_rect(
            color=univariateFacetsBoxplotOptions$panelBackgroundLineColor(),
            fill="white", size=as.numeric(univariateFacetsBoxplotOptions$panelBackgroundLineSize())
          ),
          # arrière plan du graphique
          # panel.border = element_rect(color="gray", fill=NA, size = 1.3),
          # titre des facetsf
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(
            family = "Times", size = as.numeric(univariateFacetsBoxplotOptions$stripTextSize()), face="bold",
            margin = ggplot2::margin(
              b = univariateFacetsBoxplotOptions$stripMarginB(),  t = univariateFacetsBoxplotOptions$stripMarginT(),
              l = univariateFacetsBoxplotOptions$stripMarginL(),  r = univariateFacetsBoxplotOptions$stripMarginR(),
              unit =  univariateFacetsBoxplotOptions$stripMarginUnit()
            )
          ),
          # legend.key = element_rect(color = NA, fill = NA),
          panel.spacing.y = grid::unit(as.numeric(univariateFacetsBoxplotOptions$panelYspacing()), "cm"),
          panel.spacing.x = grid::unit(as.numeric(univariateFacetsBoxplotOptions$panelXspacing()), "cm")
        )+

        ggplot2::labs(
          x = univariateFacetsBoxplotOptions$xAxisTitle(),
          y = univariateFacetsBoxplotOptions$yAxisTitle(),
          fill = NULL, color = NULL
        )

      # résummé statistique
      statSum <- donnees %>%
        dplyr::group_by(Group, Variable) %>%
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

    output$univariateFacetsBoxplot <- renderPlot({
      req(univariateFacetsBoxplot())
      univariateFacetsBoxplot()[[1]]
    })

    output$statsSummary <- renderDataTable({
      req(univariateFacetsBoxplot())
      univariateFacetsBoxplot()[[2]]
    })

    ###  Exportation
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("univariateFacetsBoxplot-", Sys.Date(), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(file, univariateFacetsBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )

    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("univariateFacetsBoxplot-", Sys.Date(), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, univariateFacetsBoxplot()[[1]], width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_makingUnivariateFacetsBoxplot_ui("makingUnivariateFacetsBoxplot_1")

## To be copied in the server
# mod_makingUnivariateFacetsBoxplot_server("makingUnivariateFacetsBoxplot_1")
