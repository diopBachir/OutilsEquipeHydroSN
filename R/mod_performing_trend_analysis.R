#' performing_trend_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_performing_trend_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    tags$head(tags$style(type="text/css", '
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
            @keyframes spin10 { to { transform: translateY(-15.0em); } }
            ')),

    tags$head(tags$style(type="text/css", '.butt{background-color:#add8e6;} .butt{color: #337ab7;}')),
    tags$head(tags$style(type="text/css", '.process{background-color:khaki;} .process{color: khaki;}')),

    br(),

    fluidRow(align = "center",
             column(6, dipsaus::actionButtonStyled(ns("linearTrend"), label = "Tendance Linéaire", class= "", type="primary")),
             column(6, dipsaus::actionButtonStyled(ns("mkTest"), label = "Test De Mann-Kendall", class= "", type="primary")),
    ),
    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(12,
                    uiOutput(ns("stationFilter"))
             )
    )
  )
}

#' performing_trend_analysis Server Functions
#'
#' @noRd
mod_performing_trend_analysis_server <- function(id, TrendAnalysisData, stations, TrendAnalysisGraphsOptions, bassin){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # plot result
    TrendTestGraph<- reactiveVal()
    ForExcelTransfert<- reactiveVal()

    # prepare data
    dataPLT <- reactive({
      req(TrendAnalysisData, stations)
      TrendAnalysisData %>%
        tidyr::pivot_longer(-Date, names_to = "station", values_to = "variable") %>%
        dplyr::inner_join(stations, by = c("station"="Station"))
    })

    observeEvent(dataPLT(), {
      output$stationFilter<- renderUI({
        req(dataPLT())
        fluidRow(align="center",
                 column(12,
                        checkboxGroupInput(
                          ns("stationFilterCheckBox"), label = "Filtrer les stations qui doivent apparaître dans le graphique de la tendane linéaire !",
                          choices = unique(dataPLT()$station), selected = unique(dataPLT()$station)[1], width = "100%", inline = T
                        )
                 )
        )
      })
    })

    # plot result
    TrendTestGraph<- reactiveVal()
    pettitTestResult<- reactiveVal()
    ForExcelTransfert<- reactiveVal()

    # linear trend plot
    observeEvent(ignoreInit = T, ignoreNULL = T, input$linearTrend, {
      req(dataPLT())

      # modal dialog
      showModal(modalDialog(
        tags$h3("Tendance Linéaire", style="color:#3474A7;family:Georgia;text-align:left;"),
        fluidRow(
          column(12, plotOutput(ns("linearTrendPlot"), width = "100%"))
        ),
        footer=tagList(
          fluidRow(
            column(3, downloadButton(ns("exportLinearTrendPlotJPEG"), label="JPEG", icon = icon("download"), class= "butt")),
            column(3, downloadButton(ns("exportLinearTrendPlotSVG"), label="SVG", icon = icon("download"), class= "butt")),
            column(3, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      if(sum(input$stationFilterCheckBox %in% dataPLT()$station) > 0){
        readydataPLT<- dplyr::filter(dataPLT(), station %in% input$stationFilterCheckBox)
      }else{
        readydataPLT <- dataPLT()
      }

      # ------------------------------------------------------------------------------------------------------#
      ###Gestion du nombre de facets
      facetNrow <- function(){

        if(TrendAnalysisGraphsOptions$facetNrow() == 0){
          return(NULL)
        }else{
          return(as.numeric(TrendAnalysisGraphsOptions$facetNrow()))
        }
      }

      # tendance linéaire
      TrendTestGraph(
        readydataPLT %>%
          ggplot2::ggplot(ggplot2::aes(x=Date, y=variable, group=1))+
          ggplot2::scale_y_continuous(expand = c(.1,.1), n.breaks = 5)+
          ggplot2::scale_x_date(
            date_breaks = TrendAnalysisGraphsOptions$dateNbreaks(), date_labels = "%Y"
          )+
          ggplot2::geom_point(color="red", size=1.5)+
          ggplot2::geom_line(color="black") +
          ggplot2::geom_smooth(method = "lm", se=F, color="black")+

          ggplot2::facet_wrap(~forcats::fct_shuffle(station), ncol = facetNrow(),  scales = "free_y")+

          ggplot2::theme_bw()+
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_text(
              family = "Times", color = "black", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTitleSize())
            ),
            axis.text.x = ggplot2::element_text(
              family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTextSize()),
              color = TrendAnalysisGraphsOptions$axisTextColor(),
              hjust = as.numeric(TrendAnalysisGraphsOptions$xAxisTextHjust()),
              vjust = as.numeric(TrendAnalysisGraphsOptions$xAxisTextVjust()),
              angle = as.numeric(TrendAnalysisGraphsOptions$xAxisTextAngle())
            ),
            axis.text.y = ggplot2::element_text(
              family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTextSize()),
              color = TrendAnalysisGraphsOptions$axisTextColor()
            ),
            axis.ticks = ggplot2::element_line(
              colour=TrendAnalysisGraphsOptions$axisTicksColor(),
              size=as.numeric(TrendAnalysisGraphsOptions$axisTicksSize())
            ),
            panel.grid.major = ggplot2::element_line(
              ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                     as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                     TrendAnalysisGraphsOptions$panelGridLineType()),
              color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
            ),
            panel.grid.minor = ggplot2::element_line(
              ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                     as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                     TrendAnalysisGraphsOptions$panelGridLineType()),
              color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
            ),
            panel.background = ggplot2::element_rect(
              color=TrendAnalysisGraphsOptions$panelBackgroundLineColor(),
              fill="white", size=as.numeric(TrendAnalysisGraphsOptions$panelBackgroundLineSize())
            ),

            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              family = "Times", size = as.numeric(TrendAnalysisGraphsOptions$stripTextSize()), face="bold",
              margin = ggplot2::margin(
                b = as.numeric(TrendAnalysisGraphsOptions$stripMarginB()),  t = as.numeric(TrendAnalysisGraphsOptions$stripMarginT()),
                l = as.numeric(TrendAnalysisGraphsOptions$stripMarginL()),  r = as.numeric(TrendAnalysisGraphsOptions$stripMarginR()),
                unit =  TrendAnalysisGraphsOptions$stripMarginUnit()
              )
            ),
            # legend.key = element_rect(color = NA, fill = NA),
            panel.spacing.y = grid::unit(as.numeric(TrendAnalysisGraphsOptions$panelYspacing()), "cm"),
            panel.spacing.x = grid::unit(as.numeric(TrendAnalysisGraphsOptions$panelXspacing()), "cm"),

            plot.margin = ggplot2::margin(.5,.5,.5,.5, unit = "cm")
          ) +

          ggplot2::labs(x = NULL, y=TrendAnalysisGraphsOptions$yAxisTitle())
      )

      output$linearTrendPlot <- renderPlot({
        req(TrendTestGraph())
        TrendTestGraph()
      })

      ### Exporting result---------------------------------------------------------------#
      #* jpeg
      output$exportLinearTrendPlotJPEG <-  downloadHandler(
        filename = function() {
          paste("Graph-Résultats-AnalysedeTendance-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          ggplot2::ggsave(plot = TrendTestGraph(), filename = file,width = 13.3, height = 7.05)
        }
      )
      # svg
      output$exportLinearTrendPlotSVG <-  downloadHandler(
        filename = function() {
          paste("Graph-Résultats-AnalysedeTendance-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          ggplot2::ggsave(plot = TrendTestGraph(), filename = file, width = 13.3, height = 7.05)
        }
      )
    })

    # makk kendall trend trend test plot
    observeEvent(ignoreInit = T, ignoreNULL = T, input$mkTest, {
      req(dataPLT())

      # modal dialog
      showModal(modalDialog(
        tags$h3("Résutat[S] Du Test De Tendance", style="color:#3474A7;family:Georgia;text-align:left;"),
        fluidRow(
          column(12, verbatimTextOutput(ns("test"))),
          column(12, shinycssloaders::withSpinner(plotOutput(ns("TrendAnalysisPlot"), width = "100%")))
        ),
        footer=tagList(
          fluidRow(align = "center",
            column(4, dipsaus::actionButtonStyled(ns("SenSlope"), label="Pente De Sen", icon = icon("chart-line"), class= "", type = "primary")),
            column(4, dipsaus::actionButtonStyled(ns("krigeZValue"), label=span("Krigeage Des Valeurs Z", id=ns("krigeZValueAnimate")), icon = icon("microchip"), class= "", type = "info")),
            column(4, dipsaus::actionButtonStyled(ns("zValue"), label=span("Spatialisation Des Valeurs Z", id=ns("zValueAnimate")), icon = icon("map"), class= "", type = "info"))
          ),

          tags$hr(style="border-color:gray;"),

          div("Options Relatives A la Spatialisation des Valeurs Z !", style="text-align:center;color:blue;font-family:georgia;font-face:bold;font-size:130%"),

          fluidRow(align="right",
                   column(12, actionButtonStyled(ns("recoderVarLevels"), span("Recoder", id=ns("recoderVarLevelsAnimate")), class= "", type="primary")),
          ),
          fluidRow(align = "left",
                   column(12,
                          textInput(
                            ns("NewVarFactorLevel"), label = "Ordre des Types de Tendance dans la Légende",
                            value = "", placeholder = "i.e., level1;level2;level3;etc.",
                            width = "100%"
                          )
                   ),
                   column(12, verbatimTextOutput(ns("NewVarFactorLevelVect"))),
                   # column(2, numericInput(ns("zoomin"), label = "Zoom", min=2, max=25, step=.01, value = 6.00, width = "100%")),
                   # column(2, numericInput(ns("longAdjust"), label = "Long. Adj.", min=-6, max=6, step=.01, value = 0.00, width = "100%")),
                   # column(2, numericInput(ns("latAdjust"), label = "Lat. Adj.", min=-6, max=6, step=.01, value = 0.00, width = "100%")),
                   column(12,  textInput(ns("variableColors"), label = "Couleur des Types de Tendance", value = "", placeholder = "red;green;blue;...", width = "100%")),
                   column(12,  textInput(ns("xbreaks"), label = "Graduations de l'axe des longitudes {X}", value = "", placeholder = "x1;x2;x2;x4;...", width = "100%")),
                   column(12,  textInput(ns("ybreaks"), label = "Graduations de l'axe des latitudes {X}", value = "", placeholder = "x1;x2;x2;x4;...", width = "100%"))
          ),
          tags$hr(style="border-color:gray;"),

          fluidRow(align = "center",
            column(3, downloadButton(ns("exportJPEG"), label="JPEG", icon = icon("download"), class= "butt")),
            column(3, downloadButton(ns("exportSVG"), label="SVG", icon = icon("download"), class= "butt")),
            column(3, downloadButton(ns("mkTestToExcel"), label="Feuille Excel", icon = icon("file-excel"), class= "butt")),
            column(3, modalButton('Fermer', icon = icon("power-off")))
          )
        ),
        size = "l"
      ))

      # ------------------------------------------------------------------------------------------------------#
      ###Gestion du nombre de facets
      facetNrow <- function(){

        if(TrendAnalysisGraphsOptions$facetNrow() == 0){
          return(NULL)
        }else{
          return(as.numeric(TrendAnalysisGraphsOptions$facetNrow()))
        }
      }

      # Application du test de mann kendall
      mk_test_result <- dataPLT() %>%
        # groupement des station par nom
        dplyr::group_by(station)%>%
        # application de la fonction d'autocorrélation
        dplyr::summarise(
          list_MK = list(modifiedmk::mmky(variable))
        )%>%
        # convertion des valeurs de corrélation en objet Double
        dplyr::mutate(
          Z = purrr::map(list_MK, ~as.numeric(.x[1])),
          SenSlope = purrr::map(list_MK, ~as.numeric(.x[7])),
          p.value = purrr::map(list_MK, ~as.numeric(.x[2])),
          tau = purrr::map(list_MK, ~as.numeric(.x[6]))
        )%>%
        # suppreSHion de la colonne nesté {list_acf}
        dplyr::select(-list_MK) %>%
        # désimbrication de la colonne {acf_vals}
        tidyr::unnest(cols = c(Z, SenSlope, p.value, tau))%>%
        #jointure
        fuzzyjoin::regex_inner_join(#jointure interne
          dataPLT(), by = "station"
        )%>%
        dplyr::ungroup() %>% dplyr::rename(Station = station.x)  %>%
        dplyr::select(Station, Longitude, Latitude,  Z, SenSlope, p.value, tau) %>%
        dplyr::group_by(Station) %>%
        dplyr::slice(1)

      # Application du test de pettitt
      donnees_orig <-  dataPLT() %>%
        mutate(Annee = lubridate::year(Date)) %>%
        dplyr::distinct()

      pettitTestResult(
        dataPLT() %>%
          # na.omit() %>%
          dplyr::group_by(station) %>% # groupement des station par code ADHI
          dplyr::summarise(list_PT = list(trend::pettitt.test(variable))) %>% # application du test
          # convertion des valeurs retournées par le test en objet {{Double}}
          dplyr::mutate(
            pettitt.value = purrr::map(list_PT, ~as.numeric(.x[[3]])),
            p.value = purrr::map(list_PT, ~as.numeric(.x[[4]]))
          ) %>%
          dplyr::select(-list_PT) %>% # suppression de la colonne nesté {list_MK}
          tidyr::unnest(cols = c(pettitt.value, p.value)) %>% # désimbrication des colonnes d'intêrets
          #jointure
          fuzzyjoin::regex_inner_join(#jointure interne
            dataPLT(), by = "station"
          )%>%
          dplyr::ungroup() %>% dplyr::rename(Station = station.x)  %>%
          dplyr::select(Station, Longitude, Latitude,  pettitt.value, p.value) %>%
          dplyr::group_by(Station) %>%
          # filter(p.value <= .05) %>%
          # extraction des années de rupture
          dplyr::mutate(
            an.rupture = (
              min(donnees_orig$Annee):max(donnees_orig$Annee)
            )[pettitt.value]
          ) %>%
          dplyr::group_by(Station) %>%
          dplyr::slice(1) %>%
          dplyr::select(Station,  Longitude, Latitude, "Annee Rupture" = an.rupture, p.value)
      )

      # pour l'envoi vers excel
      ForExcelTransfert(
        list(
          "Mann-Kendall" = mk_test_result %>%
            dplyr::mutate(
              seuil.p.value = cut(
                p.value, breaks = c(0, 0.05, 0.1, 1), labels = c("p<0.05", "0.05<p<0.10", "p>0.10")
              ),
              direction = ordered(
                ifelse(SenSlope > 0, "Ascendant", "Descendant"), levels = c("Ascendant", "Descendant")
              ),
              significativité =  case_when(
                Z < -1.96 ~" Tendance Négative Significative",
                Z > 1.96 ~ "Tendance Postive Significative",
                TRUE ~ "Aucune Tendance Significative"
              )
            ),
          "Pettitt" = pettitTestResult()
        )
      )

      # graph sen slope
      # custom palette
      palett <- gplots::rich.colors(100, palette = "temperature", rgb = F)

      TrendTestGraph(
        mk_test_result %>%
          dplyr::mutate(
            seuil.p.value = cut(
              p.value, breaks = c(0, 0.05, 0.1, 1), labels = c("p<0.05", "0.05<p<0.10", "p>0.10")
            ),
            direction = ordered(
              ifelse(SenSlope > 0, "Ascendant", "Descendant"), levels = c("Ascendant", "Descendant")
            ),
            significativité =  case_when(
              Z < -1.96 ~" Tendance Négative Significative",
              Z > 1.96 ~ "Tendance Postive Significative",
              TRUE ~ "Aucune Tendance Significative"
            )
          ) %>%
          ggplot2::ggplot(aes(SenSlope, Station)) +
          ggplot2::geom_vline(xintercept = 0) +
          ggplot2::geom_segment(aes(x = 0, xend=SenSlope, yend=Station)) +
          # geom_point(size=4, color='white') +
          ggplot2::geom_point(
            ggplot2::aes(
              shape=direction, fill=SenSlope
            ), size = 4
          ) +
          ggplot2::geom_text(
            vjust = .7, nudge_x = 0, size = 3,
            ggplot2::aes(x=ifelse(SenSlope>0,  SenSlope+.20, SenSlope-.20), y=Station, label = paste0("p = ", round(p.value, 8)))
          ) +
          ggplot2::scale_y_discrete(expand = c(.03, .03)) +
          ggplot2::scale_x_continuous(
            breaks = seq(min(mk_test_result$SenSlope), max(mk_test_result$SenSlope), length.out=5),
            labels = round(seq(min(mk_test_result$SenSlope), max(mk_test_result$SenSlope), length.out=5), 2)
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              barwidth=1, barheight=17, title.position = "left",
              title.hjust = .5, ticks.colour = "black", ticks.linewidth = .7,
              title.theme = ggplot2::element_text(size = 13, angle = 90),
              width=unit(6,"cm"), title.vjust = .5, order = 2,
              frame.linewidth = 1, frame.colour = "gray", title = "Pente de Sen [Sen Slope]"
            )
          )+
          ggplot2::scale_fill_gradientn(
            colours =  rev(palett),
            # palette = "Spectral", direction = 1,
            limits = c(
              min(mk_test_result$SenSlope),
              max(mk_test_result$SenSlope)
            ),
            breaks = round(
              seq(
                min(mk_test_result$SenSlope),
                max(mk_test_result$SenSlope),
                length.out = 8
              ), 4
            ),
            labels = format(round(seq(
              min(mk_test_result$SenSlope),
              max(mk_test_result$SenSlope),
              length.out = 8
            ), 4), decimal.mark=",")
          )+
          ggplot2::scale_shape_manual('Direction de Pente', values=c(24, 25), labels = c("Ascendant", "Descendant")) +

          ggplot2::theme_bw()+
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(
              family = "Times", color = "black", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTitleSize())
            ),
            axis.text.x = ggplot2::element_text(
              family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTextSize()),
              color = TrendAnalysisGraphsOptions$axisTextColor(),
              hjust = as.numeric(TrendAnalysisGraphsOptions$xAxisTextHjust())
            ),
            axis.text.y = ggplot2::element_text(
              family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTextSize()),
              color = TrendAnalysisGraphsOptions$axisTextColor()
            ),
            axis.ticks = ggplot2::element_line(
              colour=TrendAnalysisGraphsOptions$axisTicksColor(),
              size=as.numeric(TrendAnalysisGraphsOptions$axisTicksSize())
            ),
            legend.text = ggplot2::element_text(family = "Times", size = 12, margin = ggplot2::margin(l=.02, unit="cm")),
            legend.title =  ggplot2::element_text(family = "Times", size = 12, face = "bold"),
            panel.grid.major = ggplot2::element_line(
              ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                     as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                     TrendAnalysisGraphsOptions$panelGridLineType()),
              color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
            ),
            panel.grid.minor = ggplot2::element_line(
              ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                     as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                     TrendAnalysisGraphsOptions$panelGridLineType()),
              color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
            ),
            panel.background = ggplot2::element_rect(
              color=TrendAnalysisGraphsOptions$panelBackgroundLineColor(),
              fill="white", size=as.numeric(TrendAnalysisGraphsOptions$panelBackgroundLineSize())
            )
          )
      )

      # affichage
      output$TrendAnalysisPlot <- renderPlot({
        req(TrendTestGraph())
        TrendTestGraph()
      })

      ### Exporting result---------------------------------------------------------------#
      #* jpeg
      output$exportJPEG <-  downloadHandler(
        filename = function() {
          paste("Graph-Résultats-AnalysedeTendance-PenteDeSen", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
        },
        content = function(file) {
          ggplot2::ggsave(plot = TrendTestGraph(), filename = file,width = 13.3, height = 7.05)
        }
      )
      # svg
      output$exportSVG <-  downloadHandler(
        filename = function() {
          paste("Graph-Résultats-AnalysedeTendance-PenteDeSen", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
        },
        content = function(file) {
          ggplot2::ggsave(plot = TrendTestGraph(), filename = file, width = 13.3, height = 7.05)
        }
      )

      # exportation des résultats vers excel
      output$mkTestToExcel <-  downloadHandler(
        filename = function() {
          paste("Résultats-AnalyseDeTendance-MannKendall-Pettitt", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(ForExcelTransfert(), file)
        }
      )

      # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      # GESTION DES BOUTTONS INTERNE AU MODAL

      # ------------------------------------------------------------------------------#
      # PENTE DE SEN
      observeEvent(ignoreInit = T, ignoreNULL = T, input$SenSlope, {
        req(dataPLT())

        # ------------------------------------------------------------------------------------------------------#
        ###Gestion du nombre de facets
        facetNrow <- function(){

          if(TrendAnalysisGraphsOptions$facetNrow() == 0){
            return(NULL)
          }else{
            return(as.numeric(TrendAnalysisGraphsOptions$facetNrow()))
          }
        }

        # Application du test de mann kendall
        mk_test_result <- dataPLT() %>%
          # groupement des station par nom
          dplyr::group_by(station)%>%
          # application de la fonction d'autocorrélation
          dplyr::summarise(
            list_MK = list(modifiedmk::mmky(variable))
          )%>%
          # convertion des valeurs de corrélation en objet Double
          dplyr::mutate(
            Z = purrr::map(list_MK, ~as.numeric(.x[1])),
            SenSlope = purrr::map(list_MK, ~as.numeric(.x[7])),
            p.value = purrr::map(list_MK, ~as.numeric(.x[2])),
            tau = purrr::map(list_MK, ~as.numeric(.x[6]))
          )%>%
          # suppreSHion de la colonne nesté {list_acf}
          dplyr::select(-list_MK) %>%
          # désimbrication de la colonne {acf_vals}
          tidyr::unnest(cols = c(Z, SenSlope, p.value, tau))%>%
          #jointure
          fuzzyjoin::regex_inner_join(#jointure interne
            dataPLT(), by = "station"
          )%>%
          dplyr::ungroup() %>% dplyr::rename(Station = station.x)  %>%
          dplyr::select(Station, Longitude, Latitude,  Z, SenSlope, p.value, tau) %>%
          dplyr::group_by(Station) %>%
          dplyr::slice(1)

        # graph sen slope
        # custom palette
        palett <- gplots::rich.colors(100, palette = "temperature", rgb = F)

        TrendTestGraph(
          mk_test_result %>%
            dplyr::mutate(
              seuil.p.value = cut(
                p.value, breaks = c(0, 0.05, 0.1, 1), labels = c("p<0.05", "0.05<p<0.10", "p>0.10")
              ),
              direction = ordered(
                ifelse(SenSlope > 0, "Ascendant", "Descendant"), levels = c("Ascendant", "Descendant")
              ),
              significativité =  case_when(
                Z < -1.96 ~" Tendance Négative Significative",
                Z > 1.96 ~ "Tendance Postive Significative",
                TRUE ~ "Aucune Tendance Significative"
              )
            ) %>%
            ggplot2::ggplot(aes(SenSlope, Station)) +
            ggplot2::geom_vline(xintercept = 0) +
            ggplot2::geom_segment(aes(x = 0, xend=SenSlope, yend=Station)) +
            # geom_point(size=4, color='white') +
            ggplot2::geom_point(
              ggplot2::aes(
                shape=direction, fill=SenSlope
              ), size = 4
            ) +
            ggplot2::geom_text(
              vjust = .7, nudge_x = 0, size = 3,
              ggplot2::aes(x=ifelse(SenSlope>0, SenSlope+.20, SenSlope-.20), y=Station, label = paste0("p = ", round(p.value, 8)))
            ) +
            ggplot2::scale_y_discrete(expand = c(.03, .03)) +
            ggplot2::scale_x_continuous(
              breaks = seq(min(mk_test_result$SenSlope), max(mk_test_result$SenSlope), length.out=5),
              labels = round(seq(min(mk_test_result$SenSlope), max(mk_test_result$SenSlope), length.out=5), 2)
            ) +
            ggplot2::guides(
              fill = ggplot2::guide_colorbar(
                barwidth=1, barheight=17, title.position = "left",
                title.hjust = .5, ticks.colour = "black", ticks.linewidth = .7,
                title.theme = ggplot2::element_text(size = 13, angle = 90),
                width=unit(6,"cm"), title.vjust = .5, order = 2,
                frame.linewidth = 1, frame.colour = "gray", title = "Pente de Sen [Sen Slope]"
              )
            )+
            ggplot2::scale_fill_gradientn(
              colours =  rev(palett),
              # palette = "Spectral", direction = 1,
              limits = c(
                min(mk_test_result$SenSlope),
                max(mk_test_result$SenSlope)
              ),
              breaks = round(
                seq(
                  min(mk_test_result$SenSlope),
                  max(mk_test_result$SenSlope),
                  length.out = 8
                ), 4
              ),
              labels = format(round(seq(
                min(mk_test_result$SenSlope),
                max(mk_test_result$SenSlope),
                length.out = 8
              ), 4), decimal.mark=",")
            )+
            ggplot2::scale_shape_manual('Direction de Pente', values=c(24, 25), labels = c("Ascendant", "Descendant")) +

            ggplot2::theme_bw()+
            ggplot2::theme(
              axis.title.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_text(
                family = "Times", color = "black", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTitleSize())
              ),
              axis.text.x = ggplot2::element_text(
                family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTextSize()),
                color = TrendAnalysisGraphsOptions$axisTextColor(),
                hjust = as.numeric(TrendAnalysisGraphsOptions$xAxisTextHjust())
              ),
              axis.text.y = ggplot2::element_text(
                family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTextSize()),
                color = TrendAnalysisGraphsOptions$axisTextColor()
              ),
              axis.ticks = ggplot2::element_line(
                colour=TrendAnalysisGraphsOptions$axisTicksColor(),
                size=as.numeric(TrendAnalysisGraphsOptions$axisTicksSize())
              ),
              legend.text = ggplot2::element_text(family = "Times", size = 12, margin = ggplot2::margin(l=.02, unit="cm")),
              legend.title =  ggplot2::element_text(family = "Times", size = 12, face = "bold"),
              panel.grid.major = ggplot2::element_line(
                ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                       as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                       TrendAnalysisGraphsOptions$panelGridLineType()),
                color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
              ),
              panel.grid.minor = ggplot2::element_line(
                ifelse(length(TrendAnalysisGraphsOptions$panelGridLineType()),
                       as.numeric(TrendAnalysisGraphsOptions$panelGridLineType()),
                       TrendAnalysisGraphsOptions$panelGridLineType()),
                color = TrendAnalysisGraphsOptions$panelGridColor(), size=unit(.13, "cm")
              ),
              panel.background = ggplot2::element_rect(
                color=TrendAnalysisGraphsOptions$panelBackgroundLineColor(),
                fill="white", size=as.numeric(TrendAnalysisGraphsOptions$panelBackgroundLineSize())
              )
            )
        )

        # affichage
        output$TrendAnalysisPlot <- renderPlot({
          req(TrendTestGraph())
          TrendTestGraph()
        })

        ### Exporting result---------------------------------------------------------------#
        #* jpeg
        output$exportJPEG <-  downloadHandler(
          filename = function() {
            paste("Graph-Résultats-AnalysedeTendance-PenteDeSen", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            ggplot2::ggsave(plot = TrendTestGraph(), filename = file,width = 13.3, height = 7.05)
          }
        )
        # svg
        output$exportSVG <-  downloadHandler(
          filename = function() {
            paste("Graph-Résultats-AnalysedeTendance-PenteDeSen", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            ggplot2::ggsave(plot = TrendTestGraph(), filename = file, width = 13.3, height = 7.05)
          }
        )

      }) # PENTE DE SEN

      # ------------------------------------------------------------------------------#
      # KRIGEAGE DES VALEURS Z
      # stocker les résultats
      result_kriging <- reactiveVal()

      observeEvent(ignoreInit = T, ignoreNULL = T, input$krigeZValue, {
        req(dataPLT(), bassin, stations)


        # setting buttons with shinyjs
        shinyjs::addClass(id = "krigeZValueAnimate", class = "loading dots")
        shinyjs::disable("krigeZValue")

        # Notification
        id <- showNotification(
          paste0(
            "Interpolation par krigeage ... Peut prendre un certain temps en fonction du nombre de sites|stations et ",
            "de la superficie du bassin ..."
          ),
          duration = NULL, closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        # Application du test de mann kendall
        mk_test_result <- dataPLT() %>%
          # groupement des station par nom
          dplyr::group_by(station)%>%
          # application de la fonction d'autocorrélation
          dplyr::summarise(
            list_MK = list(modifiedmk::mmky(variable))
          )%>%
          # convertion des valeurs de corrélation en objet Double
          dplyr::mutate(
            Z = purrr::map(list_MK, ~as.numeric(.x[1])),
            SenSlope = purrr::map(list_MK, ~as.numeric(.x[7])),
            p.value = purrr::map(list_MK, ~as.numeric(.x[2])),
            tau = purrr::map(list_MK, ~as.numeric(.x[6]))
          )%>%
          # suppreSHion de la colonne nesté {list_acf}
          dplyr::select(-list_MK) %>%
          # désimbrication de la colonne {acf_vals}
          tidyr::unnest(cols = c(Z, SenSlope, p.value, tau))%>%
          #jointure
          fuzzyjoin::regex_inner_join(#jointure interne
            dataPLT(), by = "station"
          )%>%
          dplyr::ungroup() %>% dplyr::rename(Station = station.x)  %>%
          dplyr::select(Station, Longitude, Latitude,  Z, SenSlope, p.value, tau) %>%
          dplyr::group_by(Station) %>%
          dplyr::slice(1)  %>%
          dplyr::mutate(
            seuil.p.value = cut(
              p.value, breaks = c(0, 0.05, 0.1, 1), labels = c("p<0.05", "0.05<p<0.10", "p>0.10")
            ),
            direction = ordered(
              ifelse(SenSlope > 0, "Ascendant", "Descendant"), levels = c("Ascendant", "Descendant")
            ),
            significativite =  case_when(
              Z < -1.96 ~" Tendance Négative Significative",
              Z > 1.96 ~ "Tendance Postive Significative",
              TRUE ~ "Aucune Tendance Significative"
            )
          )

        #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
        # | INTERPOLATION PAR KRIGEAGE---------------------------------------------------------------------#
        # données
        data4plot<- mk_test_result %>%
          dplyr::select(Station, Z)

        # gridded cell points in WGS84-4326 proj
        Station.in.wgs84<-  prec_grid_georef(stations, "+init=epsg:4326")[[1]]

        # Définition du modèle de la grille d'interpolation spatiale
        grid.model<-  grid_def(
          bassin,  .01, 0, "+init=epsg:4326"
        )

        # géoréférencement des données
        interpolationData.WGS<-  data_cleaning_categorical(data4plot, Station.in.wgs84)

        # interpolation
        result_kriging(
          krige_static(interpolationData.WGS, grid.model, bassin, 4326) %>%
            dplyr::mutate(
              TypeDeTendance = case_when(
                Z < -1.96 ~ "Tendance Négative Significative",
                Z > 1.96 ~ "Tendance Positive Significative",
                TRUE ~ "Aucune Tendance Significative",
              ),
              TypeDeTendance = factor(TypeDeTendance)
            )
        )

        # Button settings
        shinyjs::enable("krigeZValue")
        shinyjs::removeClass(id = "krigeZValueAnimate", class = "loading dots")

        req(result_kriging())
        showNotification(
          paste0(
            "Notification Sortie Krigeage ! ",
            "Interpolation par krigeage terminée avec succès. Cliquer sur le boutton {Spatialisation Des Valeurs Z} ",
            "pour afficher la carte."
          ),
          duration = 5, closeButton = FALSE
        )

        # Button settings
        shinyjs::disable("krigeZValue")
        # shinyjs::removeClass(id = "krigeZValueAnimate", class = "loading dots")

        # Variable Levels
        output$NewVarFactorLevelVect<- renderPrint({
          req(result_kriging())
          levels(result_kriging()$TypeDeTendance)
        })

      }) # KRIGEAGE DES VALEURS Z

      # recodage des levels des types de tendance
      # # ------------------------------------------------------------------------------------------------------#
      # Rcoder les levels du facteur des noms de Variables
      observeEvent(input$recoderVarLevels, {

        #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
        # gestion des entrées vecteurs
        extract_variables_entries<-  function(text){
          # text<-  gsub(" ", "", text)
          split<-  strsplit(text, ";", fixed = FALSE)[[1]]
          split
        }

        req(result_kriging())

        if(input$NewVarFactorLevel!=""){
          new_values<- extract_variables_entries(input$NewVarFactorLevel)

          # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
          len_vect<- length(new_values) == length(unique(result_kriging()$TypeDeTendance))
          if(len_vect){# si les longueurs sont identiques

            shinyFeedback::hideFeedback("NewVarFactorLevel")

            donnees<- data.frame(result_kriging())

            # tester si les noms fournis sont présents dans la table à recoder
            testVar<- length(new_values) == sum(new_values %in% levels(donnees$TypeDeTendance))
            shinyFeedback::feedbackWarning("NewVarFactorLevel", !testVar, "Valeurs Incorrectes !")
            if(testVar==FALSE){
              shinyalert::shinyalert(
                "Valeurs d'entrée Incorrectes !",
                paste0(
                  "Vous devez vous assurer que tous les noms de variables fournis dans le champs ",
                  "{ Ordre des Types de Tendance dans la Légende } sont biens présents dans la liste des Type de Tendance !"
                )
              )
            }

            req(testVar)
            donnees$TypeDeTendance<- factor(donnees$TypeDeTendance, levels = new_values)
            result_kriging(donnees)

            # New Variable Levels
            output$NewVarFactorLevelVect<- renderPrint({
              req(result_kriging())
              levels(result_kriging()$TypeDeTendance)
            })

          }else{# si les longueurs ne sont pas identiques

            shinyFeedback::feedbackWarning("NewVarFactorLevel", !len_vect, "Longueurs Des Vecteurs Non Identiques !")
            shinyalert::shinyalert(
              "Longueurs De Vecteurs !",
              paste0("OUPS !! Vous devez entrer un vecteur de même longueur que { levels(column{{TypeDeTendance}}) } !")
            )

          }
        }

      })

      # ------------------------------------------------------------------------------#
      # SPATIALISATION DES VALEURS Z
      observeEvent(ignoreInit = T, ignoreNULL = T, input$zValue, {
        req(result_kriging(), bassin)

        # gestion des entrées vecteurs [string]
        extract_variables_entries<-  function(text){
          # text<-  gsub(" ", "", text)
          split<-  strsplit(text, ";", fixed = FALSE)[[1]]
          split
        }

        # gestion des entrées vecteurs [int]
        extract_entries<-  function(text){
          text<-  gsub(" ", "", text)
          split<-  strsplit(text, ";", fixed = FALSE)[[1]]
          as.numeric(split)
        }

        # Button settings
        shinyjs::disable("zValue")
        shinyjs::addClass(id = "zValueAnimate", class = "loading dots")

        # # ------------------------------------------------------------------------------------------------------#
        # ### TYPE DE POSITIONNEMENT DE LA LEGENDE
        if(TrendAnalysisGraphsOptions$legPosType() == "Côté"){
          leg_pos <- TrendAnalysisGraphsOptions$legLoc()
        }else{
          leg_pos <- c(
            as.numeric(TrendAnalysisGraphsOptions$legPosXcoord()),
            as.numeric(TrendAnalysisGraphsOptions$legPosYcoord())
          )
        }

        # Couleur des catégorie de tendance
        # ------------------------------------------------------------------------------------------------------#
        manualFill<-  function(){
          # Validation des entrées vecteurs
          if(input$variableColors!=""){
            variables_colors<- extract_variables_entries(input$variableColors)

            # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
            len_vect<- length(variables_colors) == length(unique(result_kriging()$TypeDeTendance))
            if(len_vect){# si les longueurs sont identiques

              # tester si les noms originaux fournis sont présents dans la table es colors
              testColor<- length(variables_colors) == sum(variables_colors %in% colors())
              if(testColor==FALSE){
                shinyFeedback::feedbackWarning("variableColors", testColor==FALSE, "Valeurs Incorrectes !")
                shinyalert::shinyalert(
                  "Erreur dans le vecteur de couleurs fournis !",
                  paste0(
                    "Vous devez vous assurer que tous les noms de Couleurs fournis dans la liste ",
                    "{ Couleur des Types de Tendance } sont biens présents dans la table des couleurs définis dans { colors() } !"
                  )
                )

                return(ggplot2::scale_fill_manual(values = c("khaki", "orange", "dodgerblue")))

              }else{
                return(ggplot2::scale_fill_manual(values = variables_colors))
              }

            }else{# si les longueurs ne sont pas identiques

              shinyalert::shinyalert(
                "Erreur dans le vecteur de couleurs fournis !",
                paste0("OUPS !! Vous devez spécifier autant de couleurs qu'il y'a de Types De Tendance !")
              )

              return(ggplot2::scale_fill_manual(values = c("khaki", "orange", "dodgerblue")))
            }
          }else{
            return(ggplot2::scale_fill_manual(values = c("khaki", "orange", "dodgerblue")))
          }
        }

        manualColor<-  function(){
          # Validation des entrées vecteurs
          if(input$variableColors!=""){
            variables_colors<- extract_variables_entries(input$variableColors)

            # tester si l'utilisateur a fourni des vecteurs de longueurs identiques
            len_vect<- length(variables_colors) == length(unique(result_kriging()$TypeDeTendance))
            if(len_vect){# si les longueurs sont identiques

              # tester si les noms originaux fournis sont présents dans la table es colors
              testColor<- length(variables_colors) == sum(variables_colors %in% colors())
              if(testColor==FALSE){
                shinyFeedback::feedbackWarning("variableColors", testColor==FALSE, "Valeurs Incorrectes !")
                shinyalert::shinyalert(
                  "Erreur dans le vecteur de couleurs fournis !",
                  paste0(
                    "Vous devez vous assurer que tous les noms de Couleurs fournis dans la liste ",
                    "{ Couleur des Types de Tendance } sont biens présents dans la table des couleurs définis dans { colors() } !"
                  )
                )

                return(ggplot2::scale_color_manual(values = c("khaki", "orange", "dodgerblue")))

              }else{
                return(ggplot2::scale_color_manual(values = variables_colors))
              }

            }else{# si les longueurs ne sont pas identiques

              shinyalert::shinyalert(
                "Erreur dans le vecteur de couleurs fournis !",
                paste0("OUPS !! Vous devez spécifier autant de couleurs qu'il y'a de Types De Tendance !")
              )

              return(ggplot2::scale_color_manual(values = c("khaki", "orange", "dodgerblue")))
            }
          }else{
            return(ggplot2::scale_color_manual(values = c("khaki", "orange", "dodgerblue")))
          }
        }

        # xticks  definition start ----------------------------- -------------------------------------------------------------#
        if(input$xbreaks==""){
          x_ticks<- NA
        } else {
          x_ticks<- extract_entries(input$xbreaks)
        }
        # filtrage
        if(!anyNA(x_ticks)){
          x_ticks_fn<- round(x_ticks, 2)
        }else{
          if(input$xbreaks!=""){
            # feedbacks
            shinyFeedback::feedbackWarning(
              "xbreaks", anyNA(x_ticks),
              "Vecteur de données incorrect !"
            )
            ## alert
            shinyalert::shinyalert(
              "Valeur incorrecte pour le champs {Graduations de l'axe des longitudes {X}} :",
              paste0(
                "Veillez spécifier un vecteur de nombre de la forme d'une ligne d'un fichier {CSV} avec le virgule ",
                "comme séparateur de colonnes selon le modèle {n1;n2;n3;etc.}. Le séparateur décimal doit être ",
                "un point virgule (et non une virgule comme le système français). Exemple Correcte: { 14;23.7;44;18.3 }"
              )
            )
          }

          x_ticks_fn<- round(seq(st_bbox(bassin)[1], st_bbox(bassin)[3], length.out=4), 2)

        }
        # xticks  definition start ----------------------------- -------------------------------------------------------------#

        # yticks  definition start ----------------------------- -------------------------------------------------------------#
        if(input$ybreaks==""){
          y_ticks<- NA
        } else {
          y_ticks<- extract_entries(input$ybreaks)
        }
        # filtrage
        if(!anyNA(y_ticks)){
          y_ticks_fn<- round(y_ticks, 2)
        }else{
          if(input$ybreaks!=""){
            # feedbacks
            shinyFeedback::feedbackWarning(
              "ybreaks", anyNA(y_ticks),
              "Vecteur de données incorrect !"
            )
            ## alert
            shinyalert::shinyalert(
              "Valeur incorrecte pour le champs {Graduations de l'axe des latitudes {Y}} :",
              paste0(
                "Veillez spécifier un vecteur de nombre de la forme d'une ligne d'un fichier {CSV} avec le virgule ",
                "comme séparateur de colonnes selon le modèle {n1;n2;n3;etc.}. Le séparateur décimal doit être ",
                "un point virgule (et non une virgule comme le système français). Exemple Correcte: { 14;23.7;44;18.3 }"
              )
            )
          }

          y_ticks_fn<- round(seq(st_bbox(bassin)[2], st_bbox(bassin)[4], length.out=4), 2)

        }
        # ybreaks definition end ----------------------------- -------------------------------------------------------------#

        # remove feedbacks
        if(!anyNA(x_ticks)){ shinyFeedback::hideFeedback("xbreaks") }
        if(!anyNA(y_ticks)){ shinyFeedback::hideFeedback("ybreaks") }

        # # centre de zoom
        # zoomCenter <-  c(
        #   sf::st_coordinates(st_centroid(bassin))[1] + as.numeric(input$longAdjust),
        #   sf::st_coordinates(st_centroid(bassin))[2] + as.numeric(input$latAdjust)
        # )

        # zoom
        # # Niveau de zoom
        # zoomLevel<- function(){
        #   if(input$zoomin != 0) {
        #     return(
        #       ggplot2::coord_sf(
        #         expand = TRUE, label_graticule = "SW", crs = 4326,
        #         # étendue des longitudes
        #         xlim = zoomMap(zoomCenter, as.numeric(input$zoomin))[[1]],
        #         # étendue des latitudes
        #         ylim = zoomMap(zoomCenter,  as.numeric(input$zoomin))[[2]]
        #       )
        #     )
        #   }else{
        #     return(
        #       ggplot2::coord_sf(
        #         expand = TRUE, label_graticule = "SW", crs = 4326
        #       )
        #     )
        #   }
        # }

        TrendTestGraph(
          ggplot2::ggplot() +
            # spatialisation des valeurs Z
            ggplot2::geom_tile(
              data=result_kriging(), aes(x=X , y=Y, fill=TypeDeTendance, color=TypeDeTendance), size=.6
            ) +
            # limit bassin
            ggplot2::geom_sf(data=bassin, linewidth=1.3, fill=NA, color="black") +
            # axis breaks
            ggplot2::scale_x_continuous(breaks = x_ticks_fn) +
            ggplot2::scale_y_continuous(breaks = y_ticks_fn) +
            ## zoom map
            # zoomLevel()+
            # légende des Types de tendance
            manualFill() +
            manualColor() +
            # personnalisation de la légende [nbre de lignes, taille des symboles, etc.]
            ggplot2::guides(
              fill = ggplot2::guide_legend(
                nrow = 1, byrow = TRUE, keywidth = grid::unit(.7, "cm"), keyheight = grid::unit(.5, "cm")
              ),
              color = "none"
            ) +
            # ajout de la flèche Nord
            northArrow(
              location = TrendAnalysisGraphsOptions$northArrowLocation(), width=as.numeric(TrendAnalysisGraphsOptions$northArrowWidth()),
              height = as.numeric(TrendAnalysisGraphsOptions$northArrowHeight()), padx=as.numeric(TrendAnalysisGraphsOptions$northArrowPadx()),
              pady= as.numeric(TrendAnalysisGraphsOptions$northArrowPady())
            )+
            # ajout de l'échelle
            scaleBar(
              location=TrendAnalysisGraphsOptions$scaleLocation(),
              height= as.numeric(TrendAnalysisGraphsOptions$scaleTickHeight()), width.hint=as.numeric(TrendAnalysisGraphsOptions$scaleWidthHint()),
              text.cex = as.numeric(TrendAnalysisGraphsOptions$scaleTextCex()),  scale.style=TrendAnalysisGraphsOptions$scaleStyle()
              # line.width = as.numeric(input$scaleLineWidth)
            ) +
            # personnalisation des éléments du graphique
            ggplot2::theme(
              legend.direction =  TrendAnalysisGraphsOptions$legDir(),

              legend.text = ggplot2::element_text(
                family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$LegTextSize()),
                color =  TrendAnalysisGraphsOptions$LegTextColor(),
                margin = ggplot2::margin(
                  b=as.numeric(TrendAnalysisGraphsOptions$legTextMarginB()),
                  l=as.numeric(TrendAnalysisGraphsOptions$legTextMarginL()),
                  t=as.numeric(TrendAnalysisGraphsOptions$legTextMarginT()),
                  r=as.numeric(TrendAnalysisGraphsOptions$legTextMarginR()),
                  unit = "cm"
                )
              ),
              # position de la légend
              legend.position =  leg_pos,
              # marge de la légende
              legend.margin = ggplot2::margin(
                b=as.numeric(TrendAnalysisGraphsOptions$legMarginB()), l=as.numeric(TrendAnalysisGraphsOptions$legMarginL()),
                t=as.numeric(TrendAnalysisGraphsOptions$legMarginT()), r=as.numeric(TrendAnalysisGraphsOptions$legMarginR()), unit = "cm"
              ),
              legend.box.background = ggplot2::element_rect(fill = "white", color = "white"),
              legend.key = ggplot2::element_blank(),

              # axes
              axis.text.x =  ggplot2::element_text(
                family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTextSize()),
                color = TrendAnalysisGraphsOptions$axisTextColor(),
                hjust = .5, vjust = .5
              ),

              axis.text.y=  ggplot2::element_text(
                family = "Times", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTextSize()),
                color =  TrendAnalysisGraphsOptions$axisTextColor()
              ),

              axis.ticks =  ggplot2::element_line(
                colour=TrendAnalysisGraphsOptions$axisTicksColor(),
                size=as.numeric(TrendAnalysisGraphsOptions$axisTicksSize())
              ),

              # les grilles
              panel.grid.major =  ggplot2::element_line(
                linetype = 1, color = "gray", size=grid::unit(.23, "cm")
              ),
              panel.grid.minor =  ggplot2::element_blank(),

              # contours
              panel.background =  ggplot2::element_rect(
                color=TrendAnalysisGraphsOptions$panelBackgroundLineColor(), fill="white",
                linewidth=as.numeric(TrendAnalysisGraphsOptions$panelBackgroundLineSize())
              )

            )+
            ggplot2::labs(
              x = NULL, y = NULL, fill = NULL, color = NULL
            )
        )

        # affichage
        output$TrendAnalysisPlot <- renderPlot({
          req(TrendTestGraph())
          TrendTestGraph()
        }, res = 57)

        # Button settings
        shinyjs::enable("zValue")
        shinyjs::removeClass(id = "zValueAnimate", class = "loading dots")

        ### Exporting result---------------------------------------------------------------#
        #* jpeg
        output$exportJPEG <-  downloadHandler(
          filename = function() {
            paste("Graph-Résultats-AnalysedeTendance-CarteValeursZ", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
          },
          content = function(file) {
            ggplot2::ggsave(plot = TrendTestGraph(), filename = file,width = 13.3, height = 7.05)
          }
        )
        # svg
        output$exportSVG <-  downloadHandler(
          filename = function() {
            paste("Graph-Résultats-AnalysedeTendance-CarteValeursZ", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
          },
          content = function(file) {
            ggplot2::ggsave(plot = TrendTestGraph(), filename = file, width = 13.3, height = 7.05)
          }
        )

      }) # SPATIALISATION DES VALEURS Z

    }) # observeEvent(ignoreInit = T, ignoreNULL = T, input$mkTest ...)

  })

}

## To be copied in the UI
# mod_performing_trend_analysis_ui("performing_trend_analysis_1")

## To be copied in the server
# mod_performing_trend_analysis_server("performing_trend_analysis_1")
