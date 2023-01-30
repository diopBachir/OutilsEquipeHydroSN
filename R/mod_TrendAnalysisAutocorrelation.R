#' TrendAnalysisAutocorrelation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TrendAnalysisAutocorrelation_ui <- function(id){
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

    fluidRow(align = "center",
             column(4, actionButtonStyled(ns("ACF"), span("ACF", id=ns("acfAnimate")), class= "", type="primary")),
             column(4, downloadButton(ns("exportPlotJPEG"), label="JPEG", icon = icon("download"), class = "btn btn-info")),
             column(4, downloadButton(ns("exportPlotSVG"), label="SVG", icon = icon("download"), class = "btn btn-info"))
    ),

    tags$hr(style="border-color:gray;"),

    fluidRow(align = "center",
             column(12,
                    uiOutput(ns("stationFilter"))
             )
    ),

    tags$hr(style="border-color:gray;"),

    # verbatimTextOutput(ns("test")),
    # plot
    withSpinner(plotOutput(ns("acfResultPlot")))
  )
}

#' TrendAnalysisAutocorrelation Server Functions
#'
#' @noRd
mod_TrendAnalysisAutocorrelation_server <- function(id, bassin, TrendAnalysisData, stations, TrendAnalysisGraphsOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # formatting data
    dataLoaded<- reactive({
      req(TrendAnalysisData)
      TrendAnalysisData %>%
        tidyr::pivot_longer(-Date, names_to = "station", values_to = "variable")
    })

    # store data
    dataACF <- reactiveVal()
    dataCI <- reactiveVal()

    observeEvent(dataLoaded(), {
      req(dataLoaded())
      dataACF(
        dataLoaded() %>%
          dplyr::group_by(station) %>%
          # application de la fonction d'autocorrélation
          dplyr::summarise(list_acf=list(acf(variable, plot=FALSE)))%>%
          # convertion des valeurs de corrélation en objet Double
          dplyr::mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf)))%>%
          # suppression de la colonne nesté {list_acf}
          dplyr::select(-list_acf) %>%
          # désimbrication de la colonne {acf_vals}
          tidyr::unnest(cols = c(acf_vals)) %>%
          # regroupement à nouveau par station
          dplyr::group_by(station)%>%
          # colonne contenant les pas de décalage {lags}
          dplyr::mutate(lag=dplyr::row_number()-1)
      )

      dataCI(
        dataLoaded() %>%
          dplyr::group_by(station)%>%
          dplyr::mutate(ci = 1.96/sqrt(n()))
      )

    })

    observeEvent(dataACF(), {
      output$stationFilter<- renderUI({
        req(dataACF())
        fluidRow(align="center",
                 column(12,
                        checkboxGroupInput(
                          ns("station_filter"), label = "Filtrer les stations qui doivent apparaître dans le graphique !",
                          choices = unique(dataACF()$station), selected = unique(dataACF()$station)[1], width = "100%", inline = T
                        )
                 )
        )
      })
    })

    # gestion des entrées vecteurs
    extract_variables_entries<-  function(text){
      # text<-  gsub(" ", "", text)
      split<-strsplit(text, ";", fixed = FALSE)[[1]]
      split
    }


    # plot
    acfGraph <- eventReactive(ignoreInit = TRUE, ignoreNULL = TRUE, input$ACF, {
      req(dataACF(), dataCI())

      # setting buttons with shinyjs
      shinyjs::addClass(id = "acfAnimate", class = "loading dots")
      shinyjs::disable("ACF")

      if(sum(input$station_filter %in% dataACF()$station > 0)){
        readDataACF<- dplyr::filter(dataACF(), station %in% input$station_filter)
        readDataCI<- dplyr::filter(dataCI(), station %in% input$station_filter)
      }else{
        readDataACF <- dataACF()
        readDataCI <- dataCI()
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

      # corrélogrammes
      final_plt<- ggplot2::ggplot(readDataACF, ggplot2::aes(x=lag, y=acf_vals)) +
        ggplot2::geom_bar(stat="identity", width=.4) +
        ggplot2::geom_hline(yintercept = 0, size=.4) +
        ggplot2::geom_hline(
          data = readDataCI,
          ggplot2::aes(yintercept = -ci), color="red", linetype="11", size=1
        ) +
        ggplot2::geom_hline(
          data = readDataCI,
          ggplot2::aes(yintercept = ci), color="red", linetype="11", size = 1
        ) +
        ggplot2::scale_y_continuous(limits = c(-0.6, 1.2), breaks = c(-.5, 0, .5, 1),
                           labels=c("-0,5", "0", "0,5", "1")
        )+
        ggplot2::labs(x="Lag (jour)", y="ACF") +

        ggplot2::facet_wrap(~forcats::fct_shuffle(station), ncol = facetNrow(),  scales = "free_x")+

        ggplot2::theme_bw()+
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(TrendAnalysisGraphsOptions$xAxisTitleSize())
          ),
          axis.title.y = ggplot2::element_text(
            family = "Times", color = "black", size=as.numeric(TrendAnalysisGraphsOptions$yAxisTitleSize())
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
        )

      # Button settings
      shinyjs::enable("ACF")
      shinyjs::removeClass(id = "acfAnimate", class = "loading dots")

      final_plt

    })

    output$acfResultPlot <- renderPlot({
      req(acfGraph())
      acfGraph()
    })

    # output$test <- renderPrint({
    #   req(dataACF())
    #   list(dataACF(), dataCI())
    # })

    ### Exporting result---------------------------------------------------------------#
    #* jpeg
    output$exportPlotJPEG <-  downloadHandler(
      filename = function() {
        paste("ACF-BarPlot-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = acfGraph(), filename = file,width = 13.3, height = 7.05)
      }
    )
    # svg
    output$exportPlotSVG <-  downloadHandler(
      filename = function() {
        paste("ACF-BarPlot-", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = acfGraph(), filename = file, width = 13.3, height = 7.05)
      }
    )

  })
}

## To be copied in the UI
# mod_TrendAnalysisAutocorrelation_ui("TrendAnalysisAutocorrelation_1")

## To be copied in the server
# mod_TrendAnalysisAutocorrelation_server("TrendAnalysisAutocorrelation_1")
