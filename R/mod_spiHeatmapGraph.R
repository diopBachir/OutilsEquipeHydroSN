#' spiHeatmapGraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spiHeatmapGraph_ui <- function(id){
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

    fluidRow(align="center",
             column(
               3, actionButton(ns("spiHeatmap"), span("SPI-HEATMAP", id=ns("spiHeatmapAnimate")), icon = icon("map"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("jpegOutput"), label="JPEG", icon = icon("download"), class = "btn btn-info")
             ),
             column(
               3, downloadButton(ns("svgOutput"), label="SVG", icon = icon("download"), class = "btn btn-info")
             )
    ),

    tags$hr(style="border-color:gray;"),

    withSpinner(plotOutput(ns("spi_heatmap")))
  )
}

#' spiHeatmapGraph Server Functions
#'
#' @noRd
mod_spiHeatmapGraph_server <- function(id, spi_result, spiHeatmapOptions){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # calcul du SPI

    spi_heatmap_graph<- eventReactive(input$spiHeatmap, {
      req(spi_result)

      # setting buttons with shinyjs
      shinyjs::addClass(id = "spiHeatmapAnimate", class = "loading dots")
      shinyjs::disable("spiHeatmap")

      #----------------------------------------------------------------------#
      # Tracé du graphique : Heatmap

      # ggplot2::theme du graph
      themeAnnualHeatMatInventory<-
        function(){
          ggplot2::theme_bw()+
            # personnalisation des éléments du graphique
            ggplot2::theme(
              # axes
              axis.text.x = ggplot2::element_text(
                family = "Times", size=as.numeric(spiHeatmapOptions$xAxisTextSize()), color = spiHeatmapOptions$axisTextColor(),
                hjust = as.numeric(spiHeatmapOptions$xAxisTextHjust()), vjust = as.numeric(spiHeatmapOptions$xAxisTextVjust()),
                angle = as.numeric(spiHeatmapOptions$xAxisTextAngle()), margin = ggplot2::margin(t = .1, unit = "cm")
              ),
              axis.ticks = ggplot2::element_line(
                size = as.numeric(spiHeatmapOptions$axisTicksSize()), color = spiHeatmapOptions$axisTicksColor()
              ),
              axis.ticks.length=grid::unit(.15, "cm"),
              axis.title.y.righ =  ggplot2::element_text(
                family = "Times", size=as.numeric(spiHeatmapOptions$yRightAxisTitleSize()),
                color = "black", angle = 90, margin = ggplot2::margin(l=.1, unit = "cm")
              ),
              # les grilles
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              # texte de l'axe des y
              axis.text.y= ggplot2::element_text(
                family = "Times", size=as.numeric(spiHeatmapOptions$yAxisTextSize()),
                color = spiHeatmapOptions$axisTextColor()
              ),
              # légende
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.margin = ggplot2::margin(
                t=as.numeric(spiHeatmapOptions$legMarginT()), b=as.numeric(spiHeatmapOptions$legMarginB()),
                l=as.numeric(spiHeatmapOptions$legMarginL()), r=as.numeric(spiHeatmapOptions$legMarginR()),
                unit = "cm"
              ),
              legend.text = ggplot2::element_text(
                family = "Times", size=as.numeric(spiHeatmapOptions$LegTextSize()), color = spiHeatmapOptions$LegTextColor(),
                margin = ggplot2::margin(
                  t=as.numeric(spiHeatmapOptions$legTextMarginT()), b=as.numeric(spiHeatmapOptions$legTextMarginB()),
                  l=as.numeric(spiHeatmapOptions$legTextMarginL()), r=as.numeric(spiHeatmapOptions$legTextMarginR()),
                  unit = "cm"
                )
              ),
              plot.background = ggplot2::element_rect(
                color="white", fill="white", size=.1
              ),
              panel.border = ggplot2::element_rect(
                color=spiHeatmapOptions$panelBackgroundLineColor(), fill=NA, size=as.numeric(spiHeatmapOptions$panelBackgroundLineSize())
              ),
              plot.margin = ggplot2::margin(c(.3, .3, .3, .3), unit = "cm"),
            )
        }

      #------------------------------------------------------------------------------#
      # 2nd axis hacking : support sec.axis pour scale_discrete {Github #3171}
      # guide_axis_label_trans <- function(label_trans = identity, ...) {
      #   axis_guide <- ggplot2::guide_axis(...)
      #   axis_guide$label_trans <- rlang::as_function(label_trans)
      #   class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
      #   axis_guide
      # }
      #
      # guide_train.guide_axis_trans <- function(x, ...) {
      #   trained <- NextMethod()
      #   trained$key$.label <- x$label_trans(trained$key$.label)
      #   trained
      # }
      #
      # #------------------------------------------------------------------------------#
      # # Add trailing Zeros
      # # add.trailing.zeros<- function(x){
      # #   ifelse(x %in% -90:90, paste0(x, ".0"), x)
      # # }

      #------------------------------------------------------------------------------#
      # Le graphique SPI
      plotSPI_heatmap<- function(){

        # palett<- palette(rich.colors(100, palette = "temperature", rgb = F))

        couleurMap<- c(
          "#dc143c", "#dc143c", "#ffdead", "#87cefa", "#6495ed",
          "#4169e1", "#0000ff", "#0000cd"
        )

        ggplot2::ggplot(spi_result) +

          ggplot2::geom_tile(ggplot2::aes(x=Date, y=reorder(Stations, Latitude), fill=SPI)) +

          ggplot2::scale_x_continuous(
            breaks = seq(
              min(spi_result$Date, na.rm=TRUE), max(spi_result$Date, na.rm=TRUE), as.numeric(spiHeatmapOptions$xAxisBreakStep())
            ), expand = c(0, 0)
          ) +

          ggplot2::scale_y_discrete(expand = c(0, 0)) +

          ggplot2::coord_cartesian()+

          ggplot2::guides(
            y.sec = ggh4x::guide_axis_manual(
              labels = ~ formatC(
                as.numeric(
                  stringr::str_replace_all(paste0(unique(sort(spi_result$Latitude)), .x), .x, "")
                ), format = 'f', flag='0', digits = 3
              ),
              title = "Latitude  (°N)"
            )
          )+

          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              barwidth=as.numeric(spiHeatmapOptions$barWidth()), barheight=as.numeric(spiHeatmapOptions$barHeight()),
              title.position = "top", ticks.colour = spiHeatmapOptions$barTicksColor(),
              ticks.linewidth = as.numeric(spiHeatmapOptions$barTicksLineWidth()),
              title.hjust = .5, frame.colour = "black", frame.linewidth = .6,
              title.theme = ggplot2::element_blank()
            )
          ) +

          ggplot2::scale_fill_gradientn(
            colours = couleurMap,
            limits = c(min(spi_result$SPI, na.rm=TRUE), max(spi_result$SPI, na.rm=TRUE)),
            breaks = round(seq(min(spi_result$SPI, na.rm=TRUE), max(spi_result$SPI, na.rm=TRUE), 1)+.5),
            labels = round(seq(min(spi_result$SPI, na.rm=TRUE), max(spi_result$SPI, na.rm=TRUE), 1)+.5)
          ) +

          ggplot2::labs(x=NULL, y=NULL, fill=NULL)+

          themeAnnualHeatMatInventory()
      }

      shinyjs::enable("spiHeatmap")
      shinyjs::removeClass(id = "spiHeatmapAnimate", class = "loading dots")

      return(plotSPI_heatmap())
    })

    # rendering
    output$spi_heatmap<- renderPlot({
      req(spi_heatmap_graph())
      spi_heatmap_graph()
    }, res = 57)

    ### Exporting result---------------------------------------------------------------#
    #* jpeg
    output$jpegOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-Heatmap", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".jpeg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = spi_heatmap_graph(), filename = file,width = 13.3, height = 7.05)
      }
    )
    # svg
    output$svgOutput <-  downloadHandler(
      filename = function() {
        paste("Résultats-SPI-Heatmap", stringr::str_replace_all(stringr::str_sub(Sys.time(), 1, 19), ":", "-"), ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(plot = spi_heatmap_graph(), filename = file, width = 13.3, height = 7.05)
      }
    )


  })
}

## To be copied in the UI
# mod_spiHeatmapGraph_ui("spiHeatmapGraph_1")

## To be copied in the server
# mod_spiHeatmapGraph_server("spiHeatmapGraph_1")
