#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFeedback
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),

    navbarPage(id="MainNavBar",

               inverse = T,

               position = "fixed-top",

               # logo UGB
               fluidRow(
                 column(6,
                        p(a(
                          tags$img(
                            src = "www/UGB.jpg", style = "width:70px;height:70px;margin-top:-14.2px;margin-left:-13.5px;"
                          ),
                          href = "https://www.ugb.sn/"
                        ))
                 ),
                 column(6,
                        # logo Leidi
                        p(a(
                          tags$img(
                            src = "www/leidi.png", style = "width:70px;height:70px;margin-top:-14.2px;margin-left:-13.5px;"
                          ),
                          href = "https://www.ugb.sn/"
                        ))
                 )
               ),

               # header = tags$style(
               #   HTML(
               #     ".navbar-header { width:21% }
               #     "
               #   )
               # ),

               br(), br(), br(), br(),

               # GRAPHIQUES
               navbarMenu(div("Graphiques", style = "color:white;font-size:90%;font-family:georgia"),

                          "BOXPLOTS ============================================================||",

                          tabPanel(div("Boxplots Mensuels Avec Les Mois Comme Facets", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplots Mensuels Avec Les Mois Comme Facets",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_monthlyBoxplotOptions_ui("monthlyBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(

                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4MonthlyBoxplot_ui("data4MonthlyBoxplot_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Préparer les Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # recodage
                                         mod_boxplotMonthlyDataPreparation_ui("boxplotMonthlyDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Graphique", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingMonthlyBoxplot_ui("makingMonthlyBoxplot_1")
                                       )#tabPanel
                                     )# tabsetPanel
                                   )# mainPanel
                          ), # tabPanel

                          # boxplots univarié
                          tabPanel(div("Boxplots Univariés", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplot Univarié",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_UnivariateBoxplotOptions_ui("UnivariateBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(

                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4UnivariateBoxplotFile_ui("data4UnivariateBoxplotFile_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Nettoyage||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_boxplotUnivariateDataPreparation_ui("boxplotUnivariateDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Graphique", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingUnivariateBoxplot_ui("makingUnivariateBoxplot_1")
                                       )#tabPanel
                                     )# tabsetPanel
                                   )# mainPanel
                          ), # tabPanel

                          # Boxplots Univariés Avec Facets
                          tabPanel(div("Boxplots Univariés Avec Facets", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplots Univariés Avec Facets",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_facetsUnivariateBoxplotOptions_ui("facetsUnivariateBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(

                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4FacetsUnivariateBoxplotFile_ui("data4FacetsUnivariateBoxplotFile_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Nettoyage||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_facetsUnivariateBoxplotPlotDataPreparation_ui("facetsUnivariateBoxplotPlotDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Graphique", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingUnivariateFacetsBoxplot_ui("makingUnivariateFacetsBoxplot_1")
                                       )#tabPanel
                                     )# tabsetPanel
                                   )# mainPanel
                          ), # tabPanel

                          tabPanel(div("Boxplots Multivariés", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplot Multivarié",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_multivariateBoxplotOptions_ui("multivariateBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(

                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4MultivariateBoxplotFile_ui("data4MultivariateBoxplotFile_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Nettoyage||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_MultivariateBoxplotPlotDataPreparation_ui("MultivariateBoxplotPlotDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Graphique", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingMultivariateBoxplot_ui("makingMultivariateBoxplot_1")
                                       )#tabPanel
                                     )# tabsetPanel
                                   )# mainPanel
                          ), # tabPanel

                          tabPanel(div("Boxplots Multivariés Avec Facets", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplot Multivarié Avec Facets",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_facetsMultivariateBoxplotOptions_ui("facetsMultivariateBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4FacetsMultivariateBoxplotFile_ui("data4FacetsMultivariateBoxplotFile_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Nettoyage||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_facetsMultivariateBoxplotPlotDataPreparation_ui("facetsMultivariateBoxplotPlotDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Graphique", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingMultivariateFacetsBoxplot_ui("makingMultivariateFacetsBoxplot_1")
                                       )#tabPanel
                                     )# tabsetPanel
                                   )# mainPanel
                          ), # tabPanel

                          tabPanel(div("Boxplots Multivariés Avec Facets [2D]", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Boxplots Multivariés Avec Facets [2D]",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_MatrixFacetsMultivariateBoxplotOptions_ui("MatrixFacetsMultivariateBoxplotOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4MatrixFacetsMultivariateBoxplot_ui("data4MatrixFacetsMultivariateBoxplot_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Nettoyage||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_ui("MatrixFacetsMultivariateBoxplotPlotDataPreparation_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("BoxPlots", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingMatrixMultivariateFacetsBoxplot_ui("makingMatrixMultivariateFacetsBoxplot_1")
                                       )#tabPanel
                                     )
                                   )
                          ),# tabPanel

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
                          "INVENTAIRE DE DONNEES ===============================================||",

                          # daily inventory
                          tabPanel(div("Echelle Journalière", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Echelle Journalière",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_dailyInventoryHeatmapOptions_ui("dailyInventoryHeatmapOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4DailyInventory_ui("data4DailyInventory_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Visualisation Rapide", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # résumé statistique
                                         mod_dailyInventoryNAvalueSummary_ui("dailyInventoryNAvalueSummary_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("HeatMap||Exportations", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingDailyInventoryHeatmap_ui("makingDailyInventoryHeatmap_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # annual inventory with
                          tabPanel(div("Echelle Annuelle", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Echelle Annuelle",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_annualInventoryHeatmapOptions_ui("annualInventoryHeatmapOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4AnnualInventory_ui("data4AnnualInventory_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Visualisation Rapide", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # résumé statistique
                                         mod_annualInventoryNAvalueSummary_ui("annualInventoryNAvalueSummary_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("HeatMap||Exportations", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingAnnualInventoryHeatmap_ui("makingAnnualInventoryHeatmap_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # daily inventory with facets
                          tabPanel(div("Echelle Journalière Avec Facets", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Echelle Journalière Avec Facets",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_dailyFacetsInventoryHeatmapOptions_ui("dailyFacetsInventoryHeatmapOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4DailyFacetInventory_ui("data4DailyFacetInventory_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Visualisation Rapide", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # résumé statistique
                                         mod_dailyFacetsInventoryNAvalueSummary_ui("dailyFacetsInventoryNAvalueSummary_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("HeatMap||Exportations", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingDailyFacetsInventoryHeatmap_ui("makingDailyFacetsInventoryHeatmap_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # annual inventory with facets
                          tabPanel(div("Echelle Annuelle Avec Facets", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Echelle Annuelle Avec Facets",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_annualFacetsInventoryHeatmapOptions_ui("annualFacetsInventoryHeatmapOptions_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4AnnualFacetInventory_ui("data4AnnualFacetInventory_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Visualisation Rapide", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # résumé statistique
                                         mod_annualFacetsInventoryNAvalueSummary_ui("annualFacetsInventoryNAvalueSummary_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("HeatMap||Exportations", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # boxplots univariés
                                         mod_makingAnnualFacetsInventoryHeatmap_ui("makingAnnualFacetsInventoryHeatmap_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          "STANDARDIZED PRECIPITATION INDEX [SPI] =================================||",

                          # univar heatmap
                          tabPanel(div("SPI UniSerie", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "SPI UniSerie",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),
                                     # options
                                     mod_spiUnivariateBarPlotOptions_ui("spiUnivariateBarPlotOptions_1")
                                   ), # sidebarPanel

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4univariateSPIcomputing_ui("data4univariateSPIcomputing_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("Calcul du SPI", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # calcul du SPI
                                         mod_spiUnivariateComputing_ui("spiUnivariateComputing_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("SPI Graph-Bar", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # spi heatmap
                                         mod_spiBarPlotGraph_ui("spiBarPlotGraph_1")
                                       )
                                     ) # tabsetPanel
                                   )
                          ), #tabPanel

                          tabPanel(div("SPI HeatMap", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:30%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "SPI-HeatMap",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),
                                     # options
                                     mod_spiHeatmapOptions_ui("spiHeatmapOptions_1")
                                   ), # sidebarPanel

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4SPIcomputing_ui("data4SPIcomputing_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("Calcul SPI", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # calcul du SPI
                                         mod_spiComputing_ui("spiComputing_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("SPI Graph-HeatMap", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # spi heatmap
                                         mod_spiHeatmapGraph_ui("spiHeatmapGraph_1")
                                       )
                                     ) # tabsetPanel
                                   )
                          ), #tabPanel

               ), # navbarMenu

               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

               ## INTERPOLATION SPATIALE
               navbarMenu(div("Interpolation Spatiale", style = "color:white;font-size:90%;font-family:georgia"),

                          tabPanel(div("Valeur Moyenne d'un Bassin", style = "color:black;font-size:110%;font-family:georgia;"),

                                   sidebarPanel(
                                     style="position:fixed;width:26%;height:90vh;overflow-y:auto;",

                                     # logos
                                     # logoUI(),

                                     h3(
                                       "Valeur Moyenne d'un Bassin",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     #==================================================================#
                                     # INPUTS
                                     h4("DONNEES", style = "color:#3474A7;background-color:lightgray;text-align:center;"),
                                     # Dossier Courant
                                     # mod_set_project_folder_ui("set_project_folder__temporal_interpolation"),
                                     # limtes bassin versant
                                     # mod_loading_bassin_for_temporal_interpolation_ui("loading_VectorFile_1"),
                                     mod_loading_VectorFile_ui("loading_VectorFile_1"),
                                     # stations
                                     # mod_data4TimeSerieInterpolation_shinyFiles_ui("data4TimeSerieInterpolation_shinyFiles_1"),
                                     mod_data4TimeSerieInterpolation_ui("data4TimeSerieInterpolation_1"),

                                     tags$hr(style="border-color:blue;"),

                                     h4("CONFIGURATION", style = "color:#3474A7;background-color:lightgray;text-align:center;"),
                                     # résolution de la grille
                                     mod_time_serie_interpolation_options_ui("time_serie_interpolation_options_1"),

                                     br(),br(), br(),br()

                                     #==================================================================#
                                   ),

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_showData4TimeSerieInterpolation_ui("showData4TimeSerieInterpolation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Valeur Moyenne Du Bassin", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_mean_watershed_time_serie_value_ui("mean_watershed_time_serie_value_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Visualisation", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_ShowTemporalInterpolationResults_ui("ShowTemporalInterpolationResults_1")
                                       ),#tabPanel
                                       tabPanel(
                                         div("Exportation", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_time_serie_interpolation_result_exportation_ui("time_serie_interpolation_result_exportation_1")
                                       ) #tabPanel
                                     ) # tabsetPanel
                                   ) # mainPannel
                          ), #tabPanel

                          tabPanel(div("Carte Interpolation Uni_Période", style = "color:black;font-size:110%;font-family:georgia;"),

                                   sidebarPanel(
                                     style="position:fixed;width:25%;height:90vh;overflow-y:auto;",

                                     # logos
                                     # logoUI(),

                                     h3(
                                       "Carte Interpolation Uni_Période",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     #==================================================================#
                                     # INPUTS
                                     h3("Données", style = "color:#3474A7"),
                                     # limtes bassin versa,nt
                                     mod_loading_VectorFile_ui("loading_VectorFile_2"),
                                     # stations
                                     mod_data4TimeSerieInterpolation_ui("data4TimeSerieInterpolation_2"),

                                     tags$hr(style="border-color:gray;"),

                                     br(),br(), br(),br()

                                     #==================================================================#
                                   ),

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_showData4TimeSerieInterpolation_ui("showData4TimeSerieInterpolation_2")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Interpolation|Cartographie", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_unique_periode_time_serie_interpolation_map_ui("unique_periode_time_serie_interpolation_map_1")
                                       )
                                     ) # tabsetPanel
                                   ) # mainPannel
                          ), #tabPanel

                          tabPanel(div("Carte Interpolation MultiPériode", style = "color:black;font-size:110%;font-family:georgia;"),

                                   sidebarPanel(
                                     style="position:fixed;width:25%;height:90vh;overflow-y:auto;",

                                     # logos
                                     # logoUI(),

                                     h3(
                                       "Carte Interpolation MultiPériode",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     #==================================================================#
                                     # INPUTS
                                     h3("Données", style = "color:#3474A7"),
                                     # limtes bassin versa,nt
                                     mod_loading_VectorFile_ui("loading_VectorFile_3"),
                                     # stations
                                     mod_data_4_multiperiode_interpolation_map_ui("data_4_multiperiode_interpolation_map_1"),

                                     tags$hr(style="border-color:gray;"),

                                     br(),br(), br(),br()

                                     #==================================================================#
                                   ),

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_showData4MultiperiodeTimeSerieInterpolation_ui("showData4MultiperiodeTimeSerieInterpolation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Interpolation||Recodage", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_multivariate_interpolation_data_preparation_ui("multivariate_interpolation_data_preparation_1")
                                       ),
                                       tabPanel(
                                         div("Cartographie", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # Module
                                         mod_multiperiode_periode_time_serie_interpolation_map_ui("multiperiode_periode_time_serie_interpolation_map_1")
                                       )
                                     ) # tabsetPanel
                                   ) # mainPannel
                          ) #tabPanel
               ), # navbarMenu

               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

               navbarMenu(div("Variables|Indices|Statistiques", style = "color:white;font-size:90%;font-family:georgia"),

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          "VARIABLES ==============================================================||",

                          tabPanel(div("Evapotranspiration [ETP] Journalière", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:30%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "ETP Journalière",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     br(),

                                     # Description des paramètres requis
                                     h4("Description des Paramètres Requis Pour le Calcul de l'ETP Journalière à l'Echelle Journalière"),

                                     HTML(paste0("<b>","longitude : ","</b>", "longitude de la Station cible", "</br>")),
                                     HTML(paste0("<b>","latitude : ","</b>", "latitude de la Station cible", "</br>")),
                                     HTML(paste0("<b>","altitude : ","</b>", "altitude de la Station cible", "</br>")),
                                     HTML(paste0("<b>","Rs :","</b>", "Radiation solaire atteignant la surface terrestre", "</br>")),
                                     HTML(paste0("<b>","Ra : ","</b>", "Radiation solaire au sommet de l'atmosphère", "</br>")),
                                     HTML(paste0("<b>","Tmin : ","</b>", "Température minimale à 2m du sol", "</br>")),
                                     HTML(paste0("<b>","Tmean : ","</b>", "Température moyenne à 2m du sol", "</br>")),
                                     HTML(paste0("<b>","Tmax : ","</b>", "Température maximale à 2m du sol", "</br>")),
                                     HTML(paste0("<b>","U2 : ","</b>", "Vitesse du vent à 2m du sol", "</br>")),
                                     HTML(paste0("<b>","HRmean : ","</b>", "Humidité relative", "</br>")),

                                     # Description des paramètres Estimés
                                     h4("Formules d'Après (Allen et al., 1998)"),

                                     tags$b("Pression atmosphère moyenne :"),br(),
                                     div(withMathJax("$$P = 101.3 \\times \\left[\\frac{293-0.0065 \\times altitude}{293}\\right]^{5.26} $$"), style="font-size:80%"),

                                     tags$b("Constante psychrométrique :"),br(),
                                     div(withMathJax("$$ \\gamma = (0.665 \\times 10^{-3}) \\times P$$"),style="font-size:80%"),

                                     tags$b("Température moyenne :"), br(),
                                     div(withMathJax("$$ T_{mean} = \\frac{Tmax+Tmin}{2} $$"), style="font-size:80%"),

                                     tags$b("Pression de vapeur saturante à Tmax :"),
                                     div(withMathJax("$$ esTmax = 0.6018 \\times \\exp\\left[\\frac{17.27*Tmax}{Tmax+237.3}\\right] $$"), style="font-size:80%"),

                                     tags$b("Pression de vapeur saturante à Tmin :"),
                                     div(withMathJax("$$ esTmin = 0.6018 \\times \\exp\\left[\\frac{17.27*Tmax}{Tmax+237.3}\\right] $$"), style="font-size:80%"),

                                     tags$b("Pression de vapeur saturante :"),  br(),
                                     div(withMathJax("$$ e_s = 0.6018 \\times \\exp\\left[\\frac{17.27*Tmean}{Tmean+237.3}\\right] $$"), style="font-size:80%"),

                                     tags$b("Pente de la fonction de pression de vapeur saturante :"),
                                     div(withMathJax("$$ \\delta =  \\frac{4098 \\times e_s}{\\left[Tmean+237.3\\right]^2} $$"), style="font-size:80%"),

                                     tags$b("Pression de vapeur partielle"),
                                     div(withMathJax("$$ e_a =  \\frac{HRmean}{100} \\left[\\frac{esTmax+esTmin}{2}\\right]$$"), style="font-size:80%"),

                                     tags$b("Deficit de pression de vapeur :"),
                                     div(withMathJax("$$ vpd = e_s - e_a $$"), style="font-size:80%"),

                                     tags$b("Rayonnement solaire net (ondes courtes) :"),
                                     div(withMathJax("$$ R_{ns} = \\left[1-\\alpha\\right]Rs $$"), style="font-size:80%"),

                                     tags$b("Rayonnement solaire potentiel (en ciel dégagé)) :"),
                                     div(withMathJax("$$ R_{so} = 0.75 + 2.10^2 \\times z $$"), style="font-size:80%"),

                                     tags$b("Rayonnement net à ondes longes :"),  br(),
                                     tags$b("_____Constante de Boltzman :"),
                                     div(withMathJax("$$ k_B = 4.903 \\times 10^{-9} $$"), style="font-size:80%"),

                                     tags$b("_____TmaxK * constante de Boltzman ^4 :"),
                                     div(withMathJax("$$ k_B\\_TmaxK = k_B \\times (Tmax+273.16)^4 $$"), style="font-size:80%"),

                                     tags$b("_____TminK * constante de Boltzman ^4 :"),
                                     div(withMathJax("$$ k_B\\_{TminK} = k_B * (Tmin+273.16)^4 $$"), style="font-size:80%"),

                                     tags$b("_____Correction de l'effet de l'humidité de l'air :"),
                                     div(withMathJax("$$ air_{H\\_C} = 0.34-0.14 \\times \\sqrt(e_a) $$"), style="font-size:80%"),

                                     tags$b("_____Correction de l'effet de la nébulosité :"),
                                     div(withMathJax("$$ cloud_C = 1.35 \\times \\frac{Rs}{R_{so}} -0.35 $$"), style="font-size:80%"),

                                     tags$b("_____Calcul final du rayonnement net à ondes longes :"),
                                     div(withMathJax("$$ R_{nl} = \\frac{k_B\\_{TmaxK} + k_B\\_{TminK}}{2} \\times air_{H\\_C} \\times cloud_C $$"), style="font-size:80%"),

                                     tags$b("Rayonnement net :"),
                                     div(withMathJax("$$ Rn = R_{ns} - R_{nl} $$"), style="font-size:80%"),

                                     br(),br(),br(),br(),br(),br()

                                   ), # sidebarPanel

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # données
                                         mod_data4ETOcomputing_ui("data4ETOcomputing_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("Calcul ETP Journalière", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                         # calcul du SPI
                                         mod_computingDailyET0_ui("computingDailyET0_1")
                                       ) # tabPanel
                                     ) # tabsetPanel
                                   )
                          ), #tabPanel

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          "STATISTIQUES ===========================================================||",

                          tabPanel(div("Analyse de Tendance", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Analyse De Tendance",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # INPUTS
                                     h3("Données", style = "color:#3474A7"),

                                     # limtes bassin versant
                                     mod_loading_VectorFile_ui("loading_VectorFile_4"),
                                     # données
                                     mod_data4TrendAnalysis_ui("data4TrendAnalysis_1"),
                                     # options
                                     mod_TrendAnalysisGraphsOptions_ui("TrendAnalysisGraphsOptions_1")
                                   ), # sidebarPanel

                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_showData4TimeSerieTrendAnalysis_ui("showData4TimeSerieTrendAnalysis_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("Test D'autocorrélation [ACF]", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # ACF function
                                         mod_TrendAnalysisAutocorrelation_ui("TrendAnalysisAutocorrelation_1")
                                       ), # tabPanel
                                       tabPanel(
                                         div("Analyse De Tendance", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # analyse de tendance
                                         mod_performing_trend_analysis_ui("performing_trend_analysis_1")
                                       )
                                     ) # tabsetPanel
                                   )
                          ), #tabPanel
               ), # Variables|Indices|Statistiques

               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

               navbarMenu(div("Modéles Hydro", style = "color:white;font-size:90%;font-family:georgia"),

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          "MODELES GR =============================================================||",

                          # GR4J
                          tabPanel(div("Modèle GR4J", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle GR4J",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_gr4j_model_options_ui("gr4g_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_mod_gr4j_model_pars_choose_ui("mod_gr4j_model_pars_choose_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_data4GR4Jmodel_ui("data4GR4Jmodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calage||Validation||Simulation", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_calage_validation_simulation_ui("calage_validation_simulation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_gr4j_results_graphs_n_exportation_ui("gr4j_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # GR5J
                          tabPanel(div("Modèle GR5J", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle GR5J",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_gr5j_model_options_ui("gr5j_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_gr5J_parameters_ui("gr5J_parameters_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_data4GR5Jmodel_ui("data4GR5Jmodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calage||Validation||Simulation", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_gr5j_calage_validation_simulation_ui("gr5j_calage_validation_simulation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_gr5j_results_graphs_n_exportation_ui("gr5j_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # GR5J
                          tabPanel(div("Modèle GR6J", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle GR6J",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_gr6j_model_options_ui("gr6j_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_gr6J_parameters_ui("gr6J_parameters_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_data4GR6Jmodel_ui("data4GR6Jmodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calage||Validation||Simulation", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_gr6j_calage_validation_simulation_ui("gr6j_calage_validation_simulation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_gr6j_results_graphs_n_exportation_ui("gr6j_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # GR2M
                          tabPanel(div("Modèle GR2M", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle GR2M",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_gr2m_model_options_ui("gr2m_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_gr2m_parameters_ui("gr2m_parameters_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_mod_data4GR2Mmodel_ui("mod_data4GR2Mmodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calage||Validation||Simulation", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_gr2m_calage_validation_simulation_ui("gr2m_calage_validation_simulation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_gr2m_results_graphs_n_exportation_ui("gr2m_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          # GR1A
                          tabPanel(div("Modèle GR1A", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle GR1A",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_gr1a_model_options_ui("gr1a_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_gr1a_parameters_ui("gr1a_parameters_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_mod_data4GR1Amodel_ui("mod_data4GR1Amodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calage||Validation||Simulation", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_gr1a_calage_validation_simulation_ui("gr1a_calage_validation_simulation_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_gr1a_results_graphs_n_exportation_ui("gr1a_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          "AUTRES MODELES =============================================================||",
                          # GR1A
                          tabPanel(div("Modèle HBV", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Modèle HBV",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     # options
                                     mod_HBV_model_options_ui("HBV_model_options_1"),

                                     # choix des paramètres du modèle
                                     mod_hbv_parameters_ui("hbv_parameters_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ),# sidebarPane
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod__data4HBVmodel_ui("data4HBVmodel_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Calibration", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # calage et simulation
                                         mod_calibration_HBV_ui("calibration_HBV_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Résulats||Graphs||Exportations", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportations des résultats
                                         mod_hbv_results_graphs_n_exportation_ui("hbv_results_graphs_n_exportation_1")
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPanel
                          ), # tabPanel
               ), # navbarMenu

               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

               navbarMenu(div("Extraction", style = "color:white;font-size:90%;font-family:georgia"),

                          # #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          # # Bassin Versant
                          # tabPanel(div("Extraction de Bassin(s) Versants(s)", style = "color:black;font-size:110%;font-family:georgia;"),
                          #          sidebarPanel(
                          #            style="position:fixed;width:32%;height:90vh;overflow-y:auto;",
                          #            h3(
                          #              "Extraction de BVs",
                          #              style=paste0(
                          #                "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                          #              )
                          #            ),
                          #
                          #            h4(
                          #              "IMPORTATION DES DONNEES",
                          #              style=paste0(
                          #                "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                          #              )
                          #            ),
                          #            # repertoire de travail
                          #            mod_set_watershed_delienation_project_folder_ui("set_watershed_delienation_project_folder_1"),
                          #            span("_____________________________________________________________", style="color:lightgray"),
                          #            # chargement du MNT
                          #            mod_loading_RasterFile_ui("loading_RasterFile_1"),
                          #            span("_____________________________________________________________", style="color:lightgray"),
                          #            # # Exutoires
                          #            mod_loading_bassin_outlet_ui("loading_bassin_outlet_1"),
                          #
                          #            tags$hr(style="border-color:gray;"),
                          #
                          #           #---------------------------------------------------------------
                          #            h4(
                          #              "OPTIONS  | CONFIGURATION",
                          #              style=paste0(
                          #                "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                          #              )
                          #            ),
                          #            # module
                          #            mod_whitebox_options_ui("whitebox_options_1"),
                          #
                          #            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                          #          ), # sidebarPanel
                          #          mainPanel(
                          #            tabsetPanel(
                          #              tabPanel(
                          #                div("Données", style = "color:#3474A7;family:Georgia;font-size:100%"),
                          #                # données
                          #                mod_MNT_VIZ_ui("MNT_VIZ_1")
                          #              ), #tabPanel
                          #              tabPanel(
                          #                div("Préparation du DEM", style = "color:#3474A7;family:Georgia;font-size:100%"),
                          #                # préparation du MNT pour les analyses hydrologiques
                          #                mod_DEM_Preparation_4_Hydrology_Analyses_ui("DEM_Preparation_4_Hydrology_Analyses_1")
                          #              ), #tabPanel
                          #              tabPanel(
                          #                div("FlowAccumulation & PourPoints", style = "color:#3474A7;family:Georgia;font-size:100%"),
                          #                # flow accumulation and pointer grids
                          #                mod_flow_accumulation_pointer_grids_ui("flow_accumulation_pointer_grids_1")
                          #              ), #tabPanel
                          #              # tabPanel(
                          #              #   div("Points D'écoulement", style = "color:#3474A7;family:Georgia;font-size:100%"),
                          #              #   # génération des points d'écoulements
                          #              #   # mod_flow_accumulation_pointer_grids_ui("flow_accumulation_pointer_grids_1")
                          #              # )#tabPanel
                          #              )# tabsetPanel
                          #          ) # mainPanel
                          # ), # tabPanel

                          #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||,||||||||||||||||||||||||||||||||||||||||||#
                          # Données CRU
                          tabPanel(div("Extraction Des Données NetCDF", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:26%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "Extraction Données NetCDF",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),

                                     h4(
                                       "IMPORTATION DES DONNEES",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),
                                     # repertoire de travail
                                     mod_set_project_folder_ui("set_project_folder_1"),
                                     span("_____________________________________________________________", style="color:lightgray"),
                                     # chargement du MNT
                                     mod_loading_RasterFile_ui("loading_RasterFile_CRU"),
                                     span("_____________________________________________________________", style="color:lightgray"),
                                     # # Stations
                                     mod_loading_scv_excel_layer_ui("loading_scv_excel_layer_1"),

                                     tags$hr(style="border-color:gray;"),

                                     #---------------------------------------------------------------
                                     # h4(
                                     #   "OPTIONS  | CONFIGURATION",
                                     #   style=paste0(
                                     #     "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                     #   )
                                     # ),
                                     # module
                                     # mod_whitebox_options_ui("whitebox_options_1"),

                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                   ), # sidebarPanel
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(
                                         div("Données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # données
                                         mod_raster_VIZ_ui("raster_VIZ_1")
                                       ), #tabPanel
                                       tabPanel(
                                         div("Extraction Des données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # extraction
                                         mod_cru_data_extract_ui("cru_data_extract_1"),
                                       ), #tabPanel
                                       tabPanel(
                                         div("Exportation Des données", style = "color:#3474A7;family:Georgia;font-size:120%"),
                                         # exportation
                                         mod_cru_data_export_extraction_ui("cru_data_export_extraction_1")
                                       ) #tabPanel
                                     )
                                   )
                          ), # tabPanel

                          tabPanel(div("Téléchargement Données NasaPower", style = "color:black;font-size:110%;font-family:georgia;"),
                                   sidebarPanel(
                                     style="position:fixed;width:26%;height:90vh;overflow-y:auto;",
                                     h3(
                                       "NasaPower Download",
                                       style=paste0(
                                         "color:#3474A7;text-align:center;font-family:Georgia;background-color:lightgray;"
                                       )
                                     ),
                                   ), # sidebarPanel
                                   mainPanel(
                                     tabsetPanel(
                                       htmlOutput("frame")
                                     )
                                   ) # mainPanel
                          )
               ) # navbarMenu
    )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "OutilsEquipeHydro"
    )
    # tags$script(src = "www/script.js"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
