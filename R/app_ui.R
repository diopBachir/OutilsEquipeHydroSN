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
                        )),
                 )
               ),

               # header = tags$style(
               #   HTML(
               #     ".navbar-header { width:21% }
               #     "
               #   )
               # ),

               br(), br(), br(), br(),

               # BOXPLOTS
               navbarMenu(div("BoxPlots", style = "color:white;font-size:100%;font-family:georgia"),

                          tabPanel(h4("Boxplots Mensuels Avec Les Mois Comme Facets"),
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
                          tabPanel(h4("Boxplots Univariés"),
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
                          tabPanel(h4("Boxplots Univariés Avec Facets"),
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

                          tabPanel(h4("Boxplots Multivariés"),
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

                          tabPanel(h4("Boxplots Multivariés Avec Facets"),
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

                          tabPanel(h4("Boxplots Multivariés Avec Facets [2D]"),
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
                          ) # tabPanel

               ), # navbarMenu

               #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
               #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

               ## INTERPOLATION SPATIALE
               navbarMenu(div("Interpolation Spatiale", style = "color:white;font-size:100%;font-family:georgia"),

                          tabPanel(h4("Valeur Moyenne d'un Bassin"),

                                   sidebarPanel(
                                     style="position:fixed;width:25%;height:90vh;overflow-y:auto;",

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
                                     h3("Données", style = "color:#3474A7"),
                                     # limtes bassin versa,nt
                                     mod_loading_VectorFile_ui("loading_VectorFile_1"),
                                     # stations
                                     mod_data4TimeSerieInterpolation_ui("data4TimeSerieInterpolation_1"),

                                     tags$hr(style="border-color:gray;"),

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
                                       )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPannel
                          ), #tabPanel

                          tabPanel(h4("Carte Interpolation Uni_Période"),

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

                          tabPanel(h4("Carte Interpolation MultiPériode"),

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

               ## INVENTAIRE DE DONNEES
               navbarMenu(div("Inventaire de Données", style = "color:white;font-size:100%;font-family:georgia"),

               )
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
