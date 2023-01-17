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

               ## INTERPOLATION SPATIALE
               navbarMenu(div("Interpolation Spatiale", style = "color:white;font-size:115%;"),

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
                                       ),#tabPanel
                                       # tabPanel(
                                       #   div("Visualisation", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                       #   # Module
                                       #   mod_ShowTemporalInterpolationResults_ui("ShowTemporalInterpolationResults_1")
                                       # ),#tabPanel
                                       # tabPanel(
                                       #   div("Exportation", style = "color:#3474A7;family:Georgia;font-size:130%"),
                                       #   # Module
                                       #   mod_time_serie_interpolation_result_exportation_ui("time_serie_interpolation_result_exportation_1")
                                       # )#tabPanel
                                     ) # tabsetPanel
                                   ) # mainPannel
                          ) #tabPanel
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
