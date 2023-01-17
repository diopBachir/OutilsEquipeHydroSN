#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # |||||||||||||||||||||||||||||||||||||VALEUR MOYENNE D'UN BASSIN
  # limites bassin versant
  limites_bv_time_serie_interpolation <- mod_loading_VectorFile_server("loading_VectorFile_1")
  # données
  data4TimeSerieInterpolation <- mod_data4TimeSerieInterpolation_server("data4TimeSerieInterpolation_1")

  # events trigger
  time_serie_interpolation_events_trigger <- reactive({
    list(
      limites_bv_time_serie_interpolation$limite_bassin_versant(),
      data4TimeSerieInterpolation$data_for_time_serie_interpolation()
    )
  })

  observeEvent(time_serie_interpolation_events_trigger(), {
    # showing and getting data
    readyData4TimeSerieInterpolation <- mod_showData4TimeSerieInterpolation_server(
      "showData4TimeSerieInterpolation_1",
      data4TimeSerieInterpolation$data_for_time_serie_interpolation(),
      limites_bv_time_serie_interpolation$limite_bassin_versant()
    )
    # calcul de la valeur moyenne du bassin
    time_serie_interpolation_result <- mod_mean_watershed_time_serie_value_server(
      "mean_watershed_time_serie_value_1",
      limites_bv_time_serie_interpolation$limite_bassin_versant(),
      readyData4TimeSerieInterpolation$data_for_time_serie_interpolation(),
      readyData4TimeSerieInterpolation$stations_for_time_serie_interpolation()
    )
    # Visualisation des résultats
    mod_ShowTemporalInterpolationResults_server(
      "ShowTemporalInterpolationResults_1",
      time_serie_interpolation_result$kriging_output_df(),
      time_serie_interpolation_result$idw_output_df(),
      time_serie_interpolation_result$spline_output_df(),
      time_serie_interpolation_result$thiessen_output_df()
    )
    # exportation des résultats
    mod_time_serie_interpolation_result_exportation_server(
      "time_serie_interpolation_result_exportation_1",
      time_serie_interpolation_result$kriging_output_df(),
      time_serie_interpolation_result$idw_output_df(),
      time_serie_interpolation_result$spline_output_df(),
      time_serie_interpolation_result$thiessen_output_df()
    )
  })

  # ||||||||||||||||||||||||||||||||||||| INERPOLATION VALEUR MOYENNE
  # limites bassin versant
  limites_unique_bv_time_serie_map_interpolation <- mod_loading_VectorFile_server("loading_VectorFile_2")
  # données
  data4UniquePeriodeTimeSerieInterpolationMap <- mod_data4TimeSerieInterpolation_server("data4TimeSerieInterpolation_2")

  # events trigger
  time_serie_interpolation_map__events_trigger <- reactive({
    list(
      limites_unique_bv_time_serie_map_interpolation$limite_bassin_versant(),
      data4UniquePeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation()
    )
  })


  observeEvent(time_serie_interpolation_map__events_trigger(), {
    # showing and getting data
    readyData4UniquePeriodeTimeSerieInterpolationMap <- mod_showData4TimeSerieInterpolation_server(
      "showData4TimeSerieInterpolation_2",
      data4UniquePeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation(),
      limites_unique_bv_time_serie_map_interpolation$limite_bassin_versant()
    )
    # options du graphique
    mod_unique_periode_time_serie_interpolation_map  <-
      mod_unique_periode_time_serie_interpolation_map_server(
        "unique_periode_time_serie_interpolation_map_1",
        limites_unique_bv_time_serie_map_interpolation$limite_bassin_versant(),
        readyData4UniquePeriodeTimeSerieInterpolationMap$stations_for_time_serie_interpolation(),
        readyData4UniquePeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation()
      )
  })
}
