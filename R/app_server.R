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

  # ||||||||||||||||||||||||||||||||||||| INERPOLATION MULTIPERIODE
  # limites bassin versant
  limites_multiperiode_bv_time_serie_map_interpolation <- mod_loading_VectorFile_server("loading_VectorFile_3")
  # données
  data4MultiPeriodeTimeSerieInterpolationMap <- mod_data_4_multiperiode_interpolation_map_server("data_4_multiperiode_interpolation_map_1")

  # events trigger
  multiperiode_time_serie_interpolation_map_events_trigger <- reactive({
    list(
      limites_multiperiode_bv_time_serie_map_interpolation$limite_bassin_versant(),
      data4MultiPeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation()
    )
  })

  observeEvent(multiperiode_time_serie_interpolation_map_events_trigger(), {
    # showing and getting data
    readyData4MultiPeriodeTimeSerieInterpolationMap <- mod_showData4MultiperiodeTimeSerieInterpolation_server(
      "showData4MultiperiodeTimeSerieInterpolation_1",
      data4MultiPeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation(),
      limites_multiperiode_bv_time_serie_map_interpolation$limite_bassin_versant()
    )
    # data preparation
    cleaned_and_ready_data_4_multivariate_interpolation_map <- mod_multivariate_interpolation_data_preparation_server(
      "multivariate_interpolation_data_preparation_1",
      limites_multiperiode_bv_time_serie_map_interpolation$limite_bassin_versant(),
      readyData4MultiPeriodeTimeSerieInterpolationMap$data_for_time_serie_interpolation(),
      readyData4MultiPeriodeTimeSerieInterpolationMap$stations_for_time_serie_interpolation()
    )
    # cartographie
    mod_multiperiode_periode_time_serie_interpolation_map_server(
      "multiperiode_periode_time_serie_interpolation_map_1",
      limites_multiperiode_bv_time_serie_map_interpolation$limite_bassin_versant(),
      cleaned_and_ready_data_4_multivariate_interpolation_map$cleaned_data_for_map()
    )
  })

  # ||||||||||||||||||||||||||||||||||||| BOXPLOT UNIVARIE
  # données
  univariateBoxplotData<- mod_data4UnivariateBoxplotFile_server("data4UnivariateBoxplotFile_1")
  # observation du changement des donées
  observeEvent(univariateBoxplotData$data_for_univariateBoxplot(), {
    # plot options
    univariateBoxplotOptions<- mod_UnivariateBoxplotOptions_server("UnivariateBoxplotOptions_1")
    # données
    univariateBoxplotData<- mod_data4UnivariateBoxplotFile_server("data4UnivariateBoxplotFile_1")
    # nettoyage et recodage
    readyDataForUnivariateBoxplotPlot<- mod_boxplotUnivariateDataPreparation_server(
      "boxplotUnivariateDataPreparation_1", univariateBoxplotData$data_for_univariateBoxplot()
    )
    # making plot
    mod_makingUnivariateBoxplot_server(
      "makingUnivariateBoxplot_1",
      readyDataForUnivariateBoxplotPlot$dataUnivariateBoxplotCleaned(),
      univariateBoxplotOptions
    )
  })

  # ||||||||||||||||||||||||||||||||||||| BOXPLOTS UNIVARIATE-FACETS
  # données
  facetsUnivariateBoxplotData<- mod_data4FacetsUnivariateBoxplotFile_server("data4FacetsUnivariateBoxplotFile_1")
  # gettingData
  data.univariate.facets.dataBoxplot<- facetsUnivariateBoxplotData$data_for_facetsUnivariateBoxplot
  # observation du changement des données
  observeEvent(ignoreInit = FALSE, data.univariate.facets.dataBoxplot(), {
    # données
    facetsUnivariateBoxplotData<- mod_data4FacetsUnivariateBoxplotFile_server("data4FacetsUnivariateBoxplotFile_1")
    # gettingData
    data.univariate.facets.dataBoxplot<- facetsUnivariateBoxplotData$data_for_facetsUnivariateBoxplot
    # plot options
    facetsUnivariateBoxplotOptions<- mod_facetsUnivariateBoxplotOptions_server("facetsUnivariateBoxplotOptions_1")
    # cleaning and recoding data
    dataCleanedUnivariateFacetsBoxplot<- mod_facetsUnivariateBoxplotPlotDataPreparation_server(
      "facetsUnivariateBoxplotPlotDataPreparation_1", data.univariate.facets.dataBoxplot()
    )
    # making plot
    mod_makingUnivariateFacetsBoxplot_server(
      "makingUnivariateFacetsBoxplot_1",
      dataCleanedUnivariateFacetsBoxplot,
      facetsUnivariateBoxplotOptions
    )
  })


}

