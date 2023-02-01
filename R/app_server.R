#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  ####* MAXIMUM UPLOAD SIZE LIMIT
  #* By default, Shiny limits file uploads to 5MB per file. You can modify this
  #* limit by using the shiny.maxRequestSize option. For example, adding
  #* options(shiny.maxRequestSize=30*1024^2) to the top of server.R would
  #*increase the limit to 30MB.
  options(shiny.maxRequestSize=100*1024^2)

  # ||||||||||||||||||||||||||||||||||||| BOXPLOTS MENSUELS
  # plot options
  monthlyBoxplotOptions<-  mod_monthlyBoxplotOptions_server("monthlyBoxplotOptions_1")
  # données
  monthlyBoxplotData<- mod_data4MonthlyBoxplot_server("data4MonthlyBoxplot_1")
  # observation du changement des donées
  observeEvent(monthlyBoxplotData$data_for_monthlyBoxplot(), {
    # données
    monthlyBoxplotData<- mod_data4MonthlyBoxplot_server("data4MonthlyBoxplot_1")
    # # nettoyage et recodage
    readyDataForMonthlyBoxPlot<- mod_boxplotMonthlyDataPreparation_server(
      "boxplotMonthlyDataPreparation_1", monthlyBoxplotData$data_for_monthlyBoxplot()
    )
    # making plot
    mod_makingMonthlyBoxplot_server(
      "makingMonthlyBoxplot_1", readyDataForMonthlyBoxPlot$dataMonthlyBoxplotCleaned(), monthlyBoxplotOptions
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

  # ||||||||||||||||||||||||||||||||||||| BOXPLOTS MULTIVARIES
  # Data
  multivariateBoxplotData<- mod_data4MultivariateBoxplotFile_server("data4MultivariateBoxplotFile_1")
  # gettingData
  data.multivariate.dataBoxplot<- multivariateBoxplotData$data_for_multivariateBoxplot
  # observation du changement des donées
  observeEvent(ignoreInit = FALSE, ignoreNULL = FALSE, data.multivariate.dataBoxplot(), {
    # données
    data.multivariate.dataBoxplot<- multivariateBoxplotData$data_for_multivariateBoxplot
    # plot options
    multivariateBoxplotOptions<- mod_multivariateBoxplotOptions_server("multivariateBoxplotOptions_1")
    # data preparation
    dataCleanedMultivariateBoxplot<- mod_MultivariateBoxplotPlotDataPreparation_server(
      "MultivariateBoxplotPlotDataPreparation_1", data.multivariate.dataBoxplot()
    )
    # making plot
    mod_makingMultivariateBoxplot_server(
      "makingMultivariateBoxplot_1", dataCleanedMultivariateBoxplot, multivariateBoxplotOptions
    )
  })

  # ||||||||||||||||||||||||||||||||||||| BOXPLOTS MULTIVARIES AVEC FACETS
  # données
  facetsMultivariateBoxplotData<- mod_data4FacetsMultivariateBoxplotFile_server("data4FacetsMultivariateBoxplotFile_1")
  # gettingData
  data.multivariate.facets.dataBoxplot<- facetsMultivariateBoxplotData$data_for_facetsMultivariateBoxplot

  ##==================================================================#
  observeEvent(ignoreInit = FALSE, data.multivariate.facets.dataBoxplot(), {
    # gettingData
    data.multivariate.facets.dataBoxplot<- facetsMultivariateBoxplotData$data_for_facetsMultivariateBoxplot
    # # OPTIONS
    facetsMultivariateBoxplotOptions<-  mod_facetsMultivariateBoxplotOptions_server("facetsMultivariateBoxplotOptions_1")
    # # data preparation
    dataCleanedMultivariateFacetsBoxplot<- mod_facetsMultivariateBoxplotPlotDataPreparation_server(
      "facetsMultivariateBoxplotPlotDataPreparation_1", data.multivariate.facets.dataBoxplot()
    )
    # making plot
    mod_makingMultivariateFacetsBoxplot_server(
      "makingMultivariateFacetsBoxplot_1",
      dataCleanedMultivariateFacetsBoxplot,
      facetsMultivariateBoxplotOptions
    )
  })

  # # ||||||||||||||||||||||||||||||||||||| BOXPLOTS MULTIVARIES AVEC FACETS [2D]
  # # données
  MatrixFacetsMultivariateBoxplotData<- mod_data4MatrixFacetsMultivariateBoxplot_server("data4MatrixFacetsMultivariateBoxplot_1")
  # gettingData
  data.multivariate.matrix.facets.dataBoxplot<- MatrixFacetsMultivariateBoxplotData$data_for_MatrixFacetsMultivariateBoxplot

  ##==================================================================#
  observeEvent(ignoreInit = FALSE, data.multivariate.matrix.facets.dataBoxplot(), {
    # gettingData
    data.multivariate.matrix.facets.dataBoxplot<- MatrixFacetsMultivariateBoxplotData$data_for_MatrixFacetsMultivariateBoxplot
    # options
    MatrixFacetsMultivariateBoxplotOptions<- mod_MatrixFacetsMultivariateBoxplotOptions_server("MatrixFacetsMultivariateBoxplotOptions_1")
    # data preparation
    dataCleanedMultivariateMatrixFacetsBoxplot<- mod_MatrixFacetsMultivariateBoxplotPlotDataPreparation_server(
      "MatrixFacetsMultivariateBoxplotPlotDataPreparation_1", data.multivariate.matrix.facets.dataBoxplot()
    )
    # boxplot
    mod_makingMatrixMultivariateFacetsBoxplot_server(
      "makingMatrixMultivariateFacetsBoxplot_1",
      dataCleanedMultivariateMatrixFacetsBoxplot,
      MatrixFacetsMultivariateBoxplotOptions
    )
  })

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

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

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

  # ||||||||||||||||||||||||||||||||||||| DAILY INVENTORY
  # data
  dailyInventoryData<-  mod_data4DailyInventory_server("data4DailyInventory_1")
  # gettingData
  daily.inventory.graph.data<- dailyInventoryData$data_for_dailyInventoryGraph
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, daily.inventory.graph.data(), {
    # gettingData
    daily.inventory.graph.data<- dailyInventoryData$data_for_dailyInventoryGraph
    # Résumé statistique
    mod_dailyInventoryNAvalueSummary_server("dailyInventoryNAvalueSummary_1", daily.inventory.graph.data())
    # options
    dailyInventoryHeatmapOptions<- mod_dailyInventoryHeatmapOptions_server("dailyInventoryHeatmapOptions_1")
    # heatmap
    mod_makingDailyInventoryHeatmap_server(
      "makingDailyInventoryHeatmap_1", daily.inventory.graph.data(), dailyInventoryHeatmapOptions
    )
  })

  # ||||||||||||||||||||||||||||||||||||| ANNUAL INVENTORY
  ##==================================================================#
  # données
  annualInventoryData<-  mod_data4AnnualInventory_server("data4AnnualInventory_1")
  # gettingData
  annual.inventory.graph.data<- annualInventoryData$data_for_annualInventoryGraph
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, annual.inventory.graph.data(), {
    # gettingData
    annual.inventory.graph.data<- annualInventoryData$data_for_annualInventoryGraph
    # Résumé statistique
    mod_annualInventoryNAvalueSummary_server("annualInventoryNAvalueSummary_1", annual.inventory.graph.data())
    # options
    annualInventoryHeatmapOptions<- mod_annualInventoryHeatmapOptions_server("annualInventoryHeatmapOptions_1")
    # heatmap
    mod_makingAnnualInventoryHeatmap_server(
      "makingAnnualInventoryHeatmap_1", annual.inventory.graph.data(), annualInventoryHeatmapOptions
    )
  })

  # ||||||||||||||||||||||||||||||||||||| DAILY INVENTORY AVEC FACETS
  # DATA
  dailyFacetsInventoryData<- mod_data4DailyFacetInventory_server("data4DailyFacetInventory_1")
  # gettingData
  daily.facets.inventory.graph.data<- dailyFacetsInventoryData$data_for_dailyFacetsInventoryGraph
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, daily.facets.inventory.graph.data(), {
    # gettingData
    daily.facets.inventory.graph.data<- dailyFacetsInventoryData$data_for_dailyFacetsInventoryGraph
    # Résumé statistique mod_dailyFacetsInventoryNAvalueSummary_server("dailyFacetsInventoryNAvalueSummary_1")
    mod_dailyFacetsInventoryNAvalueSummary_server("dailyFacetsInventoryNAvalueSummary_1", daily.facets.inventory.graph.data())
    # options
    dailyFacetsInventoryHeatmapOptions<- mod_dailyFacetsInventoryHeatmapOptions_server("dailyFacetsInventoryHeatmapOptions_1")
    # heatmap
    mod_makingDailyFacetsInventoryHeatmap_server(
      "makingDailyFacetsInventoryHeatmap_1", daily.facets.inventory.graph.data(), dailyFacetsInventoryHeatmapOptions
    )
  })

  # ||||||||||||||||||||||||||||||||||||| ANNUAL INVENTORY AVEC FACETS
  # data
  annualFacetsInventoryData<-  mod_data4AnnualFacetInventory_server("data4AnnualFacetInventory_1")
  # gettingData
  annual.facets.inventory.graph.data<- annualFacetsInventoryData$data_for_annualFacetsInventoryGraph
  ##======================================================================#
  observeEvent(ignoreInit = FALSE, annual.facets.inventory.graph.data(), {
    # gettingData
    annual.facets.inventory.graph.data<- annualFacetsInventoryData$data_for_annualFacetsInventoryGraph
    # Résumé statistique
    mod_annualFacetsInventoryNAvalueSummary_server("annualFacetsInventoryNAvalueSummary_1", annual.facets.inventory.graph.data())
    # options
    annualFacetsInventoryHeatmapOptions<- mod_annualFacetsInventoryHeatmapOptions_server("annualFacetsInventoryHeatmapOptions_1")
    # heatmap
    mod_makingAnnualFacetsInventoryHeatmap_server(
      "makingAnnualFacetsInventoryHeatmap_1", annual.facets.inventory.graph.data(), annualFacetsInventoryHeatmapOptions
    )
  })

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

  # ||||||||||||||||||||||||||||||||||||| SPI GRAPHIQUE UNI-SERIE
  # données
  spiUnivariateComputingData<- mod_data4univariateSPIcomputing_server("data4univariateSPIcomputing_1")
  # gettingData
  spi.univariate.graph.data<- spiUnivariateComputingData$data_for_SPIcomputing
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, spi.univariate.graph.data(), {
    # gettingData
    spi.univariate.graph.data<- spiUnivariateComputingData$data_for_SPIcomputing
    # computing spi
    spi.univariate.result.df<- mod_spiUnivariateComputing_server("spiUnivariateComputing_1", spi.univariate.graph.data())
    # getting spi formatted data for heatmap
    spi.result.ready.for.barplot<- spi.univariate.result.df$spi_result_cleaned
    # plot options
    spiBarPlotGraphOptions<-  mod_spiUnivariateBarPlotOptions_server("spiUnivariateBarPlotOptions_1")
    # spi heatmap graph
    mod_spiBarPlotGraph_server("spiBarPlotGraph_1", spi.result.ready.for.barplot(), spiBarPlotGraphOptions)
  })

  # ||||||||||||||||||||||||||||||||||||| SPI GRAPHIQUE HEATMAP
  # données
  spiComputingData<- mod_data4SPIcomputing_server("data4SPIcomputing_1")
  # gettingData
  spi.graph.data<- spiComputingData$data_for_SPIcomputing
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, spi.graph.data(), {
    # gettingData
    spi.graph.data<- spiComputingData$data_for_SPIcomputing
    # computing spi
    spi.result.df<- mod_spiComputing_server("spiComputing_1", spi.graph.data())
    # getting spi formatted data for heatmap
    spi.result.ready.for.heatmap<- spi.result.df$spi_result_cleaned
    # plot options
    spiHeatmapGraphOptions<- mod_spiHeatmapOptions_server("spiHeatmapOptions_1")
    # spi heatmap
    mod_spiHeatmapGraph_server("spiHeatmapGraph_1", spi.result.ready.for.heatmap(), spiHeatmapGraphOptions)
  })

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

  # ||||||||||||||||||||||||||||||||||||| ETP JOURNALIERE
  # données
  dailyETPcomputingData<-  mod_data4ETOcomputing_server("data4ETOcomputing_1")
  # gettingData
  daily.eto.data.path.files<- dailyETPcomputingData$data_files_path_for_dailyET0computing
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, daily.eto.data.path.files(), {
    # gettingData
    daily.eto.data.path.files<- dailyETPcomputingData$data_files_path_for_dailyET0computing
    # # computing
    mod_computingDailyET0_server("computingDailyET0_1", daily.eto.data.path.files())
  })

  # ||||||||||||||||||||||||||||||||||||| ANALYSE DE TENDANCE
  # limites bassin versant
  limites_bv_time_serie_trend_analysis <-mod_loading_VectorFile_server("loading_VectorFile_4")
  # données
  data4TimeSerieTrendAnalysis <- mod_data4TrendAnalysis_server("data4TrendAnalysis_1")
  # ##==================================================================#
  # events trigger
  time_serie_trend_analysis_events_trigger <- reactive({
    list(
      limites_bv_time_serie_trend_analysis$limite_bassin_versant(),
      data4TimeSerieTrendAnalysis$data_for_time_serie_trend_analysis()
    )
  })

  observeEvent(time_serie_trend_analysis_events_trigger(), {
    # options
    TrendAnalysisPlotsOptions<- mod_TrendAnalysisGraphsOptions_server("TrendAnalysisGraphsOptions_1")
    # showing and getting data mod_showData4TimeSerieTrendAnalysis_server("showData4TimeSerieTrendAnalysis_1")
    readyData4TimeSerieInterpolation <- mod_showData4TimeSerieTrendAnalysis_server(
      "showData4TimeSerieTrendAnalysis_1",
      data4TimeSerieTrendAnalysis$data_for_time_serie_trend_analysis(),
      limites_bv_time_serie_trend_analysis$limite_bassin_versant()
    )
    # test d'autocorrélation
    mod_TrendAnalysisAutocorrelation_server(
      "TrendAnalysisAutocorrelation_1",
      limites_bv_time_serie_trend_analysis$limite_bassin_versant(),
      readyData4TimeSerieInterpolation$data_for_time_serie_trend_analysis(),
      readyData4TimeSerieInterpolation$stations_for_time_serie_trend_analysis(),
      TrendAnalysisPlotsOptions
    )
    # analyse de tendance
    mod_performing_trend_analysis_server(
      "performing_trend_analysis_1",
      readyData4TimeSerieInterpolation$data_for_time_serie_trend_analysis(),
      readyData4TimeSerieInterpolation$stations_for_time_serie_trend_analysis(),
      TrendAnalysisPlotsOptions, limites_bv_time_serie_trend_analysis$limite_bassin_versant()
    )

  })

}

