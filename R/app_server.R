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
  options(shiny.maxRequestSize=2048*1024^2)
  options(future.globals.maxSize= 2048*1024^2 )

  # ||||||||||||||||||||||||||||||||||||| BOXPLOTS MENSUELS
  # plot options
  monthlyBoxplotOptions<- mod_monthlyBoxplotOptions_server("monthlyBoxplotOptions_1")
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
  # limites_bv_time_serie_interpolation <- mod_loading_bassin_for_temporal_interpolation_server("loading_VectorFile_1", working_directorytime_serie_interpolation())
  limites_bv_time_serie_interpolation <- mod_loading_VectorFile_server("loading_VectorFile_1")
  # données
  # data4TimeSerieInterpolation <-  mod_data4TimeSerieInterpolation_shinyFiles_server("data4TimeSerieInterpolation_shinyFiles_1", working_directorytime_serie_interpolation())
  data4TimeSerieInterpolation <-  mod_data4TimeSerieInterpolation_server("data4TimeSerieInterpolation_1")

  # events trigger
  time_serie_interpolation_events_trigger <- reactive({
    list(
      limites_bv_time_serie_interpolation$limite_bassin_versant(),
      data4TimeSerieInterpolation$data_for_time_serie_interpolation()
    )
  })

  observeEvent(time_serie_interpolation_events_trigger(), {
    # interpolation options
    # options<- mod_time_serie_interpolation_options_server("time_serie_interpolation_options_1")
    # showing and getting data
    readyData4TimeSerieInterpolation <- mod_showData4TimeSerieInterpolation_server(
      "showData4TimeSerieInterpolation_1",
      data4TimeSerieInterpolation$data_for_time_serie_interpolation(),
      limites_bv_time_serie_interpolation$limite_bassin_versant()
    )

    # grille et pédiode d'interpolation
    options_for_time_serie_interpolation<- mod_time_serie_interpolation_options_server(
      "time_serie_interpolation_options_1", readyData4TimeSerieInterpolation$data_for_time_serie_interpolation(),
      limites_bv_time_serie_interpolation$limite_bassin_versant()
    )

    # calcul de la valeur moyenne du bassin
    time_serie_interpolation_result <- mod_mean_watershed_time_serie_value_server(
      "mean_watershed_time_serie_value_1",
      limites_bv_time_serie_interpolation$limite_bassin_versant(),
      readyData4TimeSerieInterpolation$data_for_time_serie_interpolation(),
      readyData4TimeSerieInterpolation$stations_for_time_serie_interpolation(),
      options_for_time_serie_interpolation$gridRes,
      options_for_time_serie_interpolation$startDate,
      options_for_time_serie_interpolation$endDate,
      options_for_time_serie_interpolation$modele_de_grille
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
      time_serie_interpolation_result$thiessen_output_df(),
      readyData4TimeSerieInterpolation$data_for_time_serie_interpolation()
    )
  })


  # repertoire de travail
  # working_dir_time_serie_interpolation<- mod_set_project_folder_server("set_project_folder__temporal_interpolation")
  # working_directorytime_serie_interpolation<- working_dir_time_serie_interpolation$wdir
  # observeEvent(working_dir_time_serie_interpolation$wdir(), {
  #   # limites bassin versant
  #   # limites_bv_time_serie_interpolation <- mod_loading_bassin_for_temporal_interpolation_server("loading_VectorFile_1", working_directorytime_serie_interpolation())
  #   limites_bv_time_serie_interpolation <- mod_loading_VectorFile_server("loading_VectorFile_1")
  #   # données
  #   # data4TimeSerieInterpolation <-  mod_data4TimeSerieInterpolation_shinyFiles_server("data4TimeSerieInterpolation_shinyFiles_1", working_directorytime_serie_interpolation())
  #   data4TimeSerieInterpolation <-  mod_data4TimeSerieInterpolation_server("data4TimeSerieInterpolation_1")
  #
  #   # events trigger
  #   time_serie_interpolation_events_trigger <- reactive({
  #     list(
  #       limites_bv_time_serie_interpolation$limite_bassin_versant(),
  #       data4TimeSerieInterpolation$data_for_time_serie_interpolation()
  #     )
  #   })
  #
  #   observeEvent(time_serie_interpolation_events_trigger(), {
  #     # interpolation options
  #     # options<- mod_time_serie_interpolation_options_server("time_serie_interpolation_options_1")
  #     # showing and getting data
  #     readyData4TimeSerieInterpolation <- mod_showData4TimeSerieInterpolation_server(
  #       "showData4TimeSerieInterpolation_1",
  #       data4TimeSerieInterpolation$data_for_time_serie_interpolation(),
  #       limites_bv_time_serie_interpolation$limite_bassin_versant()
  #     )
  #
  #     # grille et pédiode d'interpolation
  #     options_for_time_serie_interpolation<- mod_time_serie_interpolation_options_server(
  #       "time_serie_interpolation_options_1", readyData4TimeSerieInterpolation$data_for_time_serie_interpolation(),
  #       limites_bv_time_serie_interpolation$limite_bassin_versant()
  #     )
  #
  #     # calcul de la valeur moyenne du bassin
  #     time_serie_interpolation_result <- mod_mean_watershed_time_serie_value_server(
  #       "mean_watershed_time_serie_value_1",
  #       limites_bv_time_serie_interpolation$limite_bassin_versant(),
  #       readyData4TimeSerieInterpolation$data_for_time_serie_interpolation(),
  #       readyData4TimeSerieInterpolation$stations_for_time_serie_interpolation(),
  #       options_for_time_serie_interpolation$gridRes,
  #       options_for_time_serie_interpolation$startDate,
  #       options_for_time_serie_interpolation$endDate,
  #       options_for_time_serie_interpolation$modele_de_grille
  #     )
  #     # Visualisation des résultats
  #     mod_ShowTemporalInterpolationResults_server(
  #       "ShowTemporalInterpolationResults_1",
  #       time_serie_interpolation_result$kriging_output_df(),
  #       time_serie_interpolation_result$idw_output_df(),
  #       time_serie_interpolation_result$spline_output_df(),
  #       time_serie_interpolation_result$thiessen_output_df()
  #     )
  #     # exportation des résultats
  #     mod_time_serie_interpolation_result_exportation_server(
  #       "time_serie_interpolation_result_exportation_1",
  #       time_serie_interpolation_result$kriging_output_df(),
  #       time_serie_interpolation_result$idw_output_df(),
  #       time_serie_interpolation_result$spline_output_df(),
  #       time_serie_interpolation_result$thiessen_output_df(),
  #       readyData4TimeSerieInterpolation$data_for_time_serie_interpolation()
  #     )
  #   })
  #
  # })

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

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : GR4J
  # data
  daily4GR4J<-  mod_data4GR4Jmodel_server("data4GR4Jmodel_1")
  # gettingData
  gr4j.application.data<- daily4GR4J$data_for_GR_modelisiation
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, gr4j.application.data(), {
    # gettingData
    gr4j.application.data<- daily4GR4J$data_for_GR_modelisiation
    # options
    gr4japplication.options<- mod_gr4j_model_options_server("gr4g_model_options_1", gr4j.application.data())
    # calage, validation and simulation
    calage_validation_simulation_result <-mod_calage_validation_simulation_server(
      "calage_validation_simulation_1", gr4j.application.data(), gr4japplication.options,
      gr4J_parameters$parametres_simulation
    )
    # choix des paramètres du modèles pour la simulation
    gr4J_parameters<- mod_mod_gr4j_model_pars_choose_server(
      "mod_gr4j_model_pars_choose_1", calage_validation_simulation_result$cross_valid_best_params
    )
    # visualisation et exportation des résultats
    mod_gr4j_results_graphs_n_exportation_server(
      "gr4j_results_graphs_n_exportation_1",
      # période d'échauffement
      calage_validation_simulation_result$warm_up_period,
      # donnees
      calage_validation_simulation_result$cross_validation_data_first,
      calage_validation_simulation_result$cross_validation_data_second,
      calage_validation_simulation_result$overall_serie_data,
      # Validation Croisée 1
      calage_validation_simulation_result$cross_validation_period_1_1,
      calage_validation_simulation_result$cross_validation_period_1_2,
      calage_validation_simulation_result$calibration_1,
      calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      calage_validation_simulation_result$cross_validation_period_2_1,
      calage_validation_simulation_result$cross_validation_period_2_2,
      calage_validation_simulation_result$calibration_2,
      calage_validation_simulation_result$validation_2,
      # simulation
      calage_validation_simulation_result$simulation_period,
      calage_validation_simulation_result$simulation_output_result
    )
  })

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : GR5J
  # data
  daily4GR5J<-  mod_data4GR5Jmodel_server("data4GR5Jmodel_1")
  # gettingData
  gr5j.application.data<- daily4GR5J$data_for_GR_modelisiation
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, gr5j.application.data(), {
    # gettingData
    gr5j.application.data<- daily4GR5J$data_for_GR_modelisiation
    # options
    gr5japplication.options<- mod_gr5j_model_options_server("gr5j_model_options_1", gr5j.application.data())
    # calage, validation and simulation
    gr5j_calage_validation_simulation_result <-mod_gr5j_calage_validation_simulation_server(
      "gr5j_calage_validation_simulation_1", gr5j.application.data(), gr5japplication.options,
      gr5J_parameters$parametres_simulation
    )
    # choix des paramètres du modèles pour la simulation
    gr5J_parameters<- mod_gr5J_parameters_server(
      "gr5J_parameters_1", gr5j_calage_validation_simulation_result$cross_valid_best_params
    )
    # visualisation et exportation des résultats
    mod_gr5j_results_graphs_n_exportation_server(
      "gr5j_results_graphs_n_exportation_1",
      # période d'échauffement
      gr5j_calage_validation_simulation_result$warm_up_period,
      # donnees
      gr5j_calage_validation_simulation_result$cross_validation_data_first,
      gr5j_calage_validation_simulation_result$cross_validation_data_second,
      gr5j_calage_validation_simulation_result$overall_serie_data,
      # Validation Croisée 1
      gr5j_calage_validation_simulation_result$cross_validation_period_1_1,
      gr5j_calage_validation_simulation_result$cross_validation_period_1_2,
      gr5j_calage_validation_simulation_result$calibration_1,
      gr5j_calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      gr5j_calage_validation_simulation_result$cross_validation_period_2_1,
      gr5j_calage_validation_simulation_result$cross_validation_period_2_2,
      gr5j_calage_validation_simulation_result$calibration_2,
      gr5j_calage_validation_simulation_result$validation_2,
      # simulation
      gr5j_calage_validation_simulation_result$simulation_period,
      gr5j_calage_validation_simulation_result$simulation_output_result
    )
  })

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : GR6J
  # data
  daily4GR6J<-  mod_data4GR5Jmodel_server("data4GR6Jmodel_1")
  # gettingData
  gr6j.application.data<- daily4GR6J$data_for_GR_modelisiation
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, gr6j.application.data(), {
    # gettingData
    gr6j.application.data<- daily4GR6J$data_for_GR_modelisiation
    # options
    gr6japplication.options<- mod_gr6j_model_options_server("gr6j_model_options_1", gr6j.application.data())
    # calage, validation and simulation
    gr6j_calage_validation_simulation_result <-mod_gr6j_calage_validation_simulation_server(
      "gr6j_calage_validation_simulation_1", gr6j.application.data(), gr6japplication.options,
      gr6J_parameters$parametres_simulation
    )
    # choix des paramètres du modèles pour la simulation
    gr6J_parameters<- mod_gr6J_parameters_server(
      "gr6J_parameters_1", gr6j_calage_validation_simulation_result$cross_valid_best_params
    )
    # visualisation et exportation des résultats
    mod_gr6j_results_graphs_n_exportation_server(
      "gr6j_results_graphs_n_exportation_1",
      # période d'échauffement 1
      gr6j_calage_validation_simulation_result$warm_up_period,
      # donnees
      gr6j_calage_validation_simulation_result$cross_validation_data_first,
      gr6j_calage_validation_simulation_result$cross_validation_data_second,
      gr6j_calage_validation_simulation_result$overall_serie_data,
      # Validation Croisée 1
      gr6j_calage_validation_simulation_result$cross_validation_period_1_1,
      gr6j_calage_validation_simulation_result$cross_validation_period_1_2,
      gr6j_calage_validation_simulation_result$calibration_1,
      gr6j_calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      gr6j_calage_validation_simulation_result$cross_validation_period_2_1,
      gr6j_calage_validation_simulation_result$cross_validation_period_2_2,
      gr6j_calage_validation_simulation_result$calibration_2,
      gr6j_calage_validation_simulation_result$validation_2,
      # simulation
      gr6j_calage_validation_simulation_result$simulation_period,
      gr6j_calage_validation_simulation_result$simulation_output_result
    )
  })

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : GR2M
  # data
  daily4GR2M<-  mod_mod_data4GR2Mmodel_server("mod_data4GR2Mmodel_1")
  # gettingData
  gr2m.application.data<- daily4GR2M$data_for_GR_modelisiation
  ##==================================================================#
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, gr2m.application.data(), {
    # gettingData
    gr2m.application.data<- daily4GR2M$data_for_GR_modelisiation
    # options
    gr2m_application.options<- mod_gr2m_model_options_server("gr2m_model_options_1", gr2m.application.data())
    # calage, validation and simulation
    gr2m_calage_validation_simulation_result <- mod_gr2m_calage_validation_simulation_server(
      "gr2m_calage_validation_simulation_1", gr2m.application.data(), gr2m_application.options,
      gr2m_parameters_choose$parametres_simulation
    )
    # choix des paramètres du modèles pour la simulation
    gr2m_parameters_choose<- mod_gr2m_parameters_server(
      "gr2m_parameters_1", gr2m_calage_validation_simulation_result$cross_valid_best_params
    )
    # visualisation et exportation des résultats
    mod_gr2m_results_graphs_n_exportation_server(
      "gr2m_results_graphs_n_exportation_1",
      # période d'échauffement
      gr2m_calage_validation_simulation_result$warm_up_period,
      # donnees
      gr2m_calage_validation_simulation_result$cross_validation_data_first,
      gr2m_calage_validation_simulation_result$cross_validation_data_second,
      gr2m_calage_validation_simulation_result$overall_serie_data,
      # Validation Croisée 1
      gr2m_calage_validation_simulation_result$cross_validation_period_1_1,
      gr2m_calage_validation_simulation_result$cross_validation_period_1_2,
      gr2m_calage_validation_simulation_result$calibration_1,
      gr2m_calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      gr2m_calage_validation_simulation_result$cross_validation_period_2_1,
      gr2m_calage_validation_simulation_result$cross_validation_period_2_2,
      gr2m_calage_validation_simulation_result$calibration_2,
      gr2m_calage_validation_simulation_result$validation_2,
      # simulation
      gr2m_calage_validation_simulation_result$simulation_period,
      gr2m_calage_validation_simulation_result$simulation_output_result
    )
  })

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : GR1A
  # data
  daily4GR1A<-  mod_mod_data4GR1Amodel_server("mod_data4GR1Amodel_1")
  # gettingData
  gr1a.application.data<- daily4GR1A$data_for_GR_modelisiation
  ##==================================================================#
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, gr1a.application.data(), {
    # gettingData
    gr1a.application.data<- daily4GR1A$data_for_GR_modelisiation
    # # options
    gr1a_application.options<- mod_gr1a_model_options_server("gr1a_model_options_1", gr1a.application.data())
    # calage, validation and simulation
    gr1a_calage_validation_simulation_result <- mod_gr1a_calage_validation_simulation_server(
      "gr1a_calage_validation_simulation_1", gr1a.application.data(), gr1a_application.options,
      gr1a_parameters_choose$parametres_simulation
    )
    # choix des paramètres du modèles pour la simulation
    gr1a_parameters_choose<- mod_gr1a_parameters_server(
      "gr1a_parameters_1", gr1a_calage_validation_simulation_result$cross_valid_best_params
    )
    # visualisation et exportation des résultats
    mod_gr1a_results_graphs_n_exportation_server(
      "gr1a_results_graphs_n_exportation_1",
      # période d'échauffement
      gr1a_calage_validation_simulation_result$warm_up_period,
      # donnees
      gr1a_calage_validation_simulation_result$cross_validation_data_first,
      gr1a_calage_validation_simulation_result$cross_validation_data_second,
      gr1a_calage_validation_simulation_result$overall_serie_data,
      # Validation Croisée 1
      gr1a_calage_validation_simulation_result$cross_validation_period_1_1,
      gr1a_calage_validation_simulation_result$cross_validation_period_1_2,
      gr1a_calage_validation_simulation_result$calibration_1,
      gr1a_calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      gr1a_calage_validation_simulation_result$cross_validation_period_2_1,
      gr1a_calage_validation_simulation_result$cross_validation_period_2_2,
      gr1a_calage_validation_simulation_result$calibration_2,
      gr1a_calage_validation_simulation_result$validation_2,
      # simulation
      gr1a_calage_validation_simulation_result$simulation_period,
      gr1a_calage_validation_simulation_result$simulation_output_result
    )
  })

  # ||||||||||||||||||||||||||||||||||||| HYDROLOGICAL MODELS : HBV
  # data
  dailydata4HBVmodel<-  mod__data4HBVmodel_server("data4HBVmodel_1")
  # gettingData
  hbv.application.data<- dailydata4HBVmodel$data_for_HBV_modelisiation
  ##==================================================================#
  ##==================================================================#
  observeEvent(ignoreInit = FALSE, hbv.application.data(), {
    # gettingData
    hbv.application.data<- dailydata4HBVmodel$data_for_HBV_modelisiation
    # options
    hbv_application.options<- mod_HBV_model_options_server("HBV_model_options_1", hbv.application.data())
    # calage, validation and simulation
    hbv_calage_validation_simulation_result <- mod_calibration_HBV_server(
      "calibration_HBV_1", hbv.application.data(), hbv_application.options,
      hbv_parameters_choose$parametres_simulation
    )
    # paramètres du modèles
    hbv_parameters_choose<- mod_hbv_parameters_server(
      "hbv_parameters_1", hbv_calage_validation_simulation_result$cross_valid_best_params_values
    )
    # exportation des résultats
    mod_hbv_results_graphs_n_exportation_server(
      "hbv_results_graphs_n_exportation_1",
      # Validation Croisée 1
      hbv_calage_validation_simulation_result$cross_validation_period_1_1,
      hbv_calage_validation_simulation_result$cross_validation_period_1_2,
      hbv_calage_validation_simulation_result$calibration_1,
      hbv_calage_validation_simulation_result$validation_1,
      # Validation Croisée 1
      hbv_calage_validation_simulation_result$cross_validation_period_2_1,
      hbv_calage_validation_simulation_result$cross_validation_period_2_2,
      hbv_calage_validation_simulation_result$calibration_2,
      hbv_calage_validation_simulation_result$validation_2,
      # simulation
      hbv_calage_validation_simulation_result$simulation_period,
      hbv_calage_validation_simulation_result$simulation_output_result
    )
  })

  #|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
  #|#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

  # # ||||||||||||||||||||||||||||||||||||| DELIMITATION DE BASSIN VERSANT
  # whitebox::wbt_init()
  # ## set tmap modeto interactive viewing
  # tmap::tmap_mode("plot")
  #
  # # repertoire de travail
  # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
  # working_directory<- working_dir$wdir
  # # whitebox option
  # whitebox_options<- mod_whitebox_options_server("whitebox_options_1")
  # observeEvent(working_directory(), {
  #   # repertoire de travail
  #   # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
  #   # working_directory<- working_dir$wdir
  #   # whitebox option
  #   whitebox_options<- mod_whitebox_options_server("whitebox_options_1")
  #   # repertoire de travail
  #   # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
  #   # chargement du MNT
  #   MNT<- mod_loading_RasterFile_server("loading_RasterFile_1", working_directory())
  #   # getting georeferenced MNT
  #   georeferenced_mnt<- MNT$mnt_georeferenced
  #   # Exutoire des bassins
  #   bassin_outlet<- mod_loading_bassin_outlet_server("loading_bassin_outlet_1", working_directory())
  #   # getting outlet layer
  #   exutoires<- bassin_outlet$exutoires_bassin_versants
  #
  #   ##==================================================================#
  #   ##==================================================================#
  #   events_trigger<- reactive({list(georeferenced_mnt(), exutoires()) })
  #   observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, events_trigger(), {
  #     # whitebox option
  #     # whitebox_options<- mod_whitebox_options_server("whitebox_options_1")
  #     # # repertoire de travail
  #     # # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
  #     # # working_directory<- working_dir$wdir
  #     # # chargement du MNT
  #     # MNT<- mod_loading_RasterFile_server("loading_RasterFile_1", working_directory())
  #     # # getting georeferenced MNT
  #     # georeferenced_mnt<- MNT$mnt_georeferenced
  #     # # Exutoire des bassins
  #     # bassin_outlet<- mod_loading_bassin_outlet_server("loading_bassin_outlet_1", working_directory())
  #     # # getting outlet layer
  #     # exutoires<- bassin_outlet$exutoires_bassin_versants
  #
  #     # visualisation du MNT
  #     mod_MNT_VIZ_server("MNT_VIZ_1", georeferenced_mnt()[[1]], exutoires()[[1]])
  #
  #     # prepare DEM
  #     dem_preparation<- mod_DEM_Preparation_4_Hydrology_Analyses_server(
  #       "DEM_Preparation_4_Hydrology_Analyses_1", working_directory(), georeferenced_mnt()[[2]],
  #       exutoires()[[1]], whitebox_options$breaching_max_dist
  #     )
  #
  #     # flow accumulation & pointers grid
  #     mod_flow_accumulation_pointer_grids_server(
  #       "flow_accumulation_pointer_grids_1", working_directory(),
  #       dem_preparation$path_to_filled_breached_DEM,
  #       whitebox_options$bassin_thresold, exutoires()[[1]],
  #       exutoires()[[2]], whitebox_options$pp_snap_dist
  #     )
  #   })
  # })

  ##==================================================================# NetCDF Extraction
  # repertoire de travail
  cru_extract_working_dir<- mod_set_project_folder_server("set_project_folder_1")
  cru_working_directory<- cru_extract_working_dir$wdir
  # whitebox option
  # whitebox_options<- mod_whitebox_options_server("whitebox_options_1")
  observeEvent(cru_working_directory(), {
    # repertoire de travail
    # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
    # working_directory<- working_dir$wdir
    # whitebox option
    # whitebox_options<- mod_whitebox_options_server("whitebox_options_1")
    # repertoire de travail
    # working_dir<- mod_set_watershed_delienation_project_folder_server("set_watershed_delienation_project_folder_1")
    # chargement du MNT
    NetCDF<- mod_loading_RasterFile_server("loading_RasterFile_CRU", cru_working_directory())
    # getting georeferenced MNT
    georeferenced_NetCDF<- NetCDF$raster_georeferenced
    # Exutoire des bassins
    stations<- mod_loading_scv_excel_layer_server("loading_scv_excel_layer_1",  cru_working_directory())
    # getting outlet layer
    stations_georeferenced<- stations$stations_loaded

    ##==================================================================#
    ##==================================================================#
    events_trigger<- reactive({list(georeferenced_NetCDF(), stations_georeferenced()) })
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, events_trigger(), {
      # visualisation du raster
      mod_raster_VIZ_server("raster_VIZ_1", NetCDF$raster_georeferenced, stations$stations_loaded)
      # extraction des résultats
      extracted_cru_data<- mod_cru_data_extract_server("cru_data_extract_1", NetCDF$raster_path, stations$stations_loaded)
      # exportation
      mod_cru_data_export_extraction_server("cru_data_export_extraction_1", extracted_cru_data$cru_extracted_data, extracted_cru_data$variable_extracted)
    })

    ##==================================================================# NasaPower Extraction
    mod_nasapower_api_embedding_server("nasapower_api_embedding_1")

  })

}

