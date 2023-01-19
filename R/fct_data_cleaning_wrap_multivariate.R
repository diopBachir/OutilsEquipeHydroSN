#' data_cleaning_wrap_multivariate
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

data_cleaning_wrap_multivariate<- function(data_to_interpolate, grid.cells.shp){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # data_to_interpolate : data.frame contenant les données d'interpolation
  # grid.cells.shp.utm : gridded cell points in  UTM 28N from prec.grid.georef()
  #----------------------------------------------------------------------------#

  # transposition
  # cleaned.data<- transpose_df(data_to_interpolate) %>%
  #   mutate(
  #     across(-Station, ~as.numeric(str_trim(.)), .names = "{col}")
  #   )

  # changing columns names
  # names(cleaned.data)<-
  #   c("Station", paste0("db_", str_replace_all(data_to_interpolate[[1]], fixed("/"), "_")))

  # Merge interpolation data with grid cells shapefile (UTM)
  merged.data.shp <- sp::merge(
    grid.cells.shp, data_to_interpolate, by = "Station"
  )

  return(merged.data.shp)
}
