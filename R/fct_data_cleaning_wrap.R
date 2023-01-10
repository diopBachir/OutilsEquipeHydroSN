#' data_cleaning_wrap
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
data_cleaning_wrap<- function(data_to_interpolate, grid.cells.shp.utm){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # data_to_interpolate : data.frame contenant les données d'interpolation
  # grid.cells.shp.utm : gridded cell points in  UTM 28N from prec.grid.georef()
  #----------------------------------------------------------------------------#

  # transposition
  cleaned.data<- transpose_df(data_to_interpolate) %>%
    dplyr::mutate(
      dplyr::across(-Date, ~as.numeric(stringr::str_trim(.)), .names = "{col}")
    )

  # changing columns names
  names(cleaned.data)<-
    c("Station", paste0("db_", stringr::str_replace_all(data_to_interpolate[[1]], stringr::fixed("/"), "_")))

  # Merge interpolation data with grid cells shapefile (UTM)
  merged.data.shp.utm <- sp::merge(
    grid.cells.shp.utm, cleaned.data, by = "Station"
  )

  #* Cette fonction retourne [merged.annual.pmm.shp.utm]
  #* ---------------------------------------------------
  return(merged.data.shp.utm)
}
