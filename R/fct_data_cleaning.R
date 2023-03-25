#' data_cleaning
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# formattage, nettoyage et géoréférencement des données à interpoler
data_cleaning<- function(data_to_interpolate, grid.cells.shp.utm){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # data_to_interpolate : data.frame contenant les données d'interpolation
  # grid.cells.shp.utm : gridded cell points in  UTM 28N from prec.grid.georef()
  #----------------------------------------------------------------------------#

  # transposition
  cleaned_data<- transpose_df(data_to_interpolate)
  # mutate(
  #   across(-Date, ~as.numeric(str_trim(.)), .names = "{col}")
  # )

  # changing columns names
  names(cleaned_data)<-
    c("Station", paste0("db_", stringr::str_replace_all(data_to_interpolate[[1]], stringr::fixed("/"), "_")))

  # Merge interpolation data with grid cells shapefile (UTM)
  merged.data.shp.utm <- sp::merge(
    grid.cells.shp.utm, cleaned_data, by = "Station"
  )

  #* Cette fonction retourne [merged.annual.pmm.shp.utm]
  #* ---------------------------------------------------
  return(merged.data.shp.utm)
}
