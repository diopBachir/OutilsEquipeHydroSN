#' data_cleaning_categorical
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# formattage, nettoyage et géoréférencement des données à interpoler [categorical]
data_cleaning_categorical<- function(data_to_interpolate, grid.cells.shp){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # data_to_interpolate : data.frame contenant les données d'interpolation
  # grid.cells.shp : gridded cell points in from prec_grid_georef()
  #----------------------------------------------------------------------------#

  # transposition
  # cleaned.data<- transpose_df(data_to_interpolate)
  # mutate(
  #   across(-Date, ~as.numeric(str_trim(.)), .names = "{col}")
  # )
  # cleaned.data<- data_to_interpolate
  #
  # # changing columns names
  # names(cleaned.data)<-
  #   c("Station", names(cleaned.data)[-1])

  # Merge interpolation data with grid cells shapefile (UTM)
  merged.data.shp <- sp::merge(
    grid.cells.shp, data_to_interpolate, by = "Station", duplicateGeoms= TRUE
  )

  #* Cette fonction retourne [merged.annual.pmm.shp.utm]
  #* ---------------------------------------------------
  return(merged.data.shp)
}
