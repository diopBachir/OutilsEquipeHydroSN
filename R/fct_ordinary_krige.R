#' ordinary_krige
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#------------------------------------------------------------------------------#
###### krigeage

# krigeage interpolation function
ordinary_krige<- function(gridded.points.utm, target_col, grid_mod){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  #  gridded.points.utm : input data in {sp} format
  #  target_col : la colonne d'interpolation
  #  grid_mod : locations to interpolate at (la grille)
  #----------------------------------------------------------------------------#

  # verbose
  cat(target_col, ":::=======||--------")

  # automap's autofitVariogram actually produces more info than we need.
  # I will only keep the var_model part.
  v_mod_OK <- automap::autofitVariogram(
    as.formula(paste(target_col, "~1")), gridded.points.utm
  )$var_mode

  # Ordinary Kriging
  OK <- gstat::krige(
    stats::as.formula(paste(target_col, "~1")),
    gridded.points.utm,                      # input data in {sp} format
    grid_mod,                                                                           # locations to interpolate at
    model = v_mod_OK                                                                    # the variogram model fitted above
  )
}
