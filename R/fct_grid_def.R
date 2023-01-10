#' grid_def
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# Définition du modèle de la grille d'interpolation spatiale
grid_def<- function(gridded.cells.shp.utm, res.grid, extend.enlarge, utmProj){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # gridded.cells.shp.utm : gridded cell points in  UTM 28N from prec.grid.georef()
  # res.grid : Résolution de la grille d'interpolation en mètres
  # extend.enlarge : Elargir l'étendu de la couche du bassin [en mètres]
  # utmProj : Projection UTM à utiliser
  #----------------------------------------------------------------------------#

  # sample extent
  region_extent <- structure(
    c(raster::extent(gridded.cells.shp.utm)[1],  raster::extent(gridded.cells.shp.utm)[3],
      raster::extent(gridded.cells.shp.utm)[2], raster::extent(gridded.cells.shp.utm)[4]),
    .Dim = c(2L, 2L), .Dimnames = list(c("x", "y"), c("min", "max"))
  )

  # Make extent 4km larger on each direction
  x.range <- c(region_extent[1] - extend.enlarge, region_extent[3] + extend.enlarge)
  y.range <- c(region_extent[2] - extend.enlarge, region_extent[4] + extend.enlarge)

  # Create desired grid at 1km (1000) resolution
  grd <- expand.grid(
    x = seq(from = x.range[1], to = x.range[2], by = res.grid),
    y = seq(from = y.range[1], to = y.range[2], by = res.grid)
  )

  # Convert grid to spatial object
  sp::coordinates(grd) <- ~x + y

  # Use the same projection as boundary_UTM
  sp::proj4string(grd) <- utmProj
  sp::gridded(grd) <- TRUE

  #* Cette fonction retourne le grille d'interpolation [grd] :
  #  --------------------------------------------------------#
  return(grd)
}
