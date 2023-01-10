#' spline_interpole
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
# Individual TPS interpolation
spline.interpole<- function(bassin.utm, dataInterp.utm, resolution, colTarget, utm.proj){
  # éfinition de la résolution
  pixelsize<- resolution
  # définition de l'étendue'
  box<- raster::extent(bassin.utm) / pixelsize * pixelsize
  template<- raster::raster(box, crs =  paste0("+init=epsg:", utm.proj),
                            nrows = (box@ymax - box@ymin) / pixelsize,
                            ncols = (box@xmax - box@xmin) / pixelsize)
  # ajustement ou modèle d'interpolation
  spline<- fields::Tps(dataInterp.utm@coords, dataInterp.utm@data[[colTarget]])
  # interpolation
  splined<- raster::interpolate(template, spline)
  # transformation en objet {SpatialPointsDataFrame}
  splined_sp<- as(splined, "SpatialPointsDataFrame")
  names(splined_sp@data)[1]<- colTarget

  cat(
    # str_replace(colTarget, fixed("db_"), ""),
    " :::==================================== !!!\n"
  )
  return(splined_sp)
}
