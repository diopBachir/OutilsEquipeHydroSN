#' prec_grid_georef
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# gridded data cells georeferencing
prec_grid_georef<-function(data, utm_zone_proj){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # data : data.frame contenant le nom et les coordonnées des points de mesures
  # utm_zone_proj : projection Mercator (UTM) en format proj.4
  #----------------------------------------------------------------------------#

  gridded.points<- data

  # conversion en objet sp
  sp::coordinates(gridded.points) <- ~Longitude + Latitude # longitude first
  # Add a spatial reference system (SRS) or coordinate reference system (CRS).
  # CRS database: http://spatialreference.org/ref/epsg/
  sp::proj4string(gridded.points) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  # Convert to UTM
  gridded.points.utm<- sp::spTransform(gridded.points, sp::CRS(utm_zone_proj))

  # Cette fonction retourne deux objets spatiaux :
  #*  [gridded.points et gridded.points.utm]
  #   --------------------------------------#
  return(list(gridded.points, gridded.points.utm))
}
