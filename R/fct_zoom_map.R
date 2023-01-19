#' zoom_map
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Définition du niveau de zoom de la carte
zoomMap<-function(zoomTo, zoomLevel){
  #---------------------------------------------------------------------------#
  # #zoomTo : centroid de zoom. [vecteur  de forme c(long, lat)]
  # #zoomLevel : A level of zero shows the whole world. A level of 1 shows a
  #              quarter of the world map. A level of two shows 1/16th of the
  #              world map and so on. So if z is the zoom level, we see a
  #              region that covers 1/4^z of the world. This means the range
  #              on the longitude axis that is displayed is 360°/2^z (because
  #              the longitude spans the full horizontal circle around the
  #              world from -180° to +180°) and the range on the latitude axis
  #              is 180°/2^z (latitude spans the half vertical circle from
  #              -90° to +90°). [Integer]
  #---------------------------------------------------------------------------#

  zoom_to <- zoomTo                  #centroid de zoom
  zoom_level <- zoomLevel            #niveau de zoom
  lon_span <- 360 / 2^zoom_level     #plage de longitude visible
  lat_span <- 180 / 2^zoom_level     #plage de latitude visible

  # Now we can calculate the longitude and latitude ranges for the the display
  # window by subtracting/adding the half of the above ranges from/to the zoom
  # center coordinates respectively.
  lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
  lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

  list(lon_bounds, lat_bounds)

}
