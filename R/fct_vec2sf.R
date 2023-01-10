#' vec2sf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
### Convertir les objets {terra} en {sf}
vec2sf <- function(from) {
  d <- as.data.frame(from, geom="hex")
  d$geometry <- structure(as.list(d$geometry), class = "WKB")
  sf::st_as_sf(d, crs=from@ptr$get_crs("wkt"))
}
