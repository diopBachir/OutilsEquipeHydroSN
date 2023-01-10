#' format_spline_output
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# Fonction d'extraction des coordonnées
format_spline_output<- function(spline.output.one){
  cbind(spline.output.one@coords, spline.output.one@data) %>%
    dplyr::rename(Longitude=x, Latitude=y) %>%
    tibble::tibble()
}
