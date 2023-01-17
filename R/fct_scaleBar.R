#' scaleBar
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# La barre d'échelle
scaleBar<-function(
    location="bl", height=.15, width.hint=.3, tick.height=1,
    text.cex = 1.1, line.width = .8, scale.style="ticks"
){
  ggspatial::annotation_scale(
    location=location, height = grid::unit(height, "cm"),
    # text_face = "bold",
    line_width = line.width, style=scale.style, width_hint=width.hint, text_cex = text.cex
  )
}
