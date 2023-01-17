#' northArrow
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# La flèche Nord
northArrow<-function(
    location = "tl", width=1.5, height = 1.5, padx=0.25, pady=0.25
){
  ggspatial::annotation_north_arrow(
    height =  grid::unit(height, "cm"), width=grid::unit(width, "cm"), location = location,
    pad_y = grid::unit(pady, "cm"), pad_x = grid::unit(padx, "cm"),
    style = ggspatial::north_arrow_minimal(text_face="bold",  text_size = 12)
  )
}
