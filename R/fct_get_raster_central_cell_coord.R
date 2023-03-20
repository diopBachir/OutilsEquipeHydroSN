#' get_raster_central_cell_coord
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_raster_central_cell_coord<- function(raster_img){
  row <- base::trunc(raster::nrow(raster_img)/2)
  col <- base::trunc(raster::ncol(raster_img)/2)
  cell <- raster::cellFromRowCol(raster_img, row, col)
  raster::xyFromCell(raster_img, cell)
}
