#' view_in_excel
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

ViewExcel <- function(df = .Last.value, file = base::tempfile(fileext = ".xlsx")) {
  df <- try(as.data.frame(df))
  stopifnot(is.data.frame(df))
  writexl::write_xlsx(df, file)
  base::shell.exec(file)
}
