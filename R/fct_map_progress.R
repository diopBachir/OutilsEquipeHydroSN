#' map_progress
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
map_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)

  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ..., .id = .id)
}
