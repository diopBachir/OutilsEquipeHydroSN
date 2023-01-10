#' test_match_order
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

### tester l'égalité parfaite de deux vecteurs
test_match_order <- function(x,y) {

  if (isTRUE(all.equal(x,y))) return(TRUE)

  if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y)))) return(FALSE)

  if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y)))) return(FALSE)
}
