#' ggmissfct
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# fonction d'inventaire des données à l'échelle journalière
ggmissfct <- function(inventory.data, order_missing = FALSE, custom_order = NULL) {
  if (order_missing == TRUE) {
    asc_missing <- inventory.data %>%
      dplyr::select(-Date) %>%
      naniar::miss_var_summary() %>%
      dplyr::pull(variable)
  } else {
    asc_missing <- NULL
  }

  inventory.data %>%
    dplyr::group_by(Date) %>%
    naniar::miss_var_summary() %>%
    dplyr::mutate(variable = factor(variable,
                                    levels = (sort(unique(variable)))) %>%
                    forcats::fct_relevel(asc_missing) %>%
                    forcats::fct_relevel(custom_order))
}
