#' transpose_df
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# transposition de data.frame
transpose_df <- function(df){
  df %>%
    t() %>%   #Tranpose, but function is for matrices. Return Matrix
    as.data.frame() %>%   #Force to be dataframe
    tibble::rownames_to_column(var = "rowname") %>%  #Resave first column from rownames
    janitor::row_to_names(row_number = 1) %>%  #Resave column headers from first row.
    tibble::tibble() # recast to tibble class
}
