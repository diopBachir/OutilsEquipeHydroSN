#' ggmissfct_annual
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# fonction d'inventaire des données à l'échelle annuelle
ggmissfct_annual <- function(data){
  test.df <- as.data.frame(ifelse(is.na(data), 0, 1))[,-1]
  test.df <- test.df[,order(colSums(test.df))]
  temp.df <- expand.grid(
    list(x = data$Date, y = colnames(test.df))
  )
  temp.df$m <- as.vector(as.matrix(test.df))
  temp.df <- data.frame(
    Date = unlist(temp.df$x), Variable = unlist(temp.df$y), NAbool = unlist(temp.df$m)
  )
}
