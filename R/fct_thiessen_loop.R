#' thiessen_loop
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# IDW : looping through every temporal step
thiessen.loop<- function(data, stations_db, bassin, epsg){

  # initialize table
  ini.thiessen<- tibble::tibble(Date = character(), THIESSEN=numeric())
  # loop over step
  for(step in data[[1]]){
    # binding
    cat(step, ":::THIESSEN:::------------------------------------------- !!\n")
    ini.thiessen<- dplyr::bind_rows(
      ini.thiessen, thiessen(data, stations_db, bassin, epsg, step)
    )
  }

  # return
  return(
    ini.thiessen %>%
      dplyr::mutate(
        Date = stringr::str_replace_all(Date, stringr::fixed("_"), "/")
      )
  )
}
