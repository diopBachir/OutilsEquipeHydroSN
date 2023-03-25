#' idw_loop
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#------------------------------------------------------------------------------#
# IDW : looping through every temporal step
idw_loop<- function(gridded.points.utm, grid.mod, bassin, utmProj){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # gridded.points.utm : données à interpoler georeferencées in UTM
  # grid.mod : grille d'interpolation
  # bassin : Bassin versant
  #----------------------------------------------------------------------------#
  # conversion en objet data.frame
  prec.df<- gridded.points.utm@data

  # changement de scr
  bassin_utm<- sf::st_transform(bassin, utmProj)

  # interpolation sur toutes les pas de temps
  list.idw <- colnames(prec.df)[-1] %>%
    purrr::set_names() %>%
    purrr::map(
      ., ~ gstat::idw(
        stats::as.formula(paste(.x, "~ 1")),
        locations = gridded.points.utm[!is.na(gridded.points.utm[[.x]]),], newdata = grid.mod
      )
    )

  # transformation du résultat de la boucle en tibble
  idw.output<-
    as.data.frame(list.idw) %>%
    tibble::as_tibble() %>%
    dplyr::select(1, 2, ends_with("var1.pred"))
  # renommage des colonnes
  names(idw.output)<- c(
    "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
  )

  idw.output.df<- idw.output %>%
    #* transformation en objet sf
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = utmProj) %>%
    #* selection des pixels à l'intérieur du contour du bassin
    sf::st_filter(bassin_utm) %>%
    #* transformation en tibble
    tibble::tibble() %>%
    #* suppression de la colonne {geometry}
    dplyr::select(-geometry) %>%
    #* pivot en tableau long
    tidyr::pivot_longer(
      tidyselect::where(is.numeric), names_to = "Date", values_to = "IDW_Valeurs"
    ) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      IDW = round(mean(IDW_Valeurs, na.rm=TRUE), 4)
    )

  return(
    idw.output.df %>%
      dplyr::mutate(Date = stringr::str_replace_all(Date, stringr::fixed("_"), "/"))
  )
}
