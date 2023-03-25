#' krige_loop
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# krigeage : looping through every temporal step
krige_loop<- function(gridded.points.utm, grid.mod, bassin, epsg){

  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # gridded.points.utm : données à interpoler georeferencées in UTM
  # grid.mod : grille d'interpolation
  # bassin : Bassin versant
  #----------------------------------------------------------------------------#

  # df data
  prec.df<-gridded.points.utm@data

  # changement de scr
  bassin_utm<- sf::st_transform(bassin, epsg)

  # interpolation sur toutes les pas de temps
  list.krige <- colnames(prec.df)[-1] %>%
    purrr ::set_names() %>%
    map_progress(
      ., ~ ordinary_krige(
        gridded.points.utm, target_col = .x, grid_mod = grid.mod
      )
    )
  # transformation du résultat de la boucle en tibble
  krige.output.df<-
    as.data.frame(list.krige) %>%
    tibble::as_tibble() %>%
    dplyr::select(1, 2, tidyselect::ends_with("var1.pred"))
  # renommage des colonnes
  names(krige.output.df)<- c(
    "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
  )

  ### Extraction de la pluie moyenne du bassin
  krige.output.df <- krige.output.df %>%
    #* transformation en objet sf
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = epsg) %>%
    #* selection des pixels à l'intérieur du contour du bassin
    sf::st_filter(bassin_utm) %>%
    #* transformation en tibble
    tibble::tibble() %>%
    #* suppression de la colonne {geometry}
    dplyr::select(-geometry) %>%
    #* pivot en tableau long
    tidyr::pivot_longer(
      tidyselect::where(is.numeric), names_to = "Date", values_to = "KRIGEAGE_Valeurs"
    ) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      KRIGEAGE = round(mean(KRIGEAGE_Valeurs, na.rm=TRUE), 4)
    )

  return(
    krige.output.df %>%
      dplyr::mutate(Date = stringr::str_replace_all(Date, stringr::fixed("_"), "/"))
  )
}
