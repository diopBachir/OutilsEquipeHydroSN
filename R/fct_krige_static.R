#' krige_static
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Kriging static value
krige_static<- function(gridded.points.utm, grid.mod, bassin, epsg){

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
    purrr::set_names() %>%
    purrr::map(
      ., ~ ordinary_krige(
        gridded.points.utm, target_col = .x, grid_mod = grid.mod
      )
    )
  # transformation du résultat de la boucle en tibble
  krige.output<-
    as.data.frame(list.krige) %>%
    tibble::as_tibble() %>%
    dplyr::select(1, 2, tidyselect::ends_with("var1.pred"))
  # renommage des colonnes
  names(krige.output)<- c(
    "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
  )

  krige.output.sf<- krige.output %>%
    #* transformation en objet sf
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = epsg) %>%
    #* selection des pixels à l'intérieur du contour du bassin
    sf::st_filter(bassin_utm)

  krige.output.df<- krige.output.sf %>%
    #* transformation en tibble
    tibble::tibble() %>%
    #* suppression de la colonne {geometry}
    dplyr::select(-geometry) %>%
    cbind(sf::st_coordinates(krige.output.sf))

  return(krige.output.df)
}
