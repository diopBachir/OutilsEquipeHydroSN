#' idw_multivariate
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#| # IDW multivariate  value
idw_multivariate<- function(gridded.points.utm, grid.mod, bassin, utmProj){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # gridded.points.utm : données à interpoler georeferencées in UTM
  # grid.mod : grille d'interpolation
  # bassin : Bassin versant
  #----------------------------------------------------------------------------#
  # conversion en objet data.frame
  prec.df<- gridded.points.utm@data
  names(prec.df)<- stringr::str_replace_all(stringr::str_trim(names(prec.df)), " ", "")
  names(prec.df)<- stringr::str_replace_all(stringr::str_trim(names(prec.df)), stringr::fixed("-"), "_")

  names(gridded.points.utm@data)<- stringr::str_replace_all(stringr::str_trim(names(gridded.points.utm@data)), " ", "")
  names(gridded.points.utm@data)<- stringr::str_replace_all(stringr::str_trim(names(gridded.points.utm@data)), stringr::fixed("-"), "_")
  # changement de scr
  bassin_utm<- sf::st_transform(bassin, utmProj)

  # interpolation sur toutes les pas de temps
  list.idw <- colnames(prec.df)[-1] %>%
    purrr::set_names() %>%
    purrr::map(
      ., ~ gstat::idw(
        stats::as.formula(paste(.x, "~ 1")),
        locations = gridded.points.utm, newdata = grid.mod
      )
    )

  # isolines
  cont<- as.data.frame(list.idw) %>%
    dplyr::select(1, 2, tidyselect::ends_with(stringr::fixed(".var1.pred")))
  names(cont)<- stringr::str_replace_all(names(cont), stringr::fixed(".var1.pred"), "")
  cont.raster<- raster::rasterFromXYZ(cont)
  cont.raster.mask<-  raster::mask(cont.raster, bassin_utm)

  cont.fn.init<-  sf::st_as_sf(raster::rasterToContour(cont.raster.mask[[1]]))  %>%
    dplyr::mutate(variable = names(cont.raster.mask[[1]]))
  sf::st_crs(cont.fn.init)<- 4326

  for(layer in 2:raster::nlayers(cont.raster.mask)){
    temp<- sf::st_as_sf(raster::rasterToContour(cont.raster.mask[[layer]]))  %>%
      dplyr::mutate(variable = names(cont.raster.mask[[layer]]))
    sf::st_crs(temp)<- 4326

    cont.fn.init<- dplyr::bind_rows(cont.fn.init, temp)
  }
  cont.fn.init$level = as.numeric( cont.fn.init$level)

  # transformation du résultat de la boucle en tibble
  idw.output<-
    as.data.frame(list.idw) %>%
    tibble::as_tibble() %>%
    dplyr::select(1, 2, tidyselect::ends_with("var1.pred"))
  # renommage des colonnes
  names(idw.output)<- c(
    "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
  )

  idw.output.sf<- idw.output %>%
    #* transformation en objet sf
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = utmProj) %>%
    #* selection des pixels à l'intérieur du contour du bassin
    sf::st_filter(bassin_utm)
  idw.output.sf<- sf::st_transform(idw.output.sf, 4326)

  idw.output.df<- idw.output.sf %>%
    #* transformation en tibble
    tibble::tibble() %>%
    #* suppression de la colonne {geometry}
    dplyr::select(-geometry) %>%
    cbind(sf::st_coordinates(idw.output.sf))

  idw.output.df.fn<- idw.output.df %>%
    dplyr::rename(
      Longitude = X, Latitude = Y
    )  %>%
    dplyr::select(Longitude, Latitude, tidyr::everything()) %>%
    tidyr::pivot_longer(
      -c(Longitude, Latitude), names_to = "variable", values_to = "valeur"
    )

  # return
  return(
    list(
      "Résultats de l'interpolation || Valeurs des pixels" = idw.output.df.fn,
      "Résultats de l'interpolation || Isolignes" = cont.fn.init
    )
  )

}
