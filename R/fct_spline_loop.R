#' spline_loop
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# TPS [Thin-Plate Spline] : looping through every temporal step
spline.loop<- function(dataInterp.utm, bassin, epsg, resolution){

  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # dataInterp.utm : données tabulaires d'interpolation en UTM
  # Résolution de la grille [en mètres]
  # bassin : Bassin versant
  #----------------------------------------------------------------------------#
  # conversion en objet data.frame
  prec.df<- dataInterp.utm@data

  # changement de scr
  bassin_utm<- sf::st_transform(bassin, epsg)

  # Nettoyyage des données

  # itération sur la période d'étude
  list.spline <- colnames(prec.df)[-1] %>%
    purrr::set_names() %>%
    purrr::map(
      ., ~ spline.interpole(bassin, dataInterp.utm, resolution, .x, epsg)
    )

  # nettoyage (2nd)
  list.spline_2<- lapply(list.spline, format_spline_output)

  # table initiale devant contenir le résultat final
  init_spline_tb<-  list.spline_2[[1]]

  # jointure
  for(item in list.spline_2[-1]){
    init_spline_tb<- init_spline_tb %>%
      dplyr::inner_join(item, by = c("Longitude", "Latitude"))
  }

  # renommage des colonnes
  names(init_spline_tb)<- c(
    "Longitude", "Latitude", stringr::str_replace(names(prec.df)[-1], "db_", "")
  )

  # ### Extraction de la pluie moyenne du bassin
  spline.output.df <- init_spline_tb %>%
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
      tidyselect::where(is.numeric), names_to = "Date", values_to = "TPS"
    ) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      TPS = round(mean(ifelse(TPS<0, TPS*(-1), TPS), na.rm=TRUE), 4)
    )

  return(
    spline.output.df %>%
      dplyr::mutate(Date = stringr::str_replace_all(Date, stringr::fixed("_"), "/"))
  )
}
