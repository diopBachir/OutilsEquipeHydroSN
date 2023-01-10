#' thiessen_static
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
## Thiessen Plot
thiessen.static<- function(gridded.points.utm, bassin){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # gridded.points.utm : données à interpoler georeferencées in UTM {sf}
  # bassin : Bassin versant
  #----------------------------------------------------------------------------#

  # transformation to {terra} {Vect} object
  dataV <- terra::vect(gridded.points.utm)

  # proximity (Voronoi/Thiessen) polygons
  st_varonoi <- terra::voronoi(dataV)

  # croping by mask
  result.crop <- terra::crop(st_varonoi, terra::vect(st_union(bassin)))

  # return
  vec2sf(result.crop)
}

# thiessen interpolation function
thiessen<- function(data, stations_db, bassin, epsg, target_col){

  data_to_interpole<- data %>%
    tidyr::pivot_longer(-Date, names_to = "Station", values_to = "valeur") %>%
    dplyr::inner_join(stations_db, by="Station")%>%
    dplyr::filter(Date == target_col) %>%
    dplyr::select(-1)

  # bassin reprojection
  bassin_cont_utm <- sf::st_transform(bassin, epsg)
  # generalization:
  bassin_cont_utm_smpl <- sf::st_simplify(
    bassin_cont_utm, preserveTopology = F, dTolerance = 200
  )

  #### Interpolation du voisin le plus proche (diagramme de Voronoi)

  # create simple feature:
  data_to_interpole_sf <- sf::st_as_sf(
    data_to_interpole, coords= c("Longitude", "Latitude"), crs = 4326
  )

  # Convert from lat-lon (WGS84) to utm
  data_to_interpole_sf_utm <- sf::st_transform(data_to_interpole_sf, epsg)

  # Combines geometries into one:
  data_to_interpole_sf_utm_gm <- sf::st_union(data_to_interpole_sf_utm)

  # Create the Voronoi diagram:
  data_to_interpole_sf_voronoi <- sf::st_voronoi(data_to_interpole_sf_utm_gm)
  data_to_interpole_sf_voronoi <- sf::st_sf(data_to_interpole_sf_voronoi)


  # Spatial join:
  data_to_interpole_sf_voronoi2 <- sf::st_join(
    data_to_interpole_sf_voronoi, data_to_interpole_sf_utm
  ) %>%
    tibble::tibble() %>%
    dplyr::select(c(1, 2))

  final.thiessen.output<- tibble::tibble(
    Date = as.character(target_col),
    THIESSEN=round(mean(data_to_interpole_sf_voronoi2[[2]]), 2)
  )
}
