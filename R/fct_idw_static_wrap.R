#' idw_static_wrap
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# Idw static value
  idw_static_wrap<- function(gridded.points, bv){
    #----------------------------------------------------------------------------#
    ## Parametres:
    #  ===========#
    # gridded.points : données à interpoler georeferencées
    # grid.mod : grille d'interpolation
    # bv : Bassin versant
    #----------------------------------------------------------------------------#

    #------------------------------------------------------------------------------#
    # GRILLE D4INTERPOLATION

    # definition of WGS84 (epsg:4326):
    wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84")

    #------------------------------------------------------------------------------#
    # interpolation mask contour from sf to spdf (SpatialPolygonsDataFrame):
    bv_sp <- sf::as_Spatial(bv)
    # convert spatial layers from sf object to the Spatial Polygon Data Frame (SPDF)
    sp::proj4string(bv_sp)<-wgs84

    #------------------------------------------------------------------------------#
    # define the grid:
    grd <- as.data.frame(sp::spsample(bv_sp, "regular", n = 20000))
    names(grd)       <- c("X", "Y")
    sp::coordinates(grd) <- c("X", "Y")
    sp::gridded(grd)     <- TRUE  # Create SpatialPixel object
    sp::fullgrid(grd)    <- TRUE  # Create SpatialGrid object
    sp::proj4string(grd) <- wgs84

    #grd_bu <- grd # backup the grid (we use it later again)
    #------------------------------------------------------------------------------#

    # conversion en objet data.frame
    prec.df<- gridded.points@data

    #---------------------------------------------------------------------------#
    gridded.points<- sf::as_Spatial(sf::st_transform(sf::st_as_sf(gridded.points), 4326))
    grd<- sf::as_Spatial(sf::st_transform(sf::st_as_sf(grd), 4326))

    # interpolation sur toutes les pas de temps
    list.idw <- colnames(prec.df)[-1] %>%
      purrr::set_names() %>%
      purrr::map(
        ., ~ gstat::idw(
          stats::as.formula(paste(.x, "~ 1")),
          locations = gridded.points, newdata = grd
        )
      )

    # isolines
    cont<- as.data.frame(list.idw)  %>%
      dplyr::rename(x= db_Moyenne.coords.x1, y= db_Moyenne.coords.x2, var1.pred=db_Moyenne.var1.pred)  %>%
      dplyr::select(-c(4, 5))
    cont.raster<- raster::rasterFromXYZ(cont)
    cont.raster.mask<-  raster::mask(cont.raster, bv)
    cont.raster.mask.fn<-  sf::st_as_sf(raster::rasterToContour(cont.raster.mask))
    st_crs(cont.raster.mask.fn)<- 4326


    # transformation du résultat en tibble
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
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      #* selection des pixels à l'intérieur du contour du bassin
      sf::st_filter(bv)

    idw.output.df<- idw.output.sf %>%
      #* transformation en tibble
      tibble::tibble() %>%
      #* suppression de la colonne {geometry}
      dplyr::select(-geometry) %>%
      cbind(sf::st_coordinates(idw.output.sf))

    return(list(idw.output.df, cont.raster.mask.fn))

  }
