#' EstParamMeteoET0
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#Estimation des paramètres manquants nécessaires à l'application de la FAO56PM
#source : FAO PAPER N°56 [PARTIE A ; CHAPITRE 3]
EstParamMeteoET0<-function(meteoData){
  #---------------------------------------------------------------#
  # meteoData = tibble contenant les paramètres minimum requis pour l'estimation
  #---------------------------------------------------------------#
  meteoData%>%
    dplyr::mutate(
      #Pression atmosphère moyenne
      P = round(101.3 * ((293-0.0065*altitude)/293)^5.26, 2),
      #constante psychrométrique [eq.8]
      gamma = round((0.665*10^-3)*P, 3),
      #température moyenne [eq.9]
      Tmean = round((Tmax+Tmin)/2, 2),
      #pression de vapeur saturante à Tmax
      esTmax = round(0.6018*exp((17.27*Tmax)/(Tmax+237.3)), 3),
      #pression de vapeur saturante à Tmin
      esTmin = round(0.6018*exp((17.27*Tmin)/(Tmin+237.3)), 3),
      #pression de vapeur saturante
      es = round(0.6018*exp((17.27*Tmean)/(Tmean+237.3)), 3),
      #pente de la fonction de la pression de vapeur saturante  #[eq.13]
      delta = round((4098 * es)/(Tmean+237.3)**2, 3),
      #pression de vapeur partielle [eq. 19]
      ea = round((HRmean/100)*((esTmax+esTmin)/2), 3),
      #deficit de pression de vapeur
      es_ea = es - ea,
      #rayonnement solaire net (ondes courtes)
      Rns = round((1-0.23)*Rs, 2),
      #rayonnement solaire par ciel dégagé (rayonnement solaire potentiel)
      Rso = round((0.75 + (2*10**(-5))*altitude) * Ra, 2),
      #rayonnement net à ondes longes
      bzConst = 4.903*10**(-9),                                   # constante de Boltzman
      bz_TmaxK = bzConst * (Tmax+273.16)**4,                      # TmaxK * constante de Boltzman ^4
      bz_TminK = bzConst * (Tmin+273.16)**4,                      # TminK * constante de Boltzman ^4
      airH_C = 0.34-0.14*sqrt(ea),                                # Correction de l'effet de l'humidité de l'air
      cloud_C = 1.35*(Rs/Rso)-0.35,                               # Correction de l'effet de la nébulosité
      #calcul final du rayonnement net à ondes longes
      Rnl = round((bz_TmaxK + bz_TminK)/2 * airH_C * cloud_C, 2),
      ###le rayonnement net
      Rn = Rns - Rnl
    )%>%
    #réarrangement des colonnes
    dplyr::select(
      #Pa : Pression atmosphérique brute
      #Pa_adj : Pression atmosphérique corrigée pour l'altitude
      #------------------------------------------------------------#
      nom, longitude, latitude, altitude, date, Rs, Ra, Rns, Rso, Rnl, Rn, Tmax, Tmin, Tmean,
      HRmean, P, U2, delta, es, ea, es_ea, delta, gamma

    )
}
