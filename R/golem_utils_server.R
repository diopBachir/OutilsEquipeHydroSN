#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

###==========================CALCUL DE L'ETo==============================#

#-------------------------------------------------------------------------#
###Equation Penman-Monteith###
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_PM<-function(tableDB){
  round(((0.408*tableDB$delta*tableDB$Rn)+
           ((tableDB$gamma*900)/(tableDB$Tmean+273))*
              tableDB$U2*(tableDB$es-tableDB$ea))/(tableDB$delta+
                tableDB$gamma*(1+(0.34*tableDB$U2))), 2)
}

###Les méthodes à base de température###

#1.Hargreaves [HG], 1975
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_HG_1975<-function(tableDB){
  round(0.0135*0.408*tableDB$Rs*(tableDB$Tmean+17.8), 2)
}

#2.Hargreaves and Samani [HS], 1975
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_HS_1985<-function(tableDB){
  round(0.408*0.0023*(tableDB$Tmean+17.8)*(tableDB$Tmax-tableDB$Tmin)^.5*tableDB$Ra, 2)
}

#3.Trajkovic [TR], 2007
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_TRA_2007<-function(tableDB){
  round(0.408*0.0023*(tableDB$Tmean+17.8)*(tableDB$Tmax-tableDB$Tmin)^0.424*tableDB$Ra, 2)
}

#4.Droogers et Allen [DA], 2012
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_DA_2012<-function(tableDB){
  round(0.408*0.0025*(tableDB$Tmean+16.8)*(tableDB$Tmax-tableDB$Tmin)^.5*tableDB$Ra , 2)
}

#5.Heydari et Heydari (2012)
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_HH_2012<-function(tableDB){
  round(0.0023*tableDB$Ra*(tableDB$Tmean+9.519)*(tableDB$Tmax-tableDB$Tmin)^.611 , 2)
}

#----------------------------------------------------------------------------------------------#

###Les méthodes à base de radiation###
#1.Makkink [MK], 1957
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_MK_1957<-function(tableDB){
  round(0.61*(tableDB$delta/(tableDB$delta+tableDB$gamma))*(tableDB$Rs/2.45) - 0.012, 2)
}

#2.Jensen and Haise [JH], 1963#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_JH_1963<-function(tableDB){
  round(0.025*(tableDB$Tmean-3)*tableDB$Rs, 2)
}

#3.Priestley-Taylor [PT], 1972
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_PT_1972<-function(tableDB, alpha){
  round(1.26*(tableDB$delta/(tableDB$delta+tableDB$gamma))*(tableDB$Rn/2.45), 2)
}

#4. Abtew [AB], 1996
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_AB_1996<-function(tableDB){
  round(0.53*(tableDB$Rs/2.45), 2)
}

#8. Oudin [OD], 2005
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_OD_2005<-function(tableDB){
  round(tableDB$Rs*((tableDB$Tmean+5)/100), 2)
}


###Les méthodes de transfert de masse###

#1. Dalton [DN], 1802
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_DN_1802<-function(tableDB){
  round((0.3648+0.07223*tableDB$U2)*(tableDB$es-tableDB$ea), 2)*4.08
}

#2. Trabert [TR], 1896
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_TR_1896<-function(tableDB){
  round(0.3075*sqrt(tableDB$U2)*(tableDB$es-tableDB$ea), 2)*4.08
}

#3. Penman [PNM], 1948
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_PNM_1948<-function(tableDB){
  round(0.35*(1+0.24*tableDB$U2)*(tableDB$es-tableDB$ea), 2)*4.08
}

#3. Rohwer [RW], 1962
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_RW_1962<-function(tableDB){
  round(0.44*(1+0.27*tableDB$U2)*(tableDB$es-tableDB$ea), 2)*4.08
}

#4. Mahringer [MA], 1970
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_MA_1970<-function(tableDB){
  round(0.15072*sqrt(3.6)*tableDB$U2*(tableDB$es-tableDB$ea), 2)*4.08
}

###Les méthodes combinatoires###

#1. Penman [PNM], 1963
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_PN_1963<-function(tableDB){
  round(((tableDB$delta/(tableDB$delta+tableDB$gamma))*
           tableDB$Rn+(tableDB$gamma/(tableDB$delta+tableDB$gamma))*
            6.43*(1+0.053*tableDB$U2)*(tableDB$es-tableDB$ea))/2.45, 2)
}

#2. Doorenbos-Pruitt [DP], 1977
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_DP_1977<-function(tableDB){
  round(((tableDB$delta/(tableDB$delta+tableDB$gamma))*tableDB$Rn+
           2.7*(tableDB$gamma/(tableDB$delta+tableDB$gamma))*
            (1+0.864*tableDB$U2*(tableDB$es-tableDB$ea)))/2.45, 2)
}

#3. Valiantzas 1 [VAL1], 2012
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_VAL1_2012<-function(tableDB){
  # latitude de la station en deres radians
  lat_rad<-round(REdaS::deg2rad(tableDB$latitude),2)
  round(0.0393*tableDB$Rs*sqrt(tableDB$Tmean+9.5)-
          0.19*(tableDB$Rs**0.6)*(lat_rad**0.15)+
            0.048*(tableDB$Tmean+20)*(1-(tableDB$HRmean/100))*
              tableDB$U2**0.7, 2)
}

#4. Valiantzas 2 [VAL2], 2012
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_VAL2_2012<-function(tableDB){
  # latitude de la station en deres radians
  lat_rad<-round(REdaS::deg2rad(tableDB$latitude),2)
  round(0.0393*tableDB$Rs*sqrt(tableDB$Tmean+9.5)-
          0.19*(tableDB$Rs**0.6)*(lat_rad**0.15)+
            0.078*(tableDB$Tmean+20)*(1-(tableDB$HRmean/100)), 2)
}

#5. Valiantzas 3 [VAL3], 2012
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET_VAL3_2012<-function(tableDB){
  lat_rad<-round(REdaS::deg2rad(tableDB$latitude),2)
  round(0.0393*tableDB$Rs*sqrt(tableDB$Tmean+9.5)-
          0.19*(tableDB$Rs**0.6)*(lat_rad**0.15)+
            0.0061*(tableDB$Tmean+20)*
              (1.12*tableDB$Tmean-tableDB$Tmin-2)**0.7, 2)
}

#-------------------------------------------------------------------------#
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ET0<-function(dataComplete){
  #----------------------------------------------------------------------------#
  #dataComplete : table contenant les données météorologiques
  #----------------------------------------------------------------------------#
  
  #calcul de l'ETo à partir des 21 équations retenues
  dataComplete%>%
    dplyr::mutate(
      ## FAO-56-PM
      ETo_PM = ET_PM(dataComplete),
      
      ## Méthodes à base de température ET_HG_1975 
      
      #1.Hargreaves [HG], 1975
      ETo_HG_1975 = ET_HG_1975(dataComplete),
      #2.Hargreaves and Samani [HS], 1975
      ETo_HS_1985 = ET_HS_1985(dataComplete),
      #3.Trajkovic [TR], 2007
      ETo_TRA_2007 = ET_TRA_2007(dataComplete),
      #4.Droogers et Allen [DA], 2012
      ETo_DA_2012 = ET_DA_2012(dataComplete),
      #5.Heydari et Heydari (2012)
      ETo_HH_2012 = ET_HH_2012(dataComplete),
      
      ###Les méthodes à base de radiation###
      
      #1.Makkink [MK], 1957
      ETo_MK_1957 = ET_MK_1957(dataComplete),
      #2.Jensen and Haise [JH], 1963
      ETo_JH_1963 = ET_JH_1963(dataComplete),
      #3.Priestley-Taylor [PT], 1972
      ETo_PT_1972 = ET_PT_1972(dataComplete),
      #4. Abtew [AB], 1996
      ETo_AB_1996 = ET_AB_1996(dataComplete),
      #5. Oudin [OD], 2005
      ETo_OD_2005 = ET_OD_2005(dataComplete),
      
      ###Les méthodes de transfert de masse###
      
      #1. Dalton [DN], 1802
      ETo_DN_1802 = ET_DN_1802(dataComplete),
      #2. Trabert [TR], 1896
      ETo_TR_1896 = ET_TR_1896(dataComplete),
      #3. Penman [PNM], 1948
      ETo_PNM_1948 = ET_PNM_1948(dataComplete),
      #3. Rohwer [RW], 1962
      ETo_RW_1962 = ET_RW_1962(dataComplete),
      #5. Mahinger [MA], 1970
      ETo_MA_1970 = ET_MA_1970(dataComplete),
      
      ###Les méthodes combinatoires###
      
      #1. Penman [PN], 1963
      ETo_PN_1963 = ET_PN_1963(dataComplete),
      #2. Doorenbos-Pruitt [DP], 1977
      ETo_DP_1977 = ET_DP_1977(dataComplete),
      #3. Valiantzas 1 [VAL], 2012
      ETo_VAL1_2012 = ET_VAL1_2012(dataComplete),
      #4. Valiantzas 2 [VAL], 2012
      ETo_VAL2_2012 = ET_VAL2_2012(dataComplete),
      #5. Valiantzas 3 [VAL], 2012
      ETo_VAL3_2012 = ET_VAL3_2012(dataComplete)
    )
}

### create temporal excel file
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx")
  writexl::write_xlsx(.data,tmp)
  browseURL(tmp)
}