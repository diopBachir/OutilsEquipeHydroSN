#' import_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import tools
#' @import readxl

# import des données csv ou excel
import_data<- function(input_id_datapath){
  #----------------------------------------------------------------------------#
  ## Parametres:
  #  ===========#
  # input_id_datapath : L'entrée uploadant les données
  #----------------------------------------------------------------------------#
  if(tools::file_ext(input_id_datapath)=="csv"){
    #* si le fichier importé est un csv
    #* détecter le séparateur de colonnes
    data<-if(grepl("\t", readLines(input_id_datapath)[1])){
      if(grepl(".", readLines(input_id_datapath)[1], fixed = TRUE)){
        read.csv2(input_id_datapath, dec = ".", sep="\t")
      }else{
        read.csv2(input_id_datapath, dec = ",", sep="\t")
      }
    }else if(grepl(";", readLines(input_id_datapath)[1])){
      if(grepl(".", readLines(input_id_datapath)[1], fixed = TRUE)){
        read.csv2(input_id_datapath, dec = ".")
      }else{
        read.csv2(input_id_datapath, dec = ",")
      }
    }else{
      if(grepl(".", readLines(input_id_datapath)[1], fixed = TRUE)){
        read.csv(input_id_datapath, dec = ".")
      }else{
        read.csv(input_id_datapath, dec = ",")
      }
    }

  }else{# si c'est plutot un fichier excel
    data<- readxl::read_excel(input_id_datapath)
  }

  return(data)
}
