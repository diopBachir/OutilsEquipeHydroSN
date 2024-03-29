#' read_shp_wrap
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

read_shp_wrap<- function(inputID){
  # shpdf is a data.frame with the name, size, type and datapath
  # of the uploaded files
  shpdf <- inputID

  # The files are uploaded with names
  # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
  # (path/names are in column datapath)
  # We need to rename the files with the actual names:
  # file.dbf, etc.
  # (these are in column name)

  # Name of the temporary directory where files are uploaded
  tempdirname <- dirname(shpdf$datapath[1])

  # Rename files
  for (i in 1:nrow(shpdf)) {
    file.rename(
      shpdf$datapath[i],
      paste0(tempdirname, "/", shpdf$name[i])
    )
  }

  # Now we read the shapefile with readOGR() of rgdal package
  # passing the name of the file with .shp extension.

  # We use the function grep() to search the pattern "*.shp$"
  # within each element of the character vector shpdf$name.
  # grep(pattern="*.shp$", shpdf$name)
  # ($ at the end denote files that finish with .shp,
  # not only that contain .shp)
  paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/")

}
