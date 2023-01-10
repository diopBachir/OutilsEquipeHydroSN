logoUI <- function(id, label = NULL) {
  fluidRow(
    align = "center",
    column(
      4, p(a(
        tags$img(
          src = "www/shiny.png", style = "width:50px;height:50px;"
        ),
        href = "https://shiny.rstudio.com/"
      ))
    ),
    column(
      4, p(a(
        tags$img(
          src = "www/UGB.jpg", style = "width:50px;height:50px;"
        ),
        href = "https://www.ugb.sn/"
      ))
    ),
    column(
      4, p(a(
        tags$img(
          src = "www/leidi.png", style = "width:50px;height:50px;"
        ),
        href = "http://leidi-ugb.online/"
      ))
    )
  )
}
