header <- function() {
  dashboardHeader(
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 50px}"),
      tags$style(".main-header .logo {height: 50px}")
    ),
    title = div(
      span(
        img(
          src = "logoCSM_new.png",
          height = 30,
          width = "10%"
          ),
        "Monitoraggio statistico degli indicatori PNRR Giustizia"
        ),
      align = "left",
      width = "100%",
      style = "padding-right:0px;"
      ),
    titleWidth = 650
    )
  } 
  