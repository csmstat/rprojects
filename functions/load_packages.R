load_packages <- function() {
  
  options(repos = c(CRAN = "http://cran.rstudio.com/"))
  
  packages <- c(
    "shiny",
    "shinydashboard",
    "shinydashboardPlus",
    #"shinyBS",
    #"shinymanager",
    "data.table",
    "ggplot2",
    #"ggthemes",
    #"ggimage",
    #"ggiraph",
    #"cowplot",
    #"rsvg",
    "plotly",
    #"widgetframe",
    #"htmlwidgets",
    "DT",
    "scales",
    "rlang",
    #"magrittr",
    "lubridate"
    #"magick",
    #"dygraphs",
    #"bslib"
    )

  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = T)
  sapply(packages, function(p) library(p, character.only = T))
  
  return(T)
}
