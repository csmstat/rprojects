hlp_to <- function(){
  txt <- paste(
    "Arretato pro-capite: arretrato civile rapportato all’organico destinato al settore civile;",
    "Turnover: tasso di turnover complessivo registrato nel quinquennio 2017-2021 nei singoli uffici."
  )
  h <- span(`data-toggle` = "tooltip", `data-placement` = "right", title = txt, icon("info-circle"), )
  return(h)
}