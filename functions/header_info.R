header_info <- function() {
  head <-
    div(
      style = "width:175%",
      "Andamento degli indicatori obiettivo per il raggiungimento dei target PNRR",
      img(src = "logoCSM.png", align = "right", .noWS = "inside", width = "15%"),
      h5("Livello nazionale, distrettuale e singola sede (con dettaglio per materia)"),
      h5("Periodo di riferimento: 2019/I semestre 2026 - dati semestrali"),
      h5("Aggiornamento: 31/12/2024"),
      h5("Consiglio superiore della magistratura - Ufficio statistico")
    )
  return(head)
}
