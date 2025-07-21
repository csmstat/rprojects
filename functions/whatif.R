whatif <- function() {
  whtf <-
    div(
      h5(strong("AGGIORNAMENTO LUGLIO 2025"), style = "color:#D81B60"),
      div(
        style = "text-align:justify;color:gray",
        p("Aggiunta la possibilità di eseguire una semplice analisi di tipo", strong("what-if"), "relativamente all'indicatore", strong("DT civile (per singolo ufficio).")),
        p("Essa consente di rispondere alla domanda: come varierebbe la proiezione del DT al 30.06.2026, se un numero addizionale (rispetto alla stima corrente) di procedimenti fosse definito nel primo semestre 2026?"),
        p(strong("N.B."), "Il numero addizionale di procedimenti deve essere indicato dall'utente nell'apposita sezione.")
      )
    )
  return(whtf)
}