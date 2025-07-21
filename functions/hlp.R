hlp <- function(is_AR_tg26 = F){
  if(!is_AR_tg26) {
    txt <- paste(
      "Linea nera piena: andamento osservato dell'indicatore a partire dal 31.12.2019 (baseline) e fino all'ultimo semestre disponibile.",
      #"Linea grigia tratteggiata: andamento previsto dell'indicatore in base al dato osservato a partire dal 31.12.2019 e fino all'ultimo semestre disponibile;",
      #"Linea celeste tratteggiata (scenario pre UPP): andamento previsto dell'indicatore in base al dato osservato a partire dal 30.06.2019 e fino al 31.12.2021;",
      #"Linea viola tratteggiata (scenario indipendente da UPP): andamento previsto dell'indicatore in base al dato osservato fino all'ultimo semestre disponibile."
      "Linea viola tratteggiata: andamento previsto dell'indicatore in base al dato osservato a partire dal 30.06.2022 (UPP) e fino all'ultimo semestre disponibile (il target finale è indicato dal simbolo del mirino)."
    )
  } else {
    txt <- paste(
      "Linea nera piena: andamento osservato dell'indicatore a partire dal 31.12.2022 (baseline) e fino all'ultimo semestre disponibile.",
      #"Linea grigia tratteggiata: andamento previsto dell'indicatore in base al dato osservato a partire dal 31.12.2019 e fino all'ultimo semestre disponibile;",
      #"Linea celeste tratteggiata (scenario pre UPP): andamento previsto dell'indicatore in base al dato osservato a partire dal 30.06.2019 e fino al 31.12.2021;",
      #"Linea viola tratteggiata (scenario indipendente da UPP): andamento previsto dell'indicatore in base al dato osservato fino all'ultimo semestre disponibile."
      "Linea viola tratteggiata: andamento previsto dell'indicatore in base al dato osservato a partire dal 31.12.2022 (baseline) e fino all'ultimo semestre disponibile."
    )
  }
  h <- span(`data-toggle` = "tooltip", `data-placement` = "right", title = txt, icon("info-circle"))
  return(h)
}