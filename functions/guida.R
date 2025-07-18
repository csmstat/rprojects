guida <- function() {
  gui <-
    div(
      # p(style = "text-align:justify;color:gray", img(src = "italy.svg", align = "center", width = "5.4%"), "baseline 2019"),
      # p(style = "text-align:justify;color:gray", img(src = "ue.svg",    align = "center", width = "5.7%"), "target 2026 (e 2024)"),
      # hr(), 
      h5(strong("Linea nera piena"),                                        "andamento", strong("osservato"), "dell'indicatore", style = "color:gray"),
      #h5(strong("Linea grigia tratteggiata (scenario post UPP)"),           "andamento previsto dell'indicatore in base al dato osservato a partire dal 31.12.2021 (post assunzione UPP)",                             style = "color:gray"), 
      #h5(strong("Linea celeste tratteggiata (scenario pre UPP)"),           "andamento previsto dell'indicatore in base al dato osservato a partire dal 30.06.2019 e fino al 31.12.2021 (pre assunzione UPP)",         style = "color:gray"),
      #h5(strong("Linea viola tratteggiata (scenario indipendente da UPP)"), "andamento previsto dell'indicatore in base al dato osservato fino all'ultimo semestre disponibile (indipendentemente da assunzione UPP)", style = "color:gray"),
      h5(strong("Linea viola tratteggiata"),                                "andamento", strong("previsto"),  "dell'indicatore (trend)", style = "color:gray"),
      hr(),
      h5(strong("NOTA METODOLOGICA"), style = "color:#D81B60"),
      div(
        style = "text-align:justify;color:gray",
        #h5(strong("Target nazionali"), style = "color:gray"),
        p(
          "I target negoziati con la Commissione europea sono fissati a",
          strong("livello nazionale"), "quali unici target ufficiali esigibili."
        ),
        # p(
        #   "I target negoziati con la Commissione europea (bandiera UE) sono fissati a",
        #   strong("livello nazionale"), "quali unici target ufficiali esigibili e assumono il",
        #   strong("2019"), "come anno-base rispetto al quale calcolare le riduzioni percentuali (baseline)."
        # ),
        p(
          "Nei grafici, per semplicità, si sono invece ipotizzati uniformi gli obiettivi PNRR per tutti i livelli di dettaglio considerati (tipo ufficio, distretto, sede e materia/sezione).
          Questi ultimi dunque sono da intendersi come meramente indicativi e" , strong("non rappresentano in alcun modo obiettivi di sede dei singoli uffici."),
        ),
        p(
          "La base statistica del monitoraggio è fornita dal Ministero della giustizia,
          così come stabilita con la Commissione europea nell’ambito del Piano Nazionale di Ripresa e Resilienza.",
        )
        # p(
        #   "Per motivi di convenienza, il termine del periodo di stima è stato posto pari al", strong("31.12.2026"), "anziché al 30.06.2026."
        # )
      )
    )
  return(gui)
}