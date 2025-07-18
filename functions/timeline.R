timeline <- function() {
  timelineBlock(
    width = 12,
    timelineLabel(2026, color = "teal"),
    timelineItem(
      title = div(tags$img(src = "ue.svg", align = "center", width = "3.5%"), "target finale"),
      time = "30.06.2026",
      div(
        h5(strong("Disposition Time civile complessivo - giorni - (somma dei tre gradi di giudizio) -40%"), "rispetto alla baseline 2019"),
        h5(strong("Disposition Time penale complessivo - giorni - (somma dei tre gradi di giudizio) -25%"), "rispetto alla baseline 2019"),
        h5(strong("Arretrato civile -90%"), "rispetto alla baseline 2022", "(Tribunali e Corti)")
      )
    ),
    timelineLabel(2024, color = "teal"),
    timelineItem(
      title = div(tags$img(src = "ue.svg", align = "center", width = "3.5%"), "target intermedio (arretrato civile)"),
      time = "31.12.2024",
      div(
        h5(strong("Arretrato civile -95%"), "rispetto alla baseline 2019", "(Tribunali e Corti)")
        #h5(strong("Arretrato civile -95%"), "Corti",             "riduzione del 95% rispetto alla baseline 2019")
      )
    ),
    timelineLabel(2022, color = "teal"),
    timelineItem(
      title = div(tags$img(src = "italy.svg", align = "center", width = "3.35%"), "baseline 2022"),
      time = "31.12.2022",
      div(
        h5(strong("Arretrato civile"), "Tribunali 1.197.786 (pendenti al 31/12/2022 iscritti dal 01/01/2017 al 31/12/2022)"),
        h5(strong("Arretrato civile"), "Corti 179.306 (pendenti al 31/12/2022 iscritti dal 01/01/2018 al 31/12/2022)")
      )
    ),
    timelineLabel(2019, color = "teal"),
    timelineItem(
      title = div(tags$img(src = "italy.svg", align = "center", width = "3.35%"), "baseline 2019"),
      time = "31.12.2019",
      div(
        h5(strong("Disposition Time civile (giorni)"), "Tribunali 556 | Corti 654 | Cassazione 1.302 | Complessivo (somma dei tre gradi di giudizio) 2.512"),
        h5(strong("Disposition Time penale (giorni)"), "Tribunali 392 | Corti 835 | Cassazione   166 | Complessivo (somma dei tre gradi di giudizio) 1.392"),
        h5(strong("Arretrato civile"), "Tribunali 337.740 (pendenti al 31/12/2019 iscritti fino al 31/12/2016) | Corti 98.371 (pendenti al 31/12/2019 iscritti fino al 31/12/2017)")
      )
    )
  )
}