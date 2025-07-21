fonti <- function() {
  fon <- 
    div(
      h5(strong("Civile"), "Datawarehouse della Giustizia Civile (Ministero della Giustizia)", style = "text-align:justify;color:gray"),
      h5(strong("Penale"), "Modelli trimestrali estratti dagli uffici giudiziari dai registri informatizzati e comunicati alla DGStat (Ministero della Giustizia)", style = "text-align:justify;color:gray"),
      h5(strong("Cassazione"), "Data Base SIC civile e penale (Elaborazioni dell’Ufficio di statistica della Corte di Cassazione)", style = "text-align:justify;color:gray"),
      h5("Download", style = "color:gray"),
      tags$a(
        h5("Monitoraggio obiettivi PNRR"),
        target = "_blank",
        href = "https://www.giustizia.it/giustizia/page/it/pnrr_monitoraggio_obiettivi"
        )
      # tags$a(
      #   h5("Relazione sull’attuazione degli interventi"),
      #   target = "_blank",
      #   href = "https://www.giustizia.it/cmsresources/cms/documents/pnrr_relazione_su_attuazione_interventi_apr2023.pdf"
      # ),
      # tags$a(
      #   h5("Relazione sul monitoraggio statistico-giudiziario"),
      #   target = "_blank",
      #   href = "https://www.giustizia.it/cmsresources/cms/documents/pnrr_relazione_monitoraggio_statistico_agg_dic2022.pdf"
      # ),
      # tags$a(
      #   h5("Nota metodologica (sulla definizione degli indicatori)"),
      #   target = "_blank",
      #   href = "https://webstat.giustizia.it/SiteAssets/SitePages/Monitoraggio%20PNRR/Nota%20metodologica.pdf"
      # ),
      # tags$a(
      #   h5("Monitoraggio PNRR (dati)"),
      #   target = "_blank",
      #   href = "https://webstat.giustizia.it/SiteAssets/SitePages/Monitoraggio%20PNRR/Monitoraggio%20PNRR.xlsx"
      # )
    )
  return(fon)
}    

