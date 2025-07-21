create_dt <- function(movi = NULL, stra = stra_civ, is_materia = F, is_distretto = F, is_nazionale = F, is_penale = F) {
  
  if(is_penale) {
    tmp1 <- copy(movi)
  } else {
    tmp1 <- copy(movi)
    tmp2 <- copy(stra)
  }
  
  if(is_distretto) {
    if(is_materia) {
      keyby_cols <- c(
        "Data",
        "Settore",
        "Distretto",
        "Tipo Ufficio",
        #"Ufficio",
        "Materia"
        )
    } else {
      keyby_cols <- c(
        "Data",
        "Settore",
        "Distretto",
        "Tipo Ufficio"
        #"Ufficio",
        #"Materia"
        )
      }
  } else {
      if(is_materia) {
        keyby_cols <- c(
          "Data",
          "Settore",
          "Distretto",
          "Tipo Ufficio",
          "Ufficio",
          "Materia"
          )
      } else {
        keyby_cols <- c(
          "Data",
          "Settore",
          "Distretto",
          "Tipo Ufficio",
          "Ufficio"
          #"Materia"
        )
      }
    }
  
  if(is_nazionale) {
    if(is_materia) {
      keyby_cols <- c(
        "Data",
        "Settore",
        #"Distretto",
        "Tipo Ufficio",
        #"Ufficio",
        "Materia"
      )
    } else {
      keyby_cols <- c(
        "Data",
        "Settore",
        #"Distretto",
        "Tipo Ufficio"
        #"Ufficio",
        #"Materia"
        )
      }
    }
  
  if(is_penale) {
    dt <- aggrega_disposition_time(tmp1, keyby_cols)  # solo DT nel penale
  } else {
    dt <- rbindlist(
      list(
        aggrega_disposition_time(
          tmp1,
          keyby_cols
        ),
        aggrega_sopravvenuti(
          tmp1,
          keyby_cols
        ),
        aggrega_definiti(
          tmp1,
          keyby_cols
        ),
        aggrega_arretrato(
          tmp2,
          keyby_cols
        )
      ),
      use.names = T,
      fill      = T
    )
  }
  
  #dt[, Data := as.factor(Data)]
  names(dt) <- gsub(" ", "_", tolower(names(dt)))                   # facet_vars non ama spazi bianchi
  
  facet_vars <- keyby_cols[!keyby_cols %in% c("Data", "Settore")]
  facet_vars <- c("variable", gsub(" ", "_", tolower(facet_vars)))  # idem
  
  if(!is_penale) {
    if(is_materia) {
      dt[materia == "fs Famiglia stato e capacità delle persone",                                           materia := "fs Fam. stato e capacità delle persone"]
      dt[materia == "g Volontaria giurisdizione e procedure camerali non in materia di famiglia e persone", materia := "g VG non in materia di famiglia e persone"]
      dt[materia == "gi Volontaria giurisdizione in materia di Impresa",                                    materia := "gi VG in materia di Impresa"]
      dt[materia == "ip Immigrazione e Protezione Internazionale",                                          materia := "ip Immigrazione e Prot. Internazionale"]
      dt[materia == "j Procedimenti a cognizione sommaria o cautelare (esclusi decreti ingiuntivi)",        materia := "j Cogniz. sommaria o cautelare (esclusi d.i.)"]
      dt[materia == "l Tutele, curatele, amministrazioni di sostegno",                                      materia := "l Tutele, curatele, amm. di sostegno"]
    }
  }
  
  setnames(dt, "data", "date")  # data è termine riservato  
  
  # dt[, `:=` (
  #   anno     = year(ydm(data)),
  #   semestre = semester(ydm(data))
  #   )
  # ]
  
  out <- list(
    "facet_vars" = facet_vars,
    "dt"         = dt
    )
  
  return(out)
}
