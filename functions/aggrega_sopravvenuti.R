aggrega_sopravvenuti <- function(dt, keyby_cols) {
  
  tmp <- dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)), keyby = keyby_cols, .SDcols = c("Sopravvenuti")]
  tmp <- melt(tmp, id.vars = keyby_cols, measure.vars = "Sopravvenuti")
  
  # Calcola i valori semestrali per differenza.
  
  mm <- NULL
  ss <- tmp[, .N, keyby = Settore][, Settore]
  tt <- tmp[, .N, keyby = `Tipo Ufficio`][, `Tipo Ufficio`]
  if("Macroarea CSM" %in% keyby_cols) {
    mm <- tmp[, .N, keyby = `Macroarea CSM`][, `Macroarea CSM`]
  }
  vv <- tmp[, .N, keyby = variable][, variable]
  aa <- 2019:2026
  
  if(!is.null(mm)) {
    for(a in aa) {
      for(s in ss) {
        for(t in tt) {
          for(m in mm) {
            for(v in vv) {
              num <- tmp[date == paste(a, "06", "30", sep = "-") & Settore == s & `Tipo Ufficio` == t & `Macroarea CSM` == m & variable == v, value]
              tmp[date == paste(a, "12", "31", sep = "-") & Settore == s & `Tipo Ufficio` == t & `Macroarea CSM` == m & variable == v, value := value - num]
            }
          }
        }
      }
    }
  } else {
    for(a in aa) {
      for(s in ss) {
        for(t in tt) {
          #for(m in mm) {
          for(v in vv) {
            num <- tmp[Data == paste(a, "06", "30", sep = "-") & Settore == s & `Tipo Ufficio` == t & variable == v, value]
            tmp[Data == paste(a, "12", "31", sep = "-") & Settore == s & `Tipo Ufficio` == t & variable == v, value := value - num]
          }
          #}
        }
      }
    }
  }

  return(tmp)
}
