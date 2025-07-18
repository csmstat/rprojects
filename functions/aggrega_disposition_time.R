aggrega_disposition_time <- function(dt, keyby_cols) {
  tmp <- dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)), keyby = keyby_cols, .SDcols = c("Definiti", "Pendenti Finali")]
  tmp[grep("-06-30", Data), `Disposition Time` := ifelse(`Definiti` > 0, round((`Pendenti Finali`/`Definiti`)*182.5, 0), NA)]  # dati semestrali
  tmp[grep("-12-31", Data), `Disposition Time` := ifelse(`Definiti` > 0, round((`Pendenti Finali`/`Definiti`)*365,   0), NA)]  # dati annuali
  tmp[grep("tg26",   Data), `Disposition Time` := ifelse(`Definiti` > 0, round((`Pendenti Finali`/`Definiti`)*365,   0), NA)]  # target
  tmp <- melt(tmp, id.vars = keyby_cols, measure.vars = c("Disposition Time", "Pendenti Finali"))  # i definiti vengono generati dal successivo aggrega_definiti (per via dei dati semestrali)
  #tmp <- melt(tmp, id.vars = keyby_cols, measure.vars = c("Disposition Time"))
  return(tmp)
  }
