aggrega_arretrato <- function(dt, keyby_cols) {
  tmp <- dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)), keyby = keyby_cols, .SDcols = c("Pendenti19", "Pendenti22")]
  tmp <- melt(tmp, id.vars = keyby_cols, measure.vars = c("Pendenti19", "Pendenti22"))
  return(tmp)
}