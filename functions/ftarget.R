ftarget <- function(tmp, facet_vars, is_checkbox = F, is_delta = F, delta = 0) {
  
  if(is_delta) {
    tmp_x_delta <- copy(tmp)
    tmp <- copy(tmp)[variable == "Disposition Time"]
  }
  
  tmp <- tmp[!is.na(value)]
  tmp <- tmp[!date %in% c("2019-06-30")]  # TODO: da rivedere
  
  all_sems      <- sort(c(paste0(2019:2026, "-06-30"),  paste0(2019:2025, "-12-31")))  
  post_upp_sems <- all_sems[all_sems >= "2022-06-30"]
  max_date      <- tmp[!date %in% c("tg26")][,.N, keyby = date][, max(date)]  # ultima data corrente
  variabile     <- tmp[, .N, keyby = variable][, variable]
  
  if(is_checkbox) {
    form            <- "value ~ as.numeric(as_date(date))"
    #models_all_obs  <- tmp[!date %in% c("tg26")][,                        list(fit = lapply(form, lm, data = .SD)), keyby = facet_vars]  # modello stimato a partire da tutte le osservazioni disponibili
    models_post_upp <- tmp[!date %in% c("tg26")][date %in% post_upp_sems, list(fit = lapply(form, lm, data = .SD)), keyby = facet_vars]  # modello stimato a partire dal semestre successivo a introduzione UPP
    fpred <- function(x) {
      dt_pred <- data.table(
        "date"          = all_sems,   
        facet_vars      = x,
        #"pred_all_obs"  = predict(models_all_obs[get(facet_vars)  == x]$fit[[1]], newdata = data.table(date = all_sems)),  # trend su tutti i semestri possibili (utilizzando il modello di sopra) 
        "pred_post_upp" = predict(models_post_upp[get(facet_vars) == x]$fit[[1]], newdata = data.table(date = all_sems)),  # trend sui semestri post UPP (utilizzando il modello di sopra)
        "variable"      = variabile
      )
      setnames(dt_pred, "facet_vars", facet_vars)
      return(dt_pred)
    }
    #dt_pred <- rbindlist(lapply(models_all_obs[[facet_vars]], function(x) fpred(x)), use.names = T, fill = T)
    dt_pred <- rbindlist(lapply(models_post_upp[[facet_vars]], function(x) fpred(x)), use.names = T, fill = T)
    info    <- merge(
      tmp[    date == "tg26",       .(get(facet_vars), "target_UE"      = round(value, 0))],
      #dt_pred[date == "2026-06-30", .(get(facet_vars), "target_all_obs" = round(pred_all_obs, 0))],
      dt_pred[date == "2026-06-30", .(get(facet_vars), "target_post_upp" = round(pred_post_upp, 0))],
      by = "V1"
    )
    info <- merge(tmp[date == "2019-12-31",     .(get(facet_vars), "baseline"        = round(value, 0))], info, by = "V1")
    info <- merge(tmp[date == max_date,         .(get(facet_vars), "max_date"        = round(value, 0))], info, by = "V1")  
    #info <- merge(dt_pred[date == "2026-06-30", .(get(facet_vars), "target_post_upp" = round(pred_post_upp, 0))], info, by = "V1")
    setnames(info, c("V1"), facet_vars)
  } else {
    if(is_delta) {
      ll <- list()
      for(v in c("Pendenti Finali", "Definiti")) {
        tmp_ <- tmp_x_delta[variable == v]
        #model_all_obs  <- lm(value ~ as.numeric(as_date(date)), data = tmp_[!date %in% c("tg26")])                           # modello stimato a partire da tutte le osservazioni disponibili
        model_post_upp <- lm(value ~ as.numeric(as_date(date)), data = tmp_[!date %in% c("tg26")][date %in% post_upp_sems])  # modello stimato a partire dal semestre successivo a introduzione UPP
        ll[[v]] <- data.table(
          "date"          = all_sems,
          #"pred_all_obs"  = predict(model_all_obs,  data.table(date = all_sems)),  # trend su tutti i semestri possibili (utilizzando il modello di sopra)
          "pred_post_upp" = predict(model_post_upp, data.table(date = all_sems)),  # trend sui semestri post UPP (utilizzando il modello di sopra)
          "variable"      = v
        )
      }
      dt_pred <- rbindlist(ll, use.names = T, fill = T)
      dt_pred <- dcast(dt_pred, date ~ variable, value.var = "pred_post_upp")
      dt_pred[, `Disposition Time` := ifelse(`Definiti` > 0, round(((`Pendenti Finali`)/(`Definiti`))*182.5, 0), NA)]  # dati semestrali
      dt_pred[date == "2026-06-30", `Disposition Time` := ifelse(`Definiti` + delta > 0, round(((`Pendenti Finali` - delta)/(`Definiti` + delta))*182.5, 0), NA)]  # dati semestrali
      dt_pred <- melt(dt_pred, id.vars = "date", measure.vars = c("Disposition Time"), value.name = "pred_post_upp")
    } else {
    #model_all_obs  <- lm(value ~ as.numeric(as_date(date)), data = tmp[!date %in% c("tg26")])                           # modello stimato a partire da tutte le osservazioni disponibili
    model_post_upp <- lm(value ~ as.numeric(as_date(date)), data = tmp[!date %in% c("tg26")][date %in% post_upp_sems])  # modello stimato a partire dal semestre successivo a introduzione UPP
    dt_pred <- data.table(
      "date"          = all_sems,
      #"pred_all_obs"  = predict(model_all_obs,  data.table(date = all_sems)),  # trend su tutti i semestri possibili (utilizzando il modello di sopra)
      "pred_post_upp" = predict(model_post_upp, data.table(date = all_sems)),  # trend sui semestri post UPP (utilizzando il modello di sopra)
      "variable"      = variabile
      )
    }
    info <- data.table(
      "baseline"        = tmp[    date == "2019-12-31", round(value, 0)],
      "target_UE"       = tmp[    date == "tg26",       round(value, 0)],
      "max_date"        = tmp[    date == max_date,     round(value, 0)],
      #"target_all_obs"  = dt_pred[date == "2026-06-30", round(pred_all_obs, 0)],
      "target_post_upp" = dt_pred[date == "2026-06-30", round(pred_post_upp, 0)]
    )
  }
  
#info[, `target_all_obs_var_perc`  := round((`target_all_obs` - baseline)/baseline, 2)]
info[, `target_post_upp_var_perc` := round((`target_post_upp` - baseline)/baseline, 2)]
info[, `max_date_var_perc`        := round((`max_date` - baseline)/baseline, 2)]

# if(is_checkbox){
setcolorder(
  info,
  c(
    facet_vars,
    "baseline",
    "max_date",
    "target_UE",
    #"target_all_obs",
    #"target_all_obs_var_perc",
    "target_post_upp",
    "max_date_var_perc",
    "target_post_upp_var_perc"
  )
)
# }
setnames(
  info,
  c(
    "baseline",
    "max_date",
    "target_UE",
    #"target_all_obs",
    #"target_all_obs_var_perc",
    "target_post_upp",
    "max_date_var_perc",
    "target_post_upp_var_perc"
    ),
  c(
    "baseline",
    "dato corrente",
    "target",
    #"proiezione 2026 (tutte le osservazioni)",
    "proiezione 2026 (linea viola)",
    "var% [corrente] (linea nera)",
    "var% [proiezione] (linea viola)"
    ),
  skip_absent = T
  )
names(info) <- gsub("_", " ", tolower(names(info)))

out <- list(
  "info"    = info,  # [, -c("baseline")] 
  "dt_pred" = dt_pred
  )
return(out)
}
