create_credentials <- function() {
  credentials <- data.table(
    user = c(
      "m.bisogni",
      "i.bertolini",
      "r.fontana",
      "mv.marchiano",
      "t.morello",
      "d.bianchini",
      "m.camoroso",
      "e.buonvino",
      "e.ciocca",
      "l.debernardin",
      "i.grimaldi",
      "e.napolillo",
      "p.piccirillo",
      "m.quercia",
      "a.santacatterina",
      "v.spagnoletti",
      "g.colantonio",
      "m.filomeno",
      "m.basilico",
      "p.fantini",
      "consiglierecsm"
      #"test"
    ),
    password = c(
      "mbisogni151",
      "ibertolini201",
      "rfontana322",
      "mvmarchiano173",
      "tmorello345",
      "dbianchini546",
      "mcamoroso467",
      "ebuonvino839",
      "eciocca248",
      "ldebernardin357",
      "igrimaldi984",
      "enapolillo392",
      "ppiccirillo708",
      "mquercia149",
      "asantacatterina568",
      "vspagnoletti995",
      "gcolantonio341",
      "mfilomeno664",
      "mbasilico387",
      "pfantini100",
      "pnrr2023"
      #"test100"
    )
  )
  return(credentials)
  
  # start = c("2019-04-15"), # optional (all others)
  # expire = c(NA, "2019-12-31"),
  # admin = c(FALSE, TRUE),
  # comment = "Simple and secure authentification mechanism
  # for single ‘Shiny’ applications.",
  # stringsAsFactors = FALSE
  #)
}
