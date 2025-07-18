as_DT <- function(info) {
  DT <- datatable(
    info,
    extensions = c('Scroller', 'Buttons'),
    class      = 'compact cell-border stripe',
    rownames   = F,
    filter     = "none",
    options    = list(
      deferRender = F,
      scroller    = F,
      dom         = 'Bfrtip',
      buttons     = c('copy', 'print', 'csv', 'excel')
    )
  ) %>%
    formatPercentage(
      c(
        # "var% [2019 vs 2026] (post UPP - grigia)",
        # "var% [2019 vs 2026] (pre UPP - celeste)",
        # "var% [2019 vs 2026] (tutte le oss. disp. - viola)",
        "var% [corrente] (linea nera)",
        "var% [proiezione] (linea viola)"
        ),
      digits = 2
      )
  return(DT)
}

