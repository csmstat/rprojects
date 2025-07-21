splot <- function(tmp, lvar, sede, xlab, ylab) {
  text <- paste0(lvar, ": ", tmp[["value"]], "\nturnover: ", tmp[["Turnover Totale"]], "\nufficio: ", tmp[[sede]])
  sp <-
    ggplot(tmp, aes(x = `Turnover Totale`, y = value, group = get(sede), colour = tipo_ufficio, text = text, size = 15)) +
    facet_wrap(~tipo_ufficio, scales = "free") +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      legend.position = "none"
    )
  sp <- ggplotly(sp, tooltip = "text")
  return(sp)
  }
