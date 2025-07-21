fplot <- function(dt, facet_vars, dt_pred = NULL, is_grid = F, title = NULL, is_delta = F, is_AR_tg26 = F, scale = 1.5, w = NULL, h = NULL) {
  
  if(is_delta) {
    dt <- copy(dt)[variable == "Disposition Time"]
  }
  
  dt <- dt[!is.na(value)]
  
  periods <- 
    list(
      "extended" = as.factor(
        c(
          #"2019-06-30",
          "2019-12-31",
          "2020-06-30",
          "2020-12-31",
          "2021-06-30",
          "2021-12-31",
          "2022-06-30",
          "2022-12-31",
          "2023-06-30",
          "2023-12-31",
          "2024-06-30",
          "2024-12-31",
          "2025-06-30",
          "2025-12-31",
          "2026-06-30"
        )
      ),
      "reduced" = as.factor(
        c(
          #"19-I",
          "19",
          "20-I",
          "20",
          "21-I",
          "21",
          "22-I",
          "22",
          "23-I",
          "23",
          "24-I",
          "24",
          "25-I",
          "25",
          "26-I"
        )
      )
    )
  
  alpha <- 0.35
  
  p <- ggplot()
  
  if(!is_AR_tg26) {
    p <- p +
      scale_x_discrete(breaks = periods$extended[c(seq(1,13,2), 14)], limits = periods$extended, labels = periods$reduced[c(seq(1,13,2), 14)]) +
      geom_line( data = dt_pred[date >= "2022-06-30"], aes(x = date, y = pred_post_upp, group = variable), color = 'purple2', linetype = 5, linewidth = 0.50, alpha = alpha) +
      geom_point(data = dt_pred[date >= "2022-06-30"], aes(x = date, y = pred_post_upp, group = variable), color = "purple2", size = 1,   alpha = alpha) +
      geom_point(data = dt[date == "2022-06-30"],      aes(x = date, y = value,         group = variable), color = "gray35",  size = 2.5, shape = 8) +
      annotate("text", x = "2022-06-30", y = 0, label = "UPP",                                             color = "gray35",  size = 3.5, angle = 180)  # etichetta UPP
      }
  if(is_AR_tg26) {
    p <- p +
      scale_x_discrete(breaks = periods$extended[seq(7,14)], limits = periods$extended[seq(7,14)], labels = periods$reduced[seq(7,14)]) +
      geom_line( data = dt_pred[date >= "2022-12-31"], aes(x = date, y = pred_arr_tg26,  group = variable), color = 'purple2', linetype = 5, linewidth = 0.50, alpha = alpha) +  # lightseagreen
      geom_point(data = dt_pred[date >= "2022-12-31"], aes(x = date, y = pred_arr_tg26,  group = variable), color = "purple2", size = 1, alpha = alpha)
  } 
  
  p <- p +
    geom_line( data = dt[!date %in% c("tg26")], aes(x = date,         y = value, group = variable), color = "gray35",  linewidth = 0.75) +
    geom_point(data = dt[!date %in% c("tg26")], aes(x = date,         y = value, group = variable), color = "gray35",  size = 1) +
    geom_point(data = dt[date == "tg26"],       aes(x = "2026-06-30", y = value, group = variable), color = "purple2", size = 4, shape = 10) +
    xlab("") +
    ylab("") +
    wrap_by(!!!parse_exprs(facet_vars), is_grid = is_grid) +
    ggtitle(title)
  
  p <- p +
    theme(
      axis.text.x      = element_text(size  = 11, color = "gray40"),  # 26456E
      axis.text.y      = element_text(size  = 11, color = "gray40"),
      strip.background = element_rect(fill  = "white"),
      strip.text       = element_text(size  = 13, color = "gray40"),
      panel.background = element_rect(fill  = "gray95"),
      panel.grid.major = element_line(color = 'gray100'),
      panel.grid.minor = element_line(color = "gray100"),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      title            = element_text(size = 8)
    )
  
  # if(is_strip) {  # serve eventualmente per plot tutti i distretti
  #   p <- p +
  #     theme(
  #       axis.text.x      = element_text(size  = 11, color = "gray40"),  # 26456E
  #       axis.text.y      = element_text(size  = 11, color = "gray40"),
  #       strip.background = element_rect(fill  = "white"),
  #       strip.text       = element_text(size  = 13, color = "gray40"),
  #       panel.background = element_rect(fill  = "gray95"),
  #       panel.grid.major = element_line(color = 'gray100'),
  #       panel.grid.minor = element_line(color = "gray100"),
  #       plot.margin      = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
  #       title            = element_text(size = 8)
  #     )
  # } else {
  #   p <- p +
  #     theme(
  #       axis.text.x      = element_text(size  = 11, color = "gray40"),  # 26456E
  #       axis.text.y      = element_text(size  = 11, color = "gray40"),
  #       strip.background = element_blank(),
  #       strip.text       = element_blank(),
  #       panel.background = element_rect(fill  = "gray95"),
  #       panel.grid.major = element_line(color = 'gray100'),
  #       panel.grid.minor = element_line(color = "gray100"),
  #       plot.margin      = unit(c(0, 0, 0, 0), units = "lines"),
  #       title            = element_text(size = 8)
  #     )
  # }
  
  p <- ggplotly(p, width = w, height = h)  # BASTA QUESTO
  
  # https://plotly.com/ggplot2/getting-started/
  # https://plotly.com/r/reference/#Layout_and_layout_style_objects
  # https://stackoverflow.com/questions/42301879/plotly-and-ggplot-with-facet-grid-in-r-how-to-to-get-yaxis-labels-to-use-tickte
  # For examples on how to specify the output container's height/width in a shiny app, see plotly_example("shiny", "ggplotly_sizing")
  
  fig <- plotly_build(p)
  
  FUN1 <- function(w, sz, an = 0, ln = 0) {
    w <- w[]
    w[["tickfont"]][["size"]] <- sz
    w[["tickangle"]] <- an
    w[["ticklen"]] <- ln
    return(w)
  }
  nn_xaxis <- names(fig$x$layout)[grep("xaxis", names(fig$x$layout))]
  nnx <- fig$x$layout[nn_xaxis]
  fig$x$layout[nn_xaxis] <- lapply(nnx, function(x) FUN1(x, sz = 11, an = 0))
  nn_yaxis <- names(fig$x$layout)[grep("yaxis", names(fig$x$layout))]
  nny <- fig$x$layout[nn_yaxis]
  fig$x$layout[nn_yaxis] <- lapply(nny, function(y) FUN1(y, sz = 11))

  fig$x$layout$margin$t <- 18
  fig$x$layout$margin$r <- 0
  fig$x$layout$margin$b <- 15
  fig$x$layout$margin$l <- 0

  if(!is.null(fig$x$layout$annotations)) {
    for(i in 1:length(fig$x$layout$annotations)){
      fig$x$layout$annotations[[i]][["font"]][["size"]] <- 13
    }
  }
  
  #save(fig, file = "fig.RData")
  return(fig)
}
