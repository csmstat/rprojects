wrap_by <- function(..., is_grid = F) {
  if(is_grid) {
    facet_grid(vars(...), scales = "free", labeller = label_value)
  } else {
    facet_wrap(vars(...), scales = "free", labeller = label_value)
  }
}