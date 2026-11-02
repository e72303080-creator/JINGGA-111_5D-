interpolasi_linear <- function(x0, y0, x1, y1, x) {
  
  y <- y0 +
    (y1 - y0) / (x1 - x0) * (x - x0)
  
  return(y)
}

interpolasi_linear(
  x0 = 1,
  y0 = 2,
  x1 = 3,
  y1 = 6,
  x = 2
)