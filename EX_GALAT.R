f <- function(x) x^2

x_data <- c(1, 2)
y_data <- f(x_data)

x_grid <- seq(1, 2, length.out = 200)

y_true <- f(x_grid)

y_interp <- sapply(
  x_grid,
  function(x)
    interpolasi_linear(
      1, 1,
      2, 4,
      x
    )
)

plot(
  x_grid,
  y_true,
  type = "1",
  lwd = 2,
  xlab = "x",
  ylab = "y",
  main = "Fungsi Sebenarnya dan Interpolasi Linear"
)

lines(
  x_grid,
  y_interp,
  lwd = 2,
  lty = 2
)

points(
  x_data,
  y_data,
  pch = 19
)

legend(
  "topleft",
  legend = c(
    "f(x) = x",
    "Interpolasi linear",
    "Data"
  ),
  lty = c(1, 2, NA),
  pch = c(NA, NA, 19)
)