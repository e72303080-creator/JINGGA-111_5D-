x_data <- c(1, 2, 4)
y_data <- c(2, 3, 5)

x_grid <- seq(
  min(x_data),
  max(x_data),
  length.out = 300
)

y_grid <- sapply(
  x_grid,
  function(x) lagrange(x_data, y_data, x)
)

plot(
  x_grid,
  y_grid,
  type = "1",
  lwd = 2,
  xlab = "x",
  y_lab = "P(x)",
  main = "Interpolasi Polinomial Lagrange"
)

points(
  x_data,
  y_data,
  pch = 19
)

points(
  3,
  lagrange(x_data, y_data, 3),
  pch = 19
)