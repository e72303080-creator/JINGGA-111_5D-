x <- c(1, 3)
y <- c(2, 6)

plot(x, y,
     pch = 19,
     xlab = "x",
     ylab = "f(x)",
     main = "Interpolasi Linear")

abline(lm(y ~ x), lwd = 2)

points(2, 4, pch = 19)

text(1, 2, "(1, 2)", pos = 2)
text(3, 6, "(3, 6)", pos = 4)
text(2, 4, "(2, 4)", pos = 3)