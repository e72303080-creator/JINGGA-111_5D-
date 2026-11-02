f_prime <- function(x) {
  3*x^2 -1
}

f_prime <- function(x) {
  4*x^3 - 1
}
newton_raphson <- function(f, f_prime, x0,
                           tol = 1e-6,
                           max_iter = 100) {
  
  hasil <- data.frame(
    iterasi = integer(),
    x = numeric(),
    f_x = numeric(),
    error = numeric()
  )
  
  x <- x0
  
  for (i in 1:max_iter) {
    
    fx <- f(x)
    dfx <- f_prime(x)
    
    if (abs(dfx) < 1e-12) {
      stop("Turunan terlalu dekat dengan nol.")
    }
    
    x_new <- x - fx / dfx
    error <- abs(x_new - x)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        x = x_new,
        f_x = f(x_new),
        error = error
      )
    )
    
    if (error < tol) {
      x <- x_new
      break
    }
    
    x <- x_new
  }
  
  list(
    akar = x,
    iterasi = i,
    tabel = hasil
  )
}

hasil_newton <- newton_raphson(
  f = f,
  f_prime = f_prime,
  x0 = 1.5,
  tol = 1e-6
)

hasil_newton$akar


hasil_newton$tabel


perbandingan <- data.frame(
  metode = c("Biseksi", "Newton-Raphson"),
  iterasi = c(
    hasil_biseksi$iterasi,
    hasil_newton$akar
  ),
  akar = c(
    hasil_biseksi$akar,
    hasil_newton$akar
  )
)

perbandingan




for (x0 in c(0.5, 1, 1.5, 2, 3)) {
  
  hasil <- newton_raphson(
    f = f,
    f_prime = f_prime,
    x0 = x0
  )
  
  cat(
    "x0 =",  x0,
    "| akar =", hasil$akar,
    "\ iterasi =", hasil$iterasi,
    "\n"
  )
}







f_belajar <- function(x) {
  55 + 5*x - 0.1*x^2 - 80
}

curve(
  f_belajar(x),
  from = 0,
  to = 20,
  xlab = "Jam Belajar untuk Target Nilai 80"
)

abline(h = 0, Ity = 2)

f_belajar(0)

f_belajar(20)


hasil_belajar_biseksi <- biseksi(
  f = f_belajar,
  a = 0,
  b = 20,
  tol = 1e-6
)

hasil_belajar_biseksi$akar




