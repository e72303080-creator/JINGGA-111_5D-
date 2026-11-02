biseksi <- function(f, a, b, tol = 1e-6, max_inter = 100) {
  
  if (f(a) * f(b) > 0) {
    stop("Tidak ada perubahan tanda pada interval [a,b].")
  }
  
  hasil <- data.frame(
    iterasi = integer(),
    a = numeric(),
    b = numeric(),
    c = numeric(),
    f_c = numeric(),
    error = numeric()
  )
  
  for (i in 1:max_iter) {
    
    c <- (a + b) / 2
    fc <- f(c)
    error <- abs(b - a)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        a = a,
        b = b,
        c = c,
        f_c = fc,
        error = error
      )
    )
    
    if (abs(fc) < tol || error < tol) {
      break
    }
    if (f(a) * fc < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  
  list(
    akar = c,
    iterasi = i,
    tabel = hasil
  )
}


hasil_biseksi <- biseksi(
  f = f,
  a = 1,
  b = 2,
  tol = 1e-6
)

hasil_biseksi$akar

hasil_biseksi$tabel


hasil <- hasil_biseksi$tabel

plot(
  hasil$iterasi,
  hasil$c,
  type = "b",
  xlab = "Iterasi",
  ylab = "Estimasi akar",
  main = "konvergensi Metode Biseksi"
)

abline(h = hasil_biseksi$akar, Ity =2)
