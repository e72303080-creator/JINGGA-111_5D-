newraph <- function(f, df, x0, tol = 1e-6, max_iter = 100) {
  
  hasil <- data.frame()
  
  x <- x0
  
  for (i in 1:max_iter) {
    
    fx <- f(x)
    dfx <- df(x)
    
    if (dfx == 0) {
      stop("Turunan bernilai nol.")
    }
    
    x_new <- x - fx / dfx
    error <- abs(x_new - x)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        x_lama = x,
        f_x =fx,
        df_x = dfx,
        x_baru = x_new,
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
    iterasi = hasil
  )
}


f <- function(x) {
  x^3 - x - 2
}

df <- function(x) {
  3*x^2 - 1
}

hasil_newraph <- newraph(
  f = f,
  df = df,
  x0 = 1.5
)

hasil_newraph$akar

hasil_newraph$iterasi