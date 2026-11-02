lagrange <- function(x_data, y_data, x) {
  
  n <- length(x_data)
  result <- 0
  
  for (i in 1:n) {
    
    L <- 1
    
    for (j in 1:n) {
      
      if (j != i) {
        L <- L *
          (x - x_data[j]) /
          (x_data[i] - x_data[j])
      }
    }
    
    result <- result + y_data[i] * L
  }
  
  return(result)
}


x_data <- c(1, 2, 4)
y_data <- c(2, 3, 5)

lagrange(
  x_data,
  y_data,
  x = 3
)
