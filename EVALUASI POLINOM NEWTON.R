newton <- function(x_data, y_data, x) {
  
  n <- length(x_data)
  
  dd <- divided_difference(
    x_data,
    y_data
  )
  
  result <- dd[1, 1]
  product <- 1
  
  if (n >= 2) {
    
    for (j in 2:n) {
      
      product <- product *
        (x - x_data[j-1])
      
      result <- result +
        dd[1, j] * product
    }
  }
  
  return(result)
}

newton(
  x_data = c(1, 2, 4),
  y_data = c(2, 3, 5),
  x = 3
)