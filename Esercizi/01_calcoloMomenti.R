setwd("Your directory")
data <- read.table("settembre/2018-09-10.dat", sep = " ", skip = 0)

momento_calc <-function(x, ordine, centrato){
  if(ordine == 0)1
  
  else if(ordine == 1 & centrato == FALSE) mean(x)
  
  else if(ordine == 1 & centrato == TRUE) 0
  
  else{
    n <- length(x)
    sum <- 0
    
    if(centrato)
      x <- x - mean(x)
    
    for(i in 1:n)
      sum <- sum + x[i]^ordine
    
    sum*(1/n)
  }
}


relazione_momenti <- function(x, n, centrato){
  sum <- 0
  val_medio <- mean(x)
  
  if(centrato)
    for(k in 0:n)
      sum <- sum + choose(n, k) * momento_calc(x, k, FALSE) * ((-1) * val_medio)^(n-k)
  else
    for(k in 0:n)
      sum <- sum + choose(n, k) * momento_calc(x, k, TRUE) * val_medio^(n-k)
  
  sum
}


momento_calc(data[,6], 1, FALSE)
relazione_momenti(data[,6], 1, FALSE)

momento_calc(data[,6], 1, TRUE)
relazione_momenti(data[,6], 1, TRUE)

momento_calc(data[,6], 2, FALSE)
relazione_momenti(data[,6], 2, FALSE)

momento_calc(data[,6], 2, TRUE)
relazione_momenti(data[,6], 2, TRUE)

print(mean(data[, paste("V", 6, sep = "")]))

print(var(data[, paste("V", 6, sep = "")]))



