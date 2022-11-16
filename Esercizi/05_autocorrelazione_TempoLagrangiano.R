require(pracma)

setwd("Your directory")

read_data <- function(n){
  if(n < 10)
    read.table(paste("settembre/2018-09-0", n, ".dat", sep=""), sep = "", skip=0)
  else
    read.table(paste("settembre/2018-09-", n, ".dat", sep=""), sep = "", skip=0)
}

fluttuazioni_vettore_giorno <- function(i){
  dati <- read_data(i)
  media <- mean(dati[,7])
  fluttuazioni <- c()
  for(i in seq_along(dati[,7])){
    fluttuazioni[i] <- dati[,7][i] - media
  }
  fluttuazioni
}

autocorrelazione <- function(x, dt){
  n <- length(x)
  numeratore <- c()
  for(t in 1:n){
    if((t+dt) <= n){
      numeratore[t] = x[t] * x[t +dt]
    }
  }
  mean(numeratore)/var(x)
}

tempo_lagrangiano <- function(x){
  vect_autocorrelazione <- c()
  n <- length(x) -1
  
  for(dt in 0: n){
    vect_autocorrelazione[dt] = autocorrelazione(x, dt)
  }
  
  -1/(line(log(abs(vect_autocorrelazione)))$coefficients[1])
}


esercizio_su_giorno <- function(i, s){
  fluttuazioni <- fluttuazioni_vettore_giorno(i)
  r <- c()
  
  for(dt in 0:32)
    r[dt + 1] = autocorrelazione(fluttuazioni, dt)
  
  
  a <- acf(fluttuazioni, plot = FALSE, type = c("correlation"))
  plot(a, main=s)
  lines(0:31, array(data = r, dim = 32), type = "l", col = "red", lwd=2)
  
  Lt = tempo_lagrangiano(fluttuazioni)
  print(paste("Tempo lagrangiano: ", Lt))
  print(paste("acf: ", mean(a$acf[,,1])))
}

esercizio_su_giorno(5, "ACF fluttuazioni velocità giorno 5")
esercizio_su_giorno(20, "ACF fluttuazioni velocità giorno 20")