require(stringr)

setwd("C:/Users/ricca/OneDrive/Desktop/UNI Magistrale/Corsi/1 anno/1Semestre/Computational Statistics/EserciziEsameDef")
data <- read.table("settembre/2018-09-10.dat", sep = " ", skip = 0)


read_data <- function(n){
  if(n < 10)
    read.table(paste("settembre/2018-09-0", n, ".dat", sep=""), sep = "", skip=0)
  else
    read.table(paste("settembre/2018-09-", n, ".dat", sep=""), sep = "", skip=0)
}

calcolo_deviaz_standard <- function(x){
  media = mean(x)
  somma <-0
  n <- length(x)
  for(i in 1:n){
    somma <- somma + ((x[i] - media)^2)
  }
  
  sqrt((somma)/n)
}

funz_standardizzazione <- function(x){
  dati_standard <- c()
  ds <- calcolo_deviaz_standard(x)
  media <- mean(x)
  
  for(i in seq_along(x)){
    val_stndrd <- (x[i] - media)/ds
    dati_standard <- append(dati_standard, val_stndrd)
  }
  dati_standard
}

pdf <- function(dati, s){
  plot(density(dati), main=s, lwd=2, col='red', xlab="velocità")
}

fluttuaz_giorno <- function(i){
  dati <- read_data(i)
  media <- mean(dati[,7])
  
  fluttuazioni <- c()
  
  for (i in seq_along(dati[,7])) {
    fluttuazioni[i] <- dati[,7][i] - media
  }
  
  fluttuazioni
}

fluttuaz_mese <- function(){
  dati_mese <- c()
  
  for(i in 1:30){
    dati <- read_data(i)
    dati_mese <- append(dati_mese, dati[,7])
  }
  
  media <- mean(dati_mese)
  fluttuazioni <- c()
  
  for(i in seq_along(dati_mese)){
    fluttuazioni[i] <- dati_mese[i] - media
  }
  
  fluttuazioni
}


orario_valido <- function(temp){
  str_detect(temp, "^(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$")
}

orario_mezzo <- function(x, inizio, fine, inverso){
  a <- as.numeric(str_remove(inizio, ":"))
  b <- as.numeric(str_remove(x, ":"))
  c <- as.numeric(str_remove(fine, ":"))
  
  if(inverso) 
    (a <= b & b <= 2359) | (0 <= b & b <= fine)
  else
    a <= b & b <= c
}

fluttuazioni_giorno_range <- function(i, inizio, fine, inverso){
  if(orario_valido(inizio) & orario_valido(fine)){
    dati <- read_data(i)
    fluttuazioni <- c()
    
    for(i in seq_along(dati[,7])){
      if(orario_mezzo(dati[,2][i], inizio, fine, inverso)){
        fluttuazioni <- append(fluttuazioni, dati[,7][i])
      }
    }
    
    media <- mean(fluttuazioni)
    
    for (i in seq_along(fluttuazioni)) {
      fluttuazioni[i] <- fluttuazioni[i] - media
    }
    
    fluttuazioni
  }
}


fluttuazioni_mese_range <- function(inizio, fine, inverso) {
  if(orario_valido(inizio) & orario_valido(fine)) {
    dati_mese <- c()
    
    for(i in 1:30) {
      dati <- leggi_dati_giorno(i)
      
      for(i in seq_along(dati[,7]))
        if(orario_nel_mezzo(dati[,2][i], inizio, fine, inverso))
          dati_mese <- append(dati_mese, dati[,7][i])
    }
    
    med <- mean(dati_mese)
    
    fluttuazioni <- c()
    
    for(i in seq_along(dati_mese))
      fluttuazioni[i] <- dati_mese[i] - med
    
    fluttuazioni
  }
}

intervalli <- function(x, ampiezza){
  max <- max(x)
  min <- min(x)
  
  n <- ceiling((max-min)/ampiezza)
  
  intervalli <- vector(length = n)
  
  for (i in 1:n) {
    for (j in seq_along(x)) {
      inf <- min + (ampiezza * (i-1))
      sup <- min + (ampiezza * i)
      
      if(x[j] >= inf & x[j]<= sup){
        intervalli[i] <- intervalli[i] + 1
      }
    }
  }
  intervalli
}


chi_quadro <- function(x, ampiezza){
  somma <- 0
  min <- min(x)
  n <- length(x)
  vect_intervalli <- intervalli(x, ampiezza)
  
  for(i in seq_along(vect_intervalli)){
    pk <- (pnorm(min + ampiezza*i) - pnorm(min+ampiezza*(i-1)))
    e <- n * pk
    somma <- somma + ((vect_intervalli[i] - e)^2)/e
  }
  
  print(paste("chi-quadro: ", somma))
  print(paste("intervalli: ", length(vect_intervalli)))
}




dati_giorno_8 <- funz_standardizzazione(fluttuaz_giorno(25))
pdf(dati_giorno_8, "PDF giorno 8")
chi_quadro(dati_giorno_8, 1.2)

dati_giorno_8_giorno <- funz_standardizzazione(fluttuazioni_giorno_range(8, "06:00", "20:00", FALSE))
pdf(dati_giorno_8_giorno, "PDF giorno 8 diurni")
chi_quadro(dati_giorno_8_giorno, 1.2)

dati_giorno_8_notte <- funz_standardizzazione(fluttuazioni_giorno_range(8, "20:00", "06:00", TRUE))
pdf(dati_giorno_8_notte, "PDF giorno 8 notturni")
chi_quadro(dati_giorno_8_notte, 0.8)

dati_giorno_15 <- funz_standardizzazione(fluttuaz_giorno(15))
pdf(dati_giorno_15, "PDF giorno 20")
chi_quadro(dati_giorno_15, 1.2)

dati_mese <- funz_standardizzazione(fluttuaz_mese())
pdf(dati_mese, "PDF mese")
chi_quadro(dati_mese, 1.5)