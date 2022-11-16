require(ggplot2)
require(stringr)
setwd("Your directory")

read_data <- function(n){
  if(n < 10)
    read.table(paste("settembre/2018-09-0", n, ".dat", sep=""), sep = "", skip=0)
  else
    read.table(paste("settembre/2018-09-", n, ".dat", sep=""), sep = "", skip=0)
}

pdf_plot <- function(dati, s){
  df <-  data.frame(velocità = dati)
  ggplot(df, aes(x = velocità)) + ggtitle(s) + geom_density(alpha = 0.2, size = 1.25, color = 'red', fill = 'red')
}

fluttuazioni_giorno <- function(i){
  dati <- read_data(i)
  media <- mean(dati[,7])
  
  fluttuazioni <- c()
  
  for(i in seq_along(dati[,7])){
    fluttuazioni[i] <- dati[,7][i] - media
  }
  fluttuazioni
}

fluttuazioni_mese <- function(){
  dati_mese <- c()
  fluttuazioni <- c()
  for(i in 1:30){
    dati <- read_data(i)
    dati_mese <- append(dati_mese, dati[,7])
  }
  
  media <- mean(dati_mese)
  
  for(i in seq_along(dati_mese)){
    fluttuazioni[i] <- dati_mese[i] - media
  }
  fluttuazioni
}

orario_valido <- function(tempo) {
  str_detect(tempo, "^(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$")
}

orario_nel_mezzo <- function(x, inizio, fine, inverso) {
  a <- as.numeric(str_remove(inizio, ":"))
  b <- as.numeric(str_remove(x, ":"))
  c <- as.numeric(str_remove(fine, ":"))
  
  if(inverso) 
    (a <= b & b <= 2359) | (0 <= b & b <= fine)
  
  else
    a <= b & b <= c
}

flutt_giorno_in_range <- function(i, inizio, fine, inverso){
  if(orario_valido(inizio) & orario_valido(fine)){
    dati <- read_data(i)
    fluttuazioni <- c()
    
    for(i in seq_along(dati[,7])){
      if(orario_nel_mezzo(dati[,2][i], inizio, fine, inverso)){
        fluttuazioni <- append(fluttuazioni, dati[,7][i])
      }
    }
    
    media <- mean(fluttuazioni)
    
    for(i in seq_along(fluttuazioni)){
      fluttuazioni[i] <- fluttuazioni[i] - media
    }
    fluttuazioni
  }
}

pdf_plot(fluttuazioni_giorno(5), "fluttuazioni velocità giorno 5")
pdf_plot(fluttuazioni_giorno(20), "fluttuazioni velocità giorno 20")

pdf_plot(fluttuazioni_mese(), "Fluttuazioni mese")

pdf_plot(flutt_giorno_in_range(5, "06:00", "20:00", FALSE), "fluttuazioni velocità, giorno 5 dalle 06:00 alle 20:00")
pdf_plot(flutt_giorno_in_range(5, "20:00", "06:00", TRUE), "fluttuazioni velocità, giorno 5 dalle 20:00 alle 06:00")





