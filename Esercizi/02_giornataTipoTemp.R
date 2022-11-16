setwd("C:/Users/ricca/OneDrive/Desktop/UNI Magistrale/Corsi/1 anno/1Semestre/Computational Statistics/EserciziEsameDef")
temperatura_media <- vector(length = 1428)
minutes_in_a_day <- 1:1428

day <- function(i){
  f <- "settembre/2018-09-"
  if(i < 10) f <- paste0(f, "0", i, ".dat")
  else f <- paste0(f, i, ".dat")
  f
}

for(i in 1:30){
  data <- nullfile()
  data <- read.table(day(i))
  tmp <- data[,6]
  for(x in minutes_in_a_day){
    temperatura_media[x] <- temperatura_media[x] + tmp[x]
  }
}

for(k in minutes_in_a_day){
  temperatura_media[k] = temperatura_media[k] / 30
}

plot(minutes_in_a_day, temperatura_media)