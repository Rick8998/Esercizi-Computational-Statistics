
# library(gplots)
# library(R.utils)
# library(fields)
# library(grDevices)
# library(plotrix)
setwd("C:/Users/ricca/OneDrive/Desktop/UNI Magistrale/Corsi/1 anno/1Semestre/Computational Statistics/EserciziEsameDef")


day <- function(i){
  f <- "settembre/2018-09-"
  if(i < 10) f <- paste0(f, "0", i, ".dat")
  else f <- paste0(f, i, ".dat")
  f
}

data_subset <- function(data, start, durata){
  data_vec <- vector(length = 60*durata)
  count <- 0
  for(i in ((60*start)) : (60*(start+durata))-1){
    data_vec[count] <- data[i, "V7"]
    count <- count +1
  }
  data_vec
}

mean_on_subset <- function(data){
  mean_v <- 0
  for (i in seq_along(data)) {
    mean_v <- mean_v + data[i]
  }
  mean_v <- mean_v / length(seq_along(data))
}

varianza_calc <- function(data, media){
  varianza <- 0
  for(i in seq_along(data)) {
    varianza <- varianza + ((data[i]-media)^2)
  }
  varianza <- varianza * (1/(length(seq_along(data))-1))
}


fluttuazioni <- function(data, media){
  vec_flutt <- vector(length = length(data))
  for(i in seq_along(data)){
    vec_flutt[i] <- data[i] - media
  }
  vec_flutt
}

create_matrix <- function(rows, columns, det, eps){
  particelle <- matrix(nrow = rows, ncol = columns)
  for(row in 1:rows){
    particelle[row, 1] <- 0
    for(col in 1:(columns-1)){
      white_noise <- rnorm(n = 1) * sqrt(det)
      particelle[row, col+1] <- ((1-(dt / lagrange_time)) * particelle[row, col]) + (sqrt(2 * eps) *  white_noise)
    }
  }
  particelle
}


full_data <- read.table(day(10))
data <- data_subset(full_data, 2, 20)
media <- mean_on_subset(data)
varianza_value <- varianza_calc(data, media)
fluctuations <- fluttuazioni(data, media)
correlazione <- acf(fluctuations, type = 'correlation', plot = F)$acf[,,1]
res <- line(log(correlazione))
lagrange_time <- -1/res$coefficients[2]
epsilon <- (2 * varianza_value) / (2 * lagrange_time)
dt <- 0.1 * lagrange_time
num_particelle <- 20
num_movimenti <- 1000
particelle <- create_matrix(num_particelle, num_movimenti, dt, epsilon)
media_on_matrix <- mean_on_subset(particelle)
print(media_on_matrix)
scarto_input <- sqrt(varianza_value)
scarto_matrix <- sqrt(varianza_calc(particelle, media_on_matrix))
print(scarto_input)
print(scarto_matrix)
plot(0,0, xlim = c(0, 220), ylim = c(-1,1), xlab = "tempo", ylab = "variazioni di velocità")
time_vector <- vector(length = num_movimenti)
time_vector[1] <- 0
for(i in 1:(num_movimenti-1)) {
  time_vector[i+1] <- time_vector[i] + dt
}
c <- rainbow(num_particelle)
for (i in 1 : num_particelle) {
  lines(time_vector, particelle[i,], col = c[i])
}

