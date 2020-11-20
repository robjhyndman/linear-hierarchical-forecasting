
library(forecast)

aus <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)

###########################################NoiseFH
set.seed(123)
sim <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  sim[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE) 
}

sim.scale <- apply(sim,2,function(x) ((x-mean(x))/sd(x)))
sim.mean <- as.vector(apply(sim, 2, function(x) mean(x)))
sim.sd <- as.vector(apply(sim, 2, function(x) sd(x)))

set.seed(123)
noise001 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  noise001[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.01)
}

set.seed(123)
noise01 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  noise01[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.1)
}

set.seed(123)
noise05 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  noise05[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
}

set.seed(123)
noise1 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  noise1[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 1)
}

sim001.scale <- sim.scale + noise001
sim001 <- sweep(sim001.scale, 2, sim.sd, FUN="*")
sim001 <- sweep(sim001, 2, sim.mean, FUN="+")
colnames(sim001) <- colnames(aus)

sim01.scale <- sim.scale + noise01
sim01 <- sweep(sim01.scale, 2, sim.sd, FUN="*")
sim01 <- sweep(sim01, 2, sim.mean, FUN="+")
colnames(sim01) <- colnames(aus)

sim05.scale <- sim.scale + noise05
sim05 <- sweep(sim05.scale, 2, sim.sd, FUN="*")
sim05 <- sweep(sim05, 2, sim.mean, FUN="+")
colnames(sim05) <- colnames(aus)

sim1.scale <- sim.scale + noise1
sim1 <- sweep(sim1.scale, 2, sim.sd, FUN="*")
sim1 <- sweep(sim1, 2, sim.mean, FUN="+")
colnames(sim1) <- colnames(aus)

actual.sim.noise.FH <- rbind(cbind(reshape2::melt(sim001), Sim ='sig0.01'), cbind(reshape2::melt(sim01), Sim = 'sig0.1'), 
                             cbind(reshape2::melt(sim05), Sim = 'sig0.5'), cbind(reshape2::melt(sim001), Sim = 'sig1'))

write_csv(actual.sim.noise.FH, 'actual.sim.noise.FH.csv')

############################################ levelNS
sim1 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim2 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim3 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim4 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim5 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim6 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim7 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim8 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
sim9 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))

for(i in 1:ncol(aus)){
  set.seed(1)
  sim1[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(2)
  sim2[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(3)
  sim3[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(4)
  sim4[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(5)
  sim5[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(6)
  sim6[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(7)
  sim7[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(8)
  sim8[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
  set.seed(9)
  sim9[,i] <- simulate(auto.arima(aus[,i]), nsim=228, future = FALSE)
}

sim.scale1 <- apply(sim1,2,function(x) ((x-mean(x))/sd(x)))
sim.mean1 <- as.vector(apply(sim1, 2, function(x) mean(x)))
sim.sd1 <- as.vector(apply(sim1, 2, function(x) sd(x)))

sim.scale2 <- apply(sim2,2,function(x) ((x-mean(x))/sd(x)))
sim.mean2 <- as.vector(apply(sim2, 2, function(x) mean(x)))
sim.sd2 <- as.vector(apply(sim2, 2, function(x) sd(x)))

sim.scale3 <- apply(sim3,2,function(x) ((x-mean(x))/sd(x)))
sim.mean3 <- as.vector(apply(sim3, 2, function(x) mean(x)))
sim.sd3 <- as.vector(apply(sim3, 2, function(x) sd(x)))

sim.scale4 <- apply(sim4,2,function(x) ((x-mean(x))/sd(x)))
sim.mean4 <- as.vector(apply(sim4, 2, function(x) mean(x)))
sim.sd4 <- as.vector(apply(sim4, 2, function(x) sd(x)))

sim.scale5 <- apply(sim5,2,function(x) ((x-mean(x))/sd(x)))
sim.mean5 <- as.vector(apply(sim5, 2, function(x) mean(x)))
sim.sd5 <- as.vector(apply(sim5, 2, function(x) sd(x)))

sim.scale6 <- apply(sim6,2,function(x) ((x-mean(x))/sd(x)))
sim.mean6 <- as.vector(apply(sim6, 2, function(x) mean(x)))
sim.sd6 <- as.vector(apply(sim6, 2, function(x) sd(x)))

sim.scale7 <- apply(sim7,2,function(x) ((x-mean(x))/sd(x)))
sim.mean7 <- as.vector(apply(sim7, 2, function(x) mean(x)))
sim.sd7 <- as.vector(apply(sim7, 2, function(x) sd(x)))

sim.scale8 <- apply(sim8,2,function(x) ((x-mean(x))/sd(x)))
sim.mean8 <- as.vector(apply(sim8, 2, function(x) mean(x)))
sim.sd8 <- as.vector(apply(sim8, 2, function(x) sd(x)))

sim.scale9 <- apply(sim9,2,function(x) ((x-mean(x))/sd(x)))
sim.mean9 <- as.vector(apply(sim9, 2, function(x) mean(x)))
sim.sd9 <- as.vector(apply(sim9, 2, function(x) sd(x)))


noise1 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise2 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise3 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise4 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise5 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise6 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise7 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise8 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
noise9 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))


for(i in 1:ncol(aus)){
  set.seed(1)
  noise1[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(2)
  noise2[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(3)
  noise3[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(4)
  noise4[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(5)
  noise5[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(6)
  noise6[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(7)
  noise7[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(8)
  noise8[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  set.seed(9)
  noise9[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.5)
  
}

 sim1.scale <- sim.scale1 + noise1
 sim1 <- sweep(sim1.scale, 2, sim.sd1, FUN="*")
 sim1 <- sweep(sim1, 2, sim.mean1, FUN="+")

 sim2.scale <- sim.scale2 + noise2
 sim2 <- sweep(sim2.scale, 2, sim.sd2, FUN="*")
 sim2 <- sweep(sim2, 2, sim.mean2, FUN="+")

 sim3.scale <- sim.scale3 + noise3
 sim3 <- sweep(sim3.scale, 2, sim.sd3, FUN="*")
 sim3 <- sweep(sim3, 2, sim.mean3, FUN="+")

 sim4.scale <- sim.scale4 + noise4
 sim4 <- sweep(sim4.scale, 2, sim.sd4, FUN="*")
 sim4 <- sweep(sim4, 2, sim.mean4, FUN="+")

sim5.scale <- sim.scale5 + noise5
sim5 <- sweep(sim5.scale, 2, sim.sd5, FUN="*")
sim5 <- sweep(sim5, 2, sim.mean5, FUN="+")

sim6.scale <- sim.scale6 + noise6
sim6 <- sweep(sim6.scale, 2, sim.sd6, FUN="*")
sim6 <- sweep(sim6, 2, sim.mean6, FUN="+")

sim7.scale <- sim.scale7 + noise7
sim7 <- sweep(sim7.scale, 2, sim.sd7, FUN="*")
sim7 <- sweep(sim7, 2, sim.mean7, FUN="+")

sim8.scale <- sim.scale8 + noise8
sim8 <- sweep(sim8.scale, 2, sim.sd8, FUN="*")
sim8 <- sweep(sim8, 2, sim.mean8, FUN="+")

sim9.scale <- sim.scale9 + noise9
sim9 <- sweep(sim9.scale, 2, sim.sd9, FUN="*")
sim9 <- sweep(sim9, 2, sim.mean9, FUN="+")

######
sim.304 <- sim05
colnames(sim.304) <- c(1:304)
sim.608 <- cbind.data.frame(sim05, sim1)
colnames(sim.608) <- c(1:608)
sim.1520 <- cbind.data.frame(sim05, sim1, sim2, sim3, sim4)
colnames(sim.1520) <- c(1:1520)
sim.3040 <- cbind.data.frame(sim05, sim1, sim2, sim3, sim4, sim5, sim6, sim7, sim8, sim9)
colnames(sim.3040) <- c(1:3040)

######

sim.304 <- reshape2::melt(sim.304) %>%
  mutate(
    level1 = c(rep('A', 140*228), rep('B', 96*228), rep('C', 68*228)),
    level2 = c(rep('A', 140*228), rep('B', 48*228), rep('C', 48*228), rep('D', 40*228), rep('E', 28*228)), 
    State = rep(stringr::str_sub(colnames(aus), 1, 1), each = 228),
    Zone = rep(stringr::str_sub(colnames(aus), 2, 2), each = 228),
    Region = rep(stringr::str_sub(colnames(aus), 3, 3), each = 228),
    Purpose = rep(stringr::str_sub(colnames(aus), 4, 6), each = 228),
    level3 = c(rep('G1', 56*228), rep('G2', 84*228), rep('G3', 96*228), rep('G4', 40*228), rep('G5', 28*228))
  )

write.csv(sim.304, 'sim.304.csv')

sim.608 <- reshape2::melt(sim.608) %>%
  mutate(
    level1 = c(rep(c(rep('A', 140*228), rep('B', 96*228), rep('C', 68*228)), 2)),
    level2 = c(rep(c(rep('A', 140*228), rep('B', 48*228), rep('C', 48*228), rep('D', 40*228), rep('E', 28*228)), 2)), 
    State = c(rep(c(rep(stringr::str_sub(colnames(aus), 1, 1), each = 228)), 2)),
    Zone = c(rep(c(rep(stringr::str_sub(colnames(aus), 2, 2), each = 228)), 2)),
    Region = c(rep(c(rep(stringr::str_sub(colnames(aus), 3, 3), each = 228)), 2)),
    Purpose = c(rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '1', sep = ''), each = 228), 
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '2', sep = ''), each = 228)),
    level3 = c(rep(c(rep('G1', 56*228), rep('G2', 84*228), rep('G3', 96*228), rep('G4', 40*228), rep('G5', 28*228)), 2))
  )

write.csv(sim.608, 'sim.608.csv')


sim.1520 <- reshape2::melt(sim.1520) %>%
  mutate(
    level1 = c(rep(c(rep('A', 140*228), rep('B', 96*228), rep('C', 68*228)), 5)),
    level2 = c(rep(c(rep('A', 140*228), rep('B', 48*228), rep('C', 48*228), rep('D', 40*228), rep('E', 28*228)), 5)), 
    State = c(rep(c(rep(stringr::str_sub(colnames(aus), 1, 1), each = 228)), 5)),
    Zone = c(rep(c(rep(stringr::str_sub(colnames(aus), 2, 2), each = 228)), 5)),
    Region = c(rep(c(rep(stringr::str_sub(colnames(aus), 3, 3), each = 228)), 5)),
    Purpose = c(rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '1', sep = ''), each = 228), 
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '2', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '3', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '4', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '5', sep = ''), each = 228)),
    level3 = c(rep(c(rep('G1', 56*228), rep('G2', 84*228), rep('G3', 96*228), rep('G4', 40*228), rep('G5', 28*228)), 5))
  )

write.csv(sim.1520, 'sim.1520.csv')

sim.3040 <- reshape2::melt(sim.3040) %>%
  mutate(
    level1 = c(rep(c(rep('A', 140*228), rep('B', 96*228), rep('C', 68*228)), 10)),
    level2 = c(rep(c(rep('A', 140*228), rep('B', 48*228), rep('C', 48*228), rep('D', 40*228), rep('E', 28*228)), 10)), 
    State = c(rep(c(rep(stringr::str_sub(colnames(aus), 1, 1), each = 228)), 10)),
    Zone = c(rep(c(rep(stringr::str_sub(colnames(aus), 2, 2), each = 228)), 10)),
    Region = c(rep(c(rep(stringr::str_sub(colnames(aus), 3, 3), each = 228)), 10)),
    Purpose = c(rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '0', sep = ''), each = 228), 
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '1', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '2', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '3', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '4', sep = ''), each = 228), 
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '5', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '6', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '7', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '8', sep = ''), each = 228),
                rep(paste0(c(stringr::str_sub(colnames(aus), 4, 6)), '9', sep = ''), each = 228)),
    level3 = c(rep(c(rep('G1', 56*228), rep('G2', 84*228), rep('G3', 96*228), rep('G4', 40*228), rep('G5', 28*228)), 10))
  )

write.csv(sim.3040, 'sim.3040.csv')
