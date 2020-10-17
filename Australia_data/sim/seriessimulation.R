

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
noise05 <- matrix(NA, ncol = ncol(aus), nrow = nrow(aus))
for(i in 1:ncol(aus)){
  noise05[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =228, sd = 0.01)
}

sim05.scale <- sim.scale + noise05
sim05 <- sweep(sim05.scale, 2, sim.sd, FUN="*")
sim05 <- sweep(sim05, 2, sim.mean, FUN="+")
colnames(sim05) <- colnames(aus)

############################################ levelNS

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
 colnames(sim2) <- colnames(aus)
 
 sim3.scale <- sim.scale3 + noise3
 sim3 <- sweep(sim3.scale, 2, sim.sd3, FUN="*")
 sim3 <- sweep(sim3, 2, sim.mean3, FUN="+")
 colnames(sim3) <- colnames(aus)
 
 sim4.scale <- sim.scale4 + noise4
 sim4 <- sweep(sim4.scale, 2, sim.sd4, FUN="*")
 sim4 <- sweep(sim4, 2, sim.mean4, FUN="+")
 colnames(sim4) <- colnames(aus)

sim5.scale <- sim.scale5 + noise5
sim5 <- sweep(sim5.scale, 2, sim.sd5, FUN="*")
sim5 <- sweep(sim5, 2, sim.mean5, FUN="+")
colnames(sim5) <- colnames(aus)

sim6.scale <- sim.scale6 + noise6
sim6 <- sweep(sim6.scale, 2, sim.sd6, FUN="*")
sim6 <- sweep(sim6, 2, sim.mean6, FUN="+")
colnames(sim6) <- colnames(aus)

sim7.scale <- sim.scale7 + noise7
sim7 <- sweep(sim7.scale, 2, sim.sd7, FUN="*")
sim7 <- sweep(sim7, 2, sim.mean7, FUN="+")
colnames(sim7) <- colnames(aus)

sim8.scale <- sim.scale8 + noise8
sim8 <- sweep(sim8.scale, 2, sim.sd8, FUN="*")
sim8 <- sweep(sim8, 2, sim.mean8, FUN="+")
colnames(sim8) <- colnames(aus)

sim9.scale <- sim.scale9 + noise9
sim9 <- sweep(sim9.scale, 2, sim.sd9, FUN="*")
sim9 <- sweep(sim9, 2, sim.mean9, FUN="+")
colnames(sim9) <- colnames(aus)

######
sim. <- sim05
sim.608 <- cbind.data.frame(sim05, sim1)
sim.1520 <- cbind.data.frame(sim05, sim1, sim2, sim3, sim4)
sim.3040 <- cbind.data.frame(sim05, sim1, sim2, sim3, sim4, sim5, sim6, sim7, sim8, sim9)

