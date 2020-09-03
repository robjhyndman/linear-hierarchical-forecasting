
library(hts)
library(Matrix)
library(reshape)
library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)

actual.sim <- read.csv('sim.actual.2.melt.csv', header = TRUE)
actual.sim$cat <- paste0(  actual.sim$level1,  actual.sim$level2, actual.sim$State, 
                         actual.sim$Zone, actual.sim$Region, actual.sim$Purpose,  actual.sim$level3)
actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 608) %>%
  as.data.frame() %>%
  ts(frequency = 12)
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names
sim.05gts <- gts(actual.05, characters = list(c( 1, 1, 1), 4),
                 gnames = c( "State", "Zone", "Region", "Purpose", 
                            "State x Purpose", "Zone x Purpose"))

##################### Adding grouping factor (G1) - 18 levels
name_length <- str_length(names)
grouping_gts <- rbind(
  #level1
  str_sub(names, start = name_length - 10, end = name_length - 10),
  #level2
  str_sub(names, start = name_length - 10, end = name_length - 9),
  #State
  str_sub(names, start = name_length - 10, end = name_length - 8),
  #Zone
  str_sub(names, start = name_length - 10, end = name_length - 7),
  #Region
  str_sub(names, start = name_length - 10, end = name_length - 6),
  #Purpose
  str_sub(names, start = name_length - 5, end = name_length - 2),
  #level3
  str_sub(names, start = name_length - 1, end = name_length),
  #level1 x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 10), 
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #level2 x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 9),
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #State x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 8), 
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #Zone x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 7),
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #level3 x Purpose
  paste(str_sub(names, start = name_length - 5, end = name_length - 2),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #level1 x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 10),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #level2 x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 9),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #State x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 8),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #Zone x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 7),
        str_sub(names, start = name_length - 1, end = name_length), sep = "")
)
sim.05gts <- gts(actual.05,  groups = grouping_gts)

#############################

k<-24
n<-nrow(actual.05)
train.05 <- window(sim.05gts,start = c(1, 1),end = c(1, (n-48)))
test.05 <- window(sim.05gts,start = c(1, ((n-48)+1)),end = c(1, n-(48-k)))
a.05gts <- window(sim.05gts,start = c(1, 1),end = c(1, n-(48-k)))
ally <- aggts(a.05gts)
ally.test<-aggts(test.05)
ally.test.05 <- as.data.frame(reshape2::melt(ally.test)$value)
h <- NROW(test.05$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)

for(i in seq(NCOL(ally)))
{
  fit.OLS <-  OLSmodel(ally[,i],12,12,24, nolag = c(1,12))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}

fc.OLS.05 <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr.05 <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr.05 <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se.05 <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale.05 <- as.data.frame(fc[,,"OLS.residual.scale"])

## PI variance
fc.OLS.PI.05 <- (fc.OLS.se.05)^2 + (fc.OLS.residual.scale.05)^2

## computing reconceliation matrix
gmat <- GmatrixG(sim.05gts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.05 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.05)){
  f.05 <- matrix(as.numeric(fc.OLS.05[i,]), ncol = 1, nrow = ncol(fc.OLS.05))
  fc.rec.05 [i,] <- rec.adj.lambda %*% f.05
}

colnames(fc.rec.05) <- colnames(ally)


k<-24
n1<-nrow(ally)
Xmat<-list()
freq <- 12
nolag <- c(1,12)
## function for computing predictors (trend, dummy seasonality, lags) for each series
maxlag <- 12
ally.final <- as.list(aggts(a.05gts)) 
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X, k = nolag)
  X_mat<-cbind.data.frame(intercept,trend,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}

Xmat.final <- lapply(ally.final, Xmatrix)

## empty matrix for the forecasts
result.var<- matrix(NA, nrow = k, ncol = NCOL(ally))
base.var <- c()
## for loops for computing rolling base forecasts
for(i in 1:k){
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n1 - k) + (i - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n1 - k) + i,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.PI.05[i,]) + (diag(fc.OLS.PI.05[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.05)
OLS.rec = reshape2::melt(fc.rec.05) 
Actual =  ally.test.05$`reshape2::melt(ally.test)$value`  
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.05) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.05) 
#date = rep(1:k, 555)


fc.OLS.05 <- cbind(OLS.unrec$value,
                  OLS.rec$value, 
                  Actual,  
                  OLS.var.rec$value, 
                  OLS.lower.unrec,
                  OLS.upr.unrec$value)
colnames(fc.OLS.05) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec')
write.csv(fc.OLS.05, "fc.OLS.PI.rolling.18.608.csv")






