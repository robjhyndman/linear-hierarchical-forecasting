
library(hts)
source("olsfc.R")
library(Matrix)
library(reshape)
library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)

actual.sim <- read.csv('actual.sim.noise.FH.csv', header = TRUE)
## Choosing series with different added noises (0.01, 0.1, 0.5, 1)
actual.sim.001 <- actual.sim[actual.sim$Sim == 'sig0.01',]
actual.sim.01 <- actual.sim[actual.sim$Sim == 'sig0.1',]
actual.sim.05 <- actual.sim[actual.sim$Sim == 'sig0.5',]
actual.sim.1 <- actual.sim[actual.sim$Sim == 'sig1',]

actual.1 <- actual.sim.1$value %>%
  matrix(nrow = 228, ncol = 304) %>%
  as.data.frame() %>%
  ts(frequency = 12)
colnames(actual.1) <- unique(actual.sim.1$Var2)


actual.1gts <- gts(actual.1, characters = list(c(1, 1, 1), 3),
                     gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))

## Change k based on the desired forecast horiaon (12, 24, 36, 48)
k<-48
n<-nrow(actual.1)
train.1 <-window(actual.1gts,start = c(1, 1),end = c(1, (n-48)))
test.1 <-window(actual.1gts,start = c(1, ((n-48)+1)),end = c(1, n-(48-k)))
ally <- aggts(train.1 )
ally.test<-aggts(test.1)
ally.test.1 <- as.data.frame(reshape2::melt(ally.test)$value)
h <- NROW(test.1$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)

for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc(ally[,i], h = h, maxlag = 12, nolag = c(1,12))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}

fc.OLS.1 <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr.1 <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr.1 <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se.1 <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale.1 <- as.data.frame(fc[,,"OLS.residual.scale"])

## PI variance
fc.OLS.PI.1 <- (fc.OLS.se.1)^2 + (fc.OLS.residual.scale.1)^2

as.matrix <- Matrix::as.matrix
t <- Matrix::t
solve <- Matrix::solve
diag <- Matrix::diag

## computing reconceliation matrix
gmat <- GmatrixG(actual.1gts$groups)
smatrix <- as((SmatrixM(gmat)), 'dgCMatrix')
lambda <- as(diag(rowSums(smatrix)), 'dgCMatrix')

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.1 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.1)){
  f.1 <- matrix(as.numeric(fc.OLS.1[i,]), ncol = 1, nrow = ncol(fc.OLS.1))
  fc.rec.1 [i,] <- rec.adj.lambda %*% f.1
}

colnames(fc.rec.1) <- colnames(ally)


k<-48
n1<-nrow(actual.1)
Xmat<-list()
freq <- 12
nolag <- c(1,12)
## function for computing predictors (trend, dummy seasonality, lags) for each series
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend <- seq(NROW(X))
  season <- forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag <- quantmod::Lag(X, k = nolag)
  X_mat <- cbind.data.frame(intercept, trend, season, Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}

actual.1.new <- head(actual.1, (n1-k))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = k, ncol = 555)
base.var <- c()

## for loop for computing forecasts error variances
for(i in 1:k){
  if(length(base.var) == 0)
    actual.1.new  <- actual.1.new 
  else
    actual.1.new [nrow(actual.1.new ),] <- tail(as.vector(base.var),304)
  actual.1.new  <- ts(rbind(actual.1.new , actual.1[((n1-k)+i),]), start = 1, frequency = 12)
  ausgts <- gts(actual.1.new , characters = list(c(1, 1, 1), 3),
                gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
  n <- nrow(actual.1.new)
  ally <- aggts(ausgts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- Matrix::bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- Matrix::solve(t(mat)%*%mat)
  mat.inverse <- as(mat.inverse, 'dgCMatrix')
  mat.test <- Matrix::bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  mat.test <- as(mat.test, 'dgCMatrix')
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  H.matrix <- as(H.matrix, 'dgCMatrix')
  Sigma.mat <- Matrix::diag(fc.OLS.PI.1[i,]) + (Matrix::diag(fc.OLS.PI.1[i,]) %*% H.matrix)
  Sigma.mat <- as(Sigma.mat, 'dgCMatrix')
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  rec.p <- as(rec.p, 'dgCMatrix')
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.1)
OLS.rec = reshape2::melt(fc.rec.1) 
Actual =  ally.test.1$`reshape2::melt(ally.test)$value`  
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.1) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.1) 
date = rep(1:k, 555)


fc.OLS.1 <- cbind(OLS.unrec$value,
                    OLS.rec$value, 
                    Actual,  
                    OLS.var.rec$value, 
                    OLS.lower.unrec,
                    OLS.upr.unrec$value, 
                   date)
colnames(fc.OLS.1) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 'date')




