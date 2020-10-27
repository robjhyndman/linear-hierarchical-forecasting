
library(hts)
library(Matrix)
library(reshape)
library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)


actual.sim <- read.csv('sim.608.melt.csv', header = TRUE)
## 8 levels
actual.sim$cat <- paste0(actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 10 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 12 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$level2
                          , actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 18 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$level2
                          , actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose, actual.sim$level3)

actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 608) %>%
  as.data.frame() %>%
  ts(frequency = 12)
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names

## 8 levels
sim.05gts <- gts(actual.05, characters = list(c(1, 1, 1), 4),
                 gnames = c("State", "Zone", "Region", "Purpose", 
                            "State x Purpose", "Zone x Purpose"))

## 10 levels
sim.05gts <- gts(actual.05, characters = list(c(1, 1, 1, 1), 4),
                 gnames = c("level1", "State", "Zone", "Region", "Purpose", 
                            "level1 x Purpose", "State x Purpose", "Zone x Purpose"))
## 10 levels
sim.05gts <- gts(actual.05, characters = list(c(1, 1, 1, 1, 1), 4),
                 gnames = c("level1", "level2", "State", "Zone", "Region", "Purpose", 
                            "level1 x Purpose", "level2 x Purpose", "State x Purpose", "Zone x Purpose"))

## 18 levels
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

#######

k<-24
n<-nrow(actual.05)
train.05 <-window(sim.05gts,start = c(1, 1),end = c(1, (n-48)))
test.05 <-window(sim.05gts,start = c(1, ((n-48)+1)),end = c(1, n-(48-k)))
ally <- aggts(train.05)
ally.test<-aggts(test.05)
ally.test.05 <- as.data.frame(reshape2::melt(ally.test)$value)

h <- NROW(test.05$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc(ally[,i], h = h, maxlag = 12, nolag = c(1,12))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}

fc.OLS.608.12 <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr.608.12 <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr.608.12 <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se.608.12 <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale.608.12 <- as.data.frame(fc[,,"OLS.residual.scale"])

as.matrix <- Matrix::as.matrix
t <- Matrix::t
solve <- Matrix::solve
diag <- Matrix::diag

## computing reconceliation matrix
gmat <- GmatrixG(sim.05gts$groups)
smatrix <- as((SmatrixM(gmat)), 'dgCMatrix')
lambda <- as(diag(rowSums(smatrix)), 'dgCMatrix')

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.608.12 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.608.12)){
  f.1 <- matrix(as.numeric(fc.OLS.608.12[i,]), ncol = 1, nrow = ncol(fc.OLS.608.12))
fc.rec.608.12 [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec.608.12 ) <- colnames(ally)
## PI variance
fc.OLS.PI.608.12 <- (fc.OLS.se.608.12)^2 + (fc.OLS.residual.scale.608.12)^2

n1<-nrow(actual.05)
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

actual.05.new <- head(actual.05, (n1-k))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = k, ncol = ncol(ally))
base.var <- c()

## for loop for computing forecasts error variances
for(i in 1:k){
  if(length(base.var) == 0)
    actual.05.new  <- actual.05.new 
  else
    actual.05.new [nrow(actual.05.new ),] <- tail(as.vector(base.var),608)
  actual.05.new  <- ts(rbind(actual.05.new , actual.05[((n1-k)+i),]), start = 1, frequency = 12)
  ## 8 levels
  ausgts <- gts(actual.05.new, characters = list(c(1, 1, 1), 4),
                gnames = c("State", "Zone", "Region", "Purpose",
                           "State x Purpose", "Zone x Purpose"))
  ## 10 levels
  # ausgts <- gts(actual.05.new, characters = list(c(1, 1, 1, 1), 4),
  #               gnames = c("level1", "State", "Zone", "Region", "Purpose",
  #                          "level1 x Purpose", "State x Purpose", "Zone x Purpose"))
  ## 12 levels
  # ausgts <- gts(actual.05.new, characters = list(c(1, 1, 1, 1, 1), 4),
  #               gnames = c("level1", "level2", "State", "Zone", "Region", "Purpose",
  #                          "level1 x Purpose", "level2 x Purpose", "State x Purpose", "Zone x Purpose"))
  # 18 levels
  #ausgts <- gts(actual.05,  groups = grouping_gts)
  n <- nrow(actual.05.new)
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
  Sigma.mat <- Matrix::diag(fc.OLS.PI.608.12[i,]) + (Matrix::diag(fc.OLS.PI.608.12[i,]) %*% H.matrix)
  Sigma.mat <- as(Sigma.mat, 'dgCMatrix')
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  rec.p <- as(rec.p, 'dgCMatrix')
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.608.12)
OLS.rec = reshape2::melt(fc.rec.608.12 ) 
Actual =  ally.test.05$`reshape2::melt(ally.test)$value`  
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.608.12) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.608.12) 
date = rep(1:k, ncol(ally))


fc.OLS.608.12 <- cbind(OLS.unrec$value,
                  OLS.rec$value, 
                  Actual,  
                  OLS.var.rec$value, 
                  OLS.lower.unrec,
                  OLS.upr.unrec$value, 
                  date)
colnames(fc.OLS.608.12) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 'date')


