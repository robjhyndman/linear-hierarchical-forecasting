
library(hts)
library(Matrix)
library(reshape2)
library(tidyverse) 
library(readr)
source('olsfc_se.R')
source('OLSmodel_se.R')
source('smatrix.R')

################
### Rolling origin
################
## reading data
aus <-ts(readr::read_csv("TourismData_v3.csv")[-c(1,2)],start=1,frequency =12)
## creating hierarchy structure
ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
## forecast horizon
h <- 24
n <- nrow(aus)
## train and test sets
train_tourist <- window(ausgts,start = c(1, 1),end = c(1, (n-h)))
test_tourist <- window(ausgts,start = c(1, ((n-h)+1)),end = c(1, n))
ally <- aggts(ausgts)
ally.test <- aggts(test_tourist)
## empty array for forecast results
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)
## computation time 
start_time <- Sys.time()
for(i in seq(NCOL(ally)))
{
  fit.OLS <- OLSmodel(ally[,i],12,12,h, nolag = c(1,12))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}
end_time <- Sys.time()
end_time - start_time

fc.OLS <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale <- as.data.frame(fc[,,"OLS.residual.scale"])


## PI variance
fc.OLS.PI <- (fc.OLS.se)^2 + (fc.OLS.residual.scale)^2

## making matrix computations sparse from Matrix package
as.matrix <- Matrix::as.matrix
t <- Matrix::t
solve <- Matrix::solve
diag <- Matrix::diag

## computing reconciliation matrix
gmat <- GmatrixG(ausgts$groups)
smatrix <- as((SmatrixM(gmat)), 'dgCMatrix')
lambda <- as(diag(rowSums(smatrix)), 'dgCMatrix')

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

## computing reconciled forecasts
fc.rec <- matrix(NA, nrow = h, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS)){
  f.1 <- matrix(as.numeric(fc.OLS[i,]), ncol = 1, nrow = ncol(fc.OLS))
  fc.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec) <- colnames(ally)

##################
### Computing reconciled variance for prediction intervals
##################

## computing predictors (trend, dummy seasonality, lags) for each series
Xmat<-list()
freq <-12
maxlag <- 12
ally.final <- as.list(ally) 
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k=1:maxlag)
  X_mat<-cbind.data.frame(intercept,trend,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}
Xmat.final <- lapply(ally.final, Xmatrix)

result.var <- matrix(NA, nrow = h, ncol = NCOL(ally))
## for loops for computing reconciled variance
for(i in 1:h){
  ## train and test sets matrix
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - h) + (i - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - h) + i,])
  mat <- as(Matrix::bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))})), 'dgCMatrix')
  ## (X'X)^-1
  mat.inverse <- as(Matrix::solve(t(mat)%*%mat), 'dgCMatrix')
  ## X*_t+h
  mat.test <- as(Matrix::bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))})), 'dgCMatrix')
  ## Formula 7 in the text
  H.matrix <- as(mat.test %*% mat.inverse %*% t(mat.test), 'dgCMatrix')
  Sigma.mat <-as( Matrix::diag(fc.OLS.PI[i,]) + (Matrix::diag(fc.OLS.PI[i,]) %*% H.matrix), 'dgCMatrix')
  rec.p <- as(as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda)), 'dgCMatrix')
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

## saving the forecasts, errors and prediction interval results - based and reconciled forecasts
OLS.unrec = reshape2::melt(fc.OLS)
OLS.rec = reshape2::melt(fc.rec) 
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr) 
OLS.upper.unrec = reshape2::melt(fc.OLS.upr) 
Actual =  reshape2::melt(ally.test)$value
error.rec = Actual- OLS.rec$value
error.unrec = Actual- OLS.unrec$value
OLS.upper.rec = OLS.rec$value + 1.96*sqrt(OLS.var.rec$value)
OLS.lower.rec = OLS.rec$value - 1.96*sqrt(OLS.var.rec$value)
date = rep(1:h, 555)

fc.OLS <- cbind(OLS.unrec$value,
                       OLS.rec$value, 
                       Actual,  
                       OLS.var.rec$value, 
                       OLS.lower.unrec,
                       OLS.upper.unrec$value, 
                       OLS.lower.rec,
                       OLS.upper.rec, 
                       error.rec,
                       error.unrec)
colnames(fc.OLS) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 
                             'OLS.lower.rec', 'OLS.upper.rec', 'error.rec', 'error.unrec')

write_csv(fc.OLS, 'fc.rolling.tourism.OLS.csv')

################
### Fixed origin
################
## reading data
aus <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],start = 1998, frequency = 12)
## creating hierarchy structure
ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
                     gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
## train and test sets
austrain <- window(ausgts, end=c(2014,12))
austest  <- window(ausgts, start=c(2015,1))

ally <- aggts(austrain)

# Set up array for forecasts
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)
## computation time
start_time <- Sys.time()
for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc(ally[,i], h = h, maxlag = 12, nolag = c(1,12))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}

end_time <- Sys.time()
end_time - start_time

fc.OLS <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale <- as.data.frame(fc[,,"OLS.residual.scale"])


## PI variance
fc.OLS.PI <- (fc.OLS.se)^2 + (fc.OLS.residual.scale)^2

## making matrix computations sparse from Matrix package

as.matrix <- Matrix::as.matrix
t <- Matrix::t
solve <- Matrix::solve
diag <- Matrix::diag

## computing reconciliation matrix
gmat <- GmatrixG(ausgts$groups)
smatrix <- as((SmatrixM(gmat)), 'dgCMatrix')
lambda <- as(diag(rowSums(smatrix)), 'dgCMatrix')

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
## computing reconciled forecasts
fc.rec <- matrix(NA, nrow = 24, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS)){
  f.1 <- matrix(as.numeric(fc.OLS[i,]), ncol = 1, nrow = ncol(fc.OLS))
  fc.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec) <- colnames(ally)


n1<-nrow(aus)
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

actual.new <- head(aus, (n1-h))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = h, ncol = 555)
base.var <- c()

## for loops for computing reconciled variance
for(i in 1:h){
  if(length(base.var) == 0)
    actual.new  <- actual.new 
  else
    actual.new [nrow(actual.new ),] <- tail(as.vector(base.var),304)
  actual.new  <- ts(rbind(actual.new , aus[((n1-h)+i),]), start = 1, frequency = 12)
  ausgts <- gts(actual.new , characters = list(c(1, 1, 1), 3),
                gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
  n <- nrow(actual.new)
  ally <- aggts(ausgts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  ## train and test sets matrix
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- as(Matrix::bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))})), 'dgCMatrix')
  ## formula 7 in the text
  mat.inverse <- as( Matrix::solve(t(mat)%*%mat), 'dgCMatrix')
  mat.test <- as(Matrix::bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))})), 'dgCMatrix')
  H.matrix <- as(mat.test %*% mat.inverse %*% t(mat.test), 'dgCMatrix')
  Sigma.mat <- as(Matrix::diag(fc.OLS.PI[i,]) + (Matrix::diag(fc.OLS.PI[i,]) %*% H.matrix), 'dgCMatrix')
  rec.p <- as(as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda)), 'dgCMatrix')
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

## saving the forecasts, errors and prediction interval results - based and reconciled forecasts

OLS.unrec = reshape2::melt(fc.OLS)
OLS.rec = reshape2::melt(fc.rec) 
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr) 
OLS.upper.unrec = reshape2::melt(fc.OLS.upr) 
Actual =  reshape2::melt(ally.test)$value
error.rec = Actual- OLS.rec$value
error.unrec = Actual- OLS.unrec$value
OLS.upper.rec = OLS.rec$value + 1.96*sqrt(OLS.var.rec$value)
OLS.lower.rec = OLS.rec$value - 1.96*sqrt(OLS.var.rec$value)
date = rep(1:h, 555)

fc.OLS <- cbind(OLS.unrec$value,
                OLS.rec$value, 
                Actual,  
                OLS.var.rec$value, 
                OLS.lower.unrec,
                OLS.upper.unrec$value, 
                OLS.lower.rec,
                OLS.upper.rec, 
                error.rec,
                error.unrec)
colnames(fc.OLS) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 
                      'OLS.lower.rec', 'OLS.upper.rec', 'error.rec', 'error.unrec')

write_csv(fc.OLS, 'fc.fix.tourism.OLS.csv')



