
library(hts)
source("olsfc.R")
library(Matrix)
library(reshape)
### Rolling origin

TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
k<-24
n<-nrow(TourismData)
train_tourist <-window(ausgts,start = c(1, 1),end = c(1, (n-k)))
validation_tourist <-window(ausgts,start = c(1, ((n-k)+1)),end = c(1, n))
ally <- aggts(ausgts)

h <- 24
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)

for(i in seq(NCOL(ally)))
{
  fit.OLS <- OLSmodel(ally[,i],12,12,h, nolag = c(1,12))
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

## computing reconceliation matrix
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.1 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.1)){
  f.1 <- matrix(as.numeric(fc.OLS.1[i,]), ncol = 1, nrow = ncol(fc.OLS.1))
  fc.rec.1 [i,] <- rec.adj.lambda %*% f.1
}

colnames(fc.rec.1) <- colnames(ally)


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

result.var <- matrix(NA, nrow = k, ncol = NCOL(ally))
## for loops for computing rolling base forecasts
for(i in 1:k){
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - k) + (i - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - k) + i,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.PI.1[i,]) + (diag(fc.OLS.PI.1[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.1)
OLS.rec = reshape2::melt(fc.rec.1) 
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.1) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.1) 
date = rep(1:k, 555)

fc.OLS.1 <- cbind(OLS.unrec$value,
                  OLS.rec$value, 
                  OLS.var.rec$value, 
                  OLS.lower.unrec,
                  OLS.upr.unrec$value, 
                  date)
colnames(fc.OLS.1) <- c('OLS.unrec', 'OLS.rec',  'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 'date')
write.csv(fc.OLS.1, "rolling.OLS.PI.csv")


### Fixed origin
aus <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],
          start = 1998, frequency = 12)


ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
                     gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
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

## computing reconceliation matrix
library(Matrix)
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.1 <- matrix(NA, nrow = 24, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.1)){
  f.1 <- matrix(as.numeric(fc.OLS.1[i,]), ncol = 1, nrow = ncol(fc.OLS.1))
  fc.rec.1 [i,] <- rec.adj.lambda %*% f.1
}

colnames(fc.rec.1) <- colnames(ally)


k<-24
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

actual.1.new <- head(aus, (n1-k))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = k, ncol = 555)
base.var <- c()

## for loop for computing forecasts error variances
for(i in 1:k){
  if(length(base.var) == 0)
    actual.1.new  <- actual.1.new 
  else
    actual.1.new [nrow(actual.1.new ),] <- tail(as.vector(base.var),304)
  actual.1.new  <- ts(rbind(actual.1.new , aus[((n1-k)+i),]), start = 1, frequency = 12)
  ausgts <- gts(actual.1.new , characters = list(c(1, 1, 1), 3),
                gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
  n <- nrow(actual.1.new)
  ally <- aggts(ausgts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.PI.1[i,]) + (diag(fc.OLS.PI.1[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.1)
OLS.rec = reshape2::melt(fc.rec.1) 
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.1) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.1) 
date = rep(1:k, 555)

fc.OLS.1 <- cbind(OLS.unrec$value,
                  OLS.rec$value, 
                  OLS.var.rec$value, 
                  OLS.lower.unrec,
                  OLS.upr.unrec$value, 
                  date)
colnames(fc.OLS.1) <- c('OLS.unrec', 'OLS.rec',  'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 'date')
write.csv(fc.OLS.1, "fix.PI.csv")

