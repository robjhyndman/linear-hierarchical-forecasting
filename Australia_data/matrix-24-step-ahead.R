library(hts)
source("olsfc.R")
library(Matrix)
library(reshape)

TourismData1 <-read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)]
k<-24
n1<-nrow(TourismData1)


Xmat<-list()
freq <-12
nolag <- c(1,12)
maxlag <- 12

## function for computing predictors (trend, dummy seasonality, lags) for each series
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend1<-seq(NROW(X))
  trend2<-(seq(NROW(X)))^2
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k= nolag)
  X_mat<-cbind.data.frame(intercept, trend1, trend2, season, Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}



TourismData <- head(TourismData1, (n-k))
## empty matrix for the forecasts
result.fore <- matrix(NA, nrow = k, ncol = 555)
base.fore <- c()

## for loop for computing base forecasts
for(i in 1:k){
  if(length(base.fore) == 0)
    TourismData <- TourismData
  else
    TourismData[nrow(TourismData),] <- tail(as.vector(base.fore),304)
  TourismData <- ts(rbind(TourismData, TourismData1[((n1-k)+i),]), start = 1, frequency = 12)
  ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
                gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
  n <- nrow(TourismData)
  ally <- aggts(ausgts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  ally.train <- ally[1:(n - 1),]
  y.final <- as.matrix(melt(ally.train[-c(1:maxlag),])$value)
  coeff <- (mat.inverse %*%t(mat))%*%y.final
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  base.fore <- mat.test%*%coeff
  result.fore [i,] <- as.vector(base.fore) 
}
result.fore[result.fore<0] <- 0
#write.csv(melt(result.fore),"matrix_fixed_unrec.csv")


## computing reconceliation matrix
gmat<-GmatrixG(ausgts$groups)
smatrix<-SmatrixM(gmat)
wvec<- InvS4g(ausgts$groups)
lambda <- diag(wvec)
rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

## computing reconciled forecasts
fr.24 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(result.fore)){
  f.24 <- matrix(result.fore[i,], ncol = 1, nrow = ncol(result.fore))
  fr.24 [i,] <- rec.adj.lambda %*% f.24
}
#write.csv(melt(fr.24) ,"matrix_fixed_rec.csv")

