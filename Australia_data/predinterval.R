
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
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=2))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.var")
)
for(i in seq(NCOL(ally)))
{
  # OLS forecasts
  fit.OLS <- OLSmodel(ally[,i],12,12,h, nolag = c(1,12))
  fc[,i,"OLS"] <- fit.OLS[[1]]
  fc[,i,"OLS.var"] <- (fit.OLS[[2]])^2
}

fc.OLS.base <- as.data.frame(fc[,,"OLS"])
fc.OLS.var <- as.data.frame(fc[,,"OLS.var"])
colnames(fc.OLS.base) <- colnames(ally)
colnames(fc.OLS.var) <- colnames(ally)


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
  Sigma.mat <- diag(fc.OLS.var[i,]) + (diag(fc.OLS.var[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

rec.var <- reshape2::melt(result.var)
write.csv(rec.var, "rec.var.aus.rolling.csv")
write.csv(reshape2::melt(fc.OLS.var), "unrec.var.aus.rolling.csv")


### Fixed origin
TourismData1 <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],
          start = 1998, frequency = 12)


ausgts <- gts(TourismData1, characters = list(c(1, 1, 1), 3),
                     gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
austrain <- window(ausgts, end=c(2014,12))
austest  <- window(ausgts, start=c(2015,1))

ally <- aggts(austrain)

# Set up array for forecasts
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=2))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS","OLS.var")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc.se(ally[,i], h=h, maxlag = 12, nolag = c(1,12))
  fc[,i,"OLS"] <- fit.OLS[[1]]
  fc[,i,"OLS.var"] <- fit.OLS[[2]]
}

fc.OLS.base <- as.data.frame(fc[,,"OLS"])
fc.OLS.var <- as.data.frame(fc[,,"OLS.var"])
colnames(fc.OLS.base) <- colnames(ally)
colnames(fc.OLS.var) <- colnames(ally)


k<-24
n1<-nrow(TourismData1)
Xmat<-list()
freq <- 12
nolag <- c(1,12)
## function for computing predictors (trend, dummy seasonality, lags) for each series
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend <- seq(NROW(X))
  season <- forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag <- quantmod::Lag(X, k= nolag)
  X_mat <- cbind.data.frame(intercept, trend, season, Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}

TourismData <- head(TourismData1, (n1-k))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = k, ncol = 555)
base.var <- c()

## for loop for computing forecasts error variances
for(i in 1:k){
  if(length(base.var) == 0)
      TourismData <- TourismData
    else
      TourismData[nrow(TourismData),] <- tail(as.vector(base.var),304)
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
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.var[i,]) + (diag(fc.OLS.var[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}
rec.var <- reshape2::melt(result.var)
unrec.var <- reshape2::melt(fc.OLS.var)

write.csv(rec.var, 'rec.var.csv')
write.csv(unrec.var, 'unrec.var.csv')


## computing reconceliation matrix
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))


