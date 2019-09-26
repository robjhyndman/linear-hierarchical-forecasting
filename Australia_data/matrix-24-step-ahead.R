library(tidyverse)
library(hts)
source("olsfc.R")
library(Matrix)
library(reshape)

########OLS-rec-matrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to integer
    gmat <- t(apply(xmat, 1, function(x) as.integer(factor(x, unique(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  nc.xmat <- ncol(xmat)
  gmat <- rbind(
    if (all(gmat[1,] == rep(1L, nc.xmat))) NULL else rep(1L, nc.xmat),
    gmat,
    if (all(gmat[NROW(gmat),] == seq(1L, nc.xmat))) NULL else seq(1L, nc.xmat)
  )
  #gmat <- gmat[!duplicated(gmat), , drop = FALSE] # Remove possible duplicated... make smarter above.
  return(structure(gmat, class = "gmatrix"))
}

SmatrixM <- function(gmat) { 
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- sparseMatrix(i = ia, j = ja, x = ra)
  })
  sparse <- do.call("rbind", sparse.S)
  return(sparse)
}


TourismData1 <-read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)]
k<-24
n<-nrow(TourismData1)


Xmat<-list()
freq <-12
maxlag <- 4

## function for computing predictors (trend, dummy seasonality, lags) for each series
Xmatrix<-function(X){
  X<-as.vector(X)
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k=1:maxlag)
  X_mat<-cbind.data.frame(trend,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}


#### OLS - rec forecasts

TourismData <- head(TourismData1, (n-k))
## empty matrix for the forecasts
result.fore <- matrix(NA, nrow = k, ncol = NCOL(ally))
base.fore <- c()

for(i in 1:k){
  if(length(base.fore) == 0)
    TourismData <- TourismData
  else
    TourismData[nrow(TourismData),] <- tail(as.vector(t(base.fore)),304)
  TourismData <- rbind.data.frame(TourismData, rep(0,304))
  TourismData2 <- ts(TourismData, start = 1, frequency = 12)
  ausgts <- gts(TourismData2, characters = list(c(1, 1, 1), 3),
                gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
  n1 <- nrow(TourismData2)
  train_tourist <-window(ausgts, start = c(1,1), end = c(1, (n1-1)))
  validation_tourist <-window(ausgts, start = c(1, ((n1-1)+1)), end = c(1, ((n1-1)+1)))
  ally <- aggts(ausgts)
  ally.final <- as.list(ally)
  
  ## computing reconceliation matrix
  gmat<-GmatrixG(ausgts$groups)
  smatrix<-SmatrixM(gmat)
  rec.adj<-as.matrix(smatrix%*%solve(((t(smatrix))%*%smatrix))%*%t(smatrix))
  Xmat.final <- lapply(ally.final, Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n1 - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n1 - 1) + 1,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  ally.train <- as.data.frame(ally)
  ally.train <- ally[1:(n1 - 1),]
  y.final <- as.matrix(melt(ally.train[-c(1:maxlag),])$value)
  coeff <- (mat.inverse %*%t(mat))%*%y.final
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  base.fore <- as.matrix(mat.test%*%coeff)
  base.fore[base.fore<0] <- 0
  result <- rec.adj %*% base.fore
  fore <- as.vector(as.matrix(result)) 
  result.fore [i,] <- fore
}

final.result <- melt(result.fore)
write.csv(final.result,"matrix_fixed_rec.csv")

## for computing unreconciled forecasts comment out line 99
