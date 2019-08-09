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


TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
########OLS-rec-matrix
k<-24
n<-nrow(TourismData)
train_tourist <-window(ausgts,start = c(1, 1),end = c(1, (n-k)))
validation_tourist <-window(ausgts,start = c(1, ((n-k)+1)),end = c(1, n))
ally <- aggts(ausgts)
gmat<-GmatrixG(ausgts$groups)
smatrix<-SmatrixM(gmat)
rec.adj<-as.matrix(smatrix%*%solve(((t(smatrix))%*%smatrix))%*%t(smatrix))

Xmat<-list()
freq <-12
maxlag <- 12
ally.final <- as.list(ally) 
Xmatrix<-function(X){
  X<-as.vector(X)
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k=1:maxlag)
  X_mat<-cbind.data.frame(trend,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}


Xmat.final <- lapply(ally.final, Xmatrix)


result.fore <- list()
for(i in 1:k){
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - k) + (i - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - k) + i,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  ally <- as.data.frame(ally)
  ally.train <- ally[1:((n - k) + (i - 1)),]
  y.final <- ally.train[-c(1:maxlag),] 
  y.final <- melt(y.final)
  y.final <- as.matrix(y.final$value)
  coeff <- (mat.inverse %*%t(mat))%*%y.final
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  base.fore <- mat.test%*%coeff
  base.fore[base.fore<0] <- 0
  result <- rec.adj %*% base.fore
  fore <- as.data.frame(as.matrix(result)) 
  #result.fore[i,] <- fore
  result.fore[[length(result.fore)+1]] <- fore
}

final.result <- t(do.call(cbind, result.fore))
final.result <- melt(final.result)
write.csv(final.result,"test_matrix_rolling.csv")


########OLS-unrec-matrix
TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
########OLS-unrec-matrix
k<-24
n<-nrow(TourismData)
train_tourist <-window(ausgts,start = c(1, 1),end = c(1, (n-k)))
validation_tourist <-window(ausgts,start = c(1, ((n-k)+1)),end = c(1, n))
ally <- aggts(ausgts)

Xmat<-list()
freq <-12
maxlag <- 12
ally.final <- as.list(ally) 
Xmatrix<-function(X){
  X<-as.vector(X)
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k=1:maxlag)
  X_mat<-cbind.data.frame(trend,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}


Xmat.final <- lapply(ally.final, Xmatrix)


result.fore <- list()
for(i in 1:k){
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - k) + (i - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - k) + i,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  ally <- as.data.frame(ally)
  ally.train <- ally[1:((n - k) + (i - 1)),]
  y.final <- ally.train[-c(1:maxlag),] 
  y.final <- melt(y.final)
  y.final <- as.matrix(y.final$value)
  coeff <- (mat.inverse %*%t(mat))%*%y.final
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  base.fore <- mat.test%*%coeff
  base.fore[base.fore<0] <- 0
  result <- base.fore
  #result <- rec.adj %*% base.fore
  fore <- as.data.frame(as.matrix(result)) 
  result.fore[[length(result.fore)+1]] <- fore
}

final.result <- t(do.call(cbind, result.fore))
final.result <- melt(final.result)
write.csv(final.result,"test_matrix_rolling_unrec.csv")

