library(tidyverse)
library(hts)


# Reading data and adding separate category columns
aus <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],
          start = 1998, frequency = 12)
ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose",
                         "State x Purpose", "Zone x Purpose"))

# Splitting data into training and validation sets
austrain <- window(ausgts, end=c(2014,12))
austest  <- window(ausgts, start=c(2015,1))

# Construct matrix of all time series including aggregates
ally <- aggts(austrain)

# Set up array for forecasts - fixed origin
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=1))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("ols")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  # OLS forecasts
  fc[,i,"ols"] <- olsfc(ally[,i],h = h, maxlag = 12, nolag = c(1,12))
  
}
fc <- as.data.frame(fc)
colnames(fc) <- colnames(ally)
allytest <- as.data.frame(aggts(austest))
res <- allytest - fc  
fc <- as.matrix(fc)
res <- as.matrix(res)

#### Reconciling forecasts
#min_trace (shrink)
n <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n
tar <- diag(apply(res, 2, compose(crossprod, stats::na.omit))/n)
corm <- cov2cor(covm)
xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
xs <- xs[stats::complete.cases(xs),]
v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
diag(v) <- 0
corapn <- cov2cor(tar)
d <- (corm - corapn)^2
lambda <- sum(v)/sum(d)
lambda <- max(min(lambda, 1), 0)
W <- lambda * tar + (1 - lambda) * covm
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
R <- t(smatrix)%*%solve(W)
P <- solve(R%*%smatrix)%*%R
SP <- smatrix%*%P

#fc_rec <- as.matrix(SP%*%t(fc))
## wls_var

W <- diag(diag(covm))
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
R <- t(smatrix)%*%solve(W)
P <- solve(R%*%smatrix)%*%R
SP <- smatrix%*%P




fr <- matrix(NA, nrow = 24, ncol = ncol(ally))

for(i in 1:nrow(fc)){
  f.1 <- matrix(fc[i,], ncol = 1, nrow = ncol(fc))
  fr [i,] <- SP %*% f.1
}
colnames(fr) <- colnames(ally)

###### rolling

TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
########hierarchy+ARIMA-hierarchy+ets
k<-24
n<-nrow(TourismData)
train_tourist <-window(ausgts,start = c(1, 1),end = c(1, (n-k)))
validation_tourist <-window(ausgts,start = c(1, ((n-k)+1)),end = c(1, n))
ally <- aggts(ausgts)


# Set up array for forecasts - rolling origin
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=1))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS")
)
for(i in seq(NCOL(ally)))
{
  # OLS forecasts
  fc[,i,"OLS"] <- OLSmodel(ally[,i],12,12,24, nolag = c(1,12))
}
fc <- as.data.frame(fc)
colnames(fc) <- colnames(ally)
allytest <- as.data.frame(aggts(austest))
res <- allytest - fc  
fc <- as.matrix(fc)
res <- as.matrix(res)


#### Reconciling forecasts
#min_trace (shrink)
n <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n
tar <- diag(apply(res, 2, compose(crossprod, stats::na.omit))/n)
corm <- cov2cor(covm)
xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
xs <- xs[stats::complete.cases(xs),]
v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
diag(v) <- 0
corapn <- cov2cor(tar)
d <- (corm - corapn)^2
lambda <- sum(v)/sum(d)
lambda <- max(min(lambda, 1), 0)
W <- lambda * tar + (1 - lambda) * covm
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
R <- t(smatrix)%*%solve(W)
P <- solve(R%*%smatrix)%*%R
SP <- smatrix%*%P

#fc_rec <- as.matrix(SP%*%t(fc))
## wls_var

W <- diag(diag(covm))
gmat <- GmatrixG(ausgts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
R <- t(smatrix)%*%solve(W)
P <- solve(R%*%smatrix)%*%R
SP <- smatrix%*%P




fr <- matrix(NA, nrow = 24, ncol = ncol(ally))

for(i in 1:nrow(fc)){
  f.1 <- matrix(fc[i,], ncol = 1, nrow = ncol(fc))
  fr [i,] <- SP %*% f.1
}
colnames(fr) <- colnames(ally)




