

lapply(c("quantmod", "forecast", "dplyr", "plyr", "stringr", "hts", "data.table"),
       require,
       character.only = TRUE
)
wikipedia_data <- read.csv("wikipedia_data.csv", header = TRUE)

## Data reshaping
# 394: length of each series and 913: number of series
wikipedia_wide1 <- wikipedia_data$views %>%
  matrix(nrow = 394, ncol = 913) %>%
  as.data.frame() %>%
  ts(frequency = 7)
colnames(wikipedia_wide1) <- unique(wikipedia_data$cat_column) %>% substr(1,14)

##################
## using hierarchies and groupings up to 2-way combinations
##################
wikigts <- gts(wikipedia_wide1, character=c(7,2,2,3),
               gnames = c("Access",
                          "Agent",
                          "Language",
                          "Purpose",
                          "Access x Agent",
                          "Access x Language",
                          "Access x Purpose",
                          "Agent x Language",
                          "Agent x Purpose",
                          "Language x Purpose"))

# Splitting data into training and test sets
wikitrain <- window(wikigts, end = c(1, 366))
wikitest <- window(wikigts, start = c(1, 367))

# Construct matrix of all time series including aggregates
ally <- aggts(wikitrain)
ally.test <- aggts(wikitest)
ally.test.05 <- as.data.frame(reshape2::melt(ally.test)$value)

# Set up array for forecasts
h <- NROW(wikitest$bts)

fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=5))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.lwr", "OLS.upr", "OLS.se", "OLS.residual.scale")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc(ally[,i], h = h, maxlag = 7, nolag = c(1,7))
  fc[, i,"OLS"] <- fit.OLS[[1]]
  fc[, i,"OLS.lwr"] <- fit.OLS[[2]]
  fc[, i,"OLS.upr"] <- fit.OLS[[3]]
  fc[, i,"OLS.se"] <- fit.OLS[[4]]
  fc[, i,"OLS.residual.scale"] <- fit.OLS[[5]]
}

fc.OLS <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale <- as.data.frame(fc[,,"OLS.residual.scale"])

colnames(fc.OLS.lwr) <- c(1:ncol(fc.OLS.lwr))
colnames(fc.OLS.upr) <- c(1:ncol(fc.OLS.upr))
colnames(fc.OLS) <- c(1:ncol(fc.OLS))
colnames(fc.OLS.se) <- c(1:ncol(fc.OLS.se))
colnames(fc.OLS.residual.scale) <- c(1:ncol(fc.OLS.residual.scale))

## computing reconceliation matrix
gmat <- GmatrixG(wikigts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec <- matrix(NA, nrow = 28, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS)){
  f.1 <- matrix(as.numeric(fc.OLS[i,]), ncol = 1, nrow = ncol(fc.OLS))
  fc.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec ) <- colnames(ally)
## PI variance
fc.OLS.PI <- (fc.OLS.se)^2 + (fc.OLS.residual.scale)^2


k<-28
n1<-nrow(wikipedia_wide1)
Xmat<-list()
freq <- 7
nolag <- c(1,7)
## function for computing predictors (trend, dummy seasonality, lags) for each series
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend1 <- seq(NROW(X))
  trend2 <-seq(NROW(X))^2
  season <- forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag <- quantmod::Lag(X, k= nolag)
  X_mat <- cbind.data.frame(intercept, trend1, trend2, season, Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}

wikipedia_wide <- head(wikipedia_wide1, (n1-k))
## empty matrix for the forecasts
result.var <- matrix(NA, nrow = k, ncol = 1035)
base.var <- c()

## for loop for computing forecasts error variances
for(i in 1:k){
  if(length(base.var) == 0)
    wikipedia_wide <-  wikipedia_wide
  else
    wikipedia_wide[nrow( wikipedia_wide),] <- tail(as.vector(base.var),913)
  wikipedia_wide <- ts(rbind( wikipedia_wide,  wikipedia_wide1[((n1-k)+i),]), start = 1, frequency = 7)
  wikigts <- gts(wikipedia_wide, character=c(7,2,2,3),
                 gnames = c("Access",
                            "Agent",
                            "Language",
                            "Purpose",
                            "Access x Agent",
                            "Access x Language",
                            "Access x Purpose",
                            "Agent x Language",
                            "Agent x Purpose",
                            "Language x Purpose"))
  
  n <- nrow( wikipedia_wide)
  ally <- aggts(wikigts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.PI[i,]) + (diag(fc.OLS.PI[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}
rec.var <- reshape2::melt(result.var)
write.csv(rec.var, "rec.var.csv")

fc.OLS.lwr <- reshape2::melt(fc.OLS.lwr)
fc.OLS.upr <- reshape2::melt(fc.OLS.upr)
fc.OLS <- reshape2::melt(fc.OLS)
Actual <- ally.test.05$`reshape2::melt(ally.test)$value` 
fc.unrec.pi <- cbind.data.frame('lower.OLS.unrec' = fc.OLS.lwr$value, 'upper.OLS.unrec' = fc.OLS.upr$value, 
                                'Series' = fc.OLS.lwr$variable , 'fc' = fc.OLS$value, 'Actual' = Actual)
write.csv(fc.unrec.pi, "unrec.pi.csv")





######################################################Rolling

wikipedia_data <- read.csv("wikipedia_data.csv", header = TRUE)

## Data reshaping
# 394: length of each series and 913: number of series
wikipedia_wide1 <- wikipedia_data$views %>%
  matrix(nrow = 394, ncol = 913) %>%
  as.data.frame() %>%
  ts(frequency = 7)
colnames(wikipedia_wide1) <- unique(wikipedia_data$cat_column) %>% substr(1,14)

##################
## using hierarchies and groupings up to 2-way combinations
##################
wikigts <- gts(wikipedia_wide1, character=c(7,2,2,3),
               gnames = c("Access",
                          "Agent",
                          "Language",
                          "Purpose",
                          "Access x Agent",
                          "Access x Language",
                          "Access x Purpose",
                          "Agent x Language",
                          "Agent x Purpose",
                          "Language x Purpose"))

# Splitting data into training and test sets
wikitrain <- window(wikigts, end = c(1, 366))
wikitest <- window(wikigts, start = c(1, 367))

# Construct matrix of all time series including aggregates
ally <- aggts(wikigts)
ally.test <- aggts(wikitest)


h <- 28
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=2))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.var")
)
for(i in seq(NCOL(ally)))
{
  # OLS forecasts
  fit.OLS <- OLSmodel(ally[,i],7,7,h, nolag = c(1,7))
  fc[,i,"OLS"] <- fit.OLS[[1]]
  fc[,i,"OLS.var"] <- (fit.OLS[[2]])^2
}

fc.OLS.base <- as.data.frame(fc[,,"OLS"])
fc.OLS.var <- as.data.frame(fc[,,"OLS.var"])
colnames(fc.OLS.base) <- colnames(ally)
colnames(fc.OLS.var) <- colnames(ally)


## computing predictors (trend, dummy seasonality, lags) for each series
Xmat<-list()
freq <-7
maxlag <- 7
ally.final <- as.list(ally) 
Xmatrix<-function(X){
  X<-as.vector(X)
  intercept <- rep(1, length(X))
  trend1<-seq(NROW(X))
  trend2<-seq(NROW(X))^2
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  Xlag<-quantmod::Lag(X,k=1:maxlag)
  X_mat<-cbind.data.frame(intercept,trend1, trend2,season,Xlag)
  Xmat[[length(Xmat)+1]] <- X_mat 
}
Xmat.final <- lapply(ally.final, Xmatrix)
k <- 28
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
write.csv(rec.var, "rec.var.wiki.rolling.csv")
write.csv(reshape2::melt(fc.OLS.var), "unrec.var.wiki.rolling.csv")





