
library(hts)
library(Matrix)
library(reshape)
library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)


actual.sim <- read.csv('sim.actual.10.melt.csv', header = TRUE)
actual.sim$cat <- paste0(actual.sim$level1, actual.sim$level2, actual.sim$State, 
                         actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)
actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 3040) %>%
  as.data.frame() %>%
  ts(frequency = 12)
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names
sim.05gts <- gts(actual.05, characters = list(c(1, 1, 1, 1), 4),
                 gnames = c("level1", "State", "Zone", "Region", "Purpose", 
                            "level1 x Purpose", "State x Purpose", "Zone x Purpose"))

##################### Adding grouping factor (G1) - 18 levels
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

#############################

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
start.time <- Sys.time()
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
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

fc.OLS.3040.12 <- as.data.frame(fc[,,"OLS"])
fc.OLS.lwr.3040.12 <- as.data.frame(fc[,,"OLS.lwr"])
fc.OLS.upr.3040.12 <- as.data.frame(fc[,,"OLS.upr"])
fc.OLS.se.3040.12 <- as.data.frame(fc[,,"OLS.se"])
fc.OLS.residual.scale.3040.12 <- as.data.frame(fc[,,"OLS.residual.scale"])

## computing reconceliation matrix
gmat <- GmatrixG(sim.05gts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

fc.rec.3040.12 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.3040.12)){
  f.1 <- matrix(as.numeric(fc.OLS.3040.12[i,]), ncol = 1, nrow = ncol(fc.OLS.3040.12))
fc.rec.3040.12 [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec.3040.12 ) <- colnames(ally)
## PI variance
fc.OLS.PI.3040.12 <- (fc.OLS.se.3040.12)^2 + (fc.OLS.residual.scale.3040.12)^2

k<-24
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
    actual.05.new [nrow(actual.05.new ),] <- tail(as.vector(base.var),3040)
  actual.05.new  <- ts(rbind(actual.05.new , actual.05[((n1-k)+i),]), start = 1, frequency = 12)
  # 8, 10 and 12 levels
  ausgts <- gts(actual.05.new, characters = list(c(1, 1, 1, 1, 1), 4),
                gnames = c("level1", "level2", "State", "Zone", "Region", "Purpose",
                           "level1 x Purpose", "level2 x Purpose", "State x Purpose", "Zone x Purpose"))
  # 18 levels
  #ausgts <- gts(actual.05,  groups = grouping_gts)
  n <- nrow(actual.05.new)
  ally <- aggts(ausgts)
  Xmat.final <- lapply(as.list(ally), Xmatrix)
  Xmat.final.train <- lapply(Xmat.final, function(x)x[1:((n - 1) + (1 - 1)),])
  Xmat.final.test <- lapply(Xmat.final, function(x)x[(n - 1) + 1,])
  mat <- bdiag(lapply(Xmat.final.train, function(x){as.matrix(na.omit(x))}))
  mat.inverse <- solve(t(mat)%*%mat)
  mat.test <- bdiag(lapply(Xmat.final.test, function(x){as.matrix(na.omit(x))}))
  H.matrix <- mat.test %*% mat.inverse %*% t(mat.test)
  Sigma.mat <- diag(fc.OLS.PI.3040.12[i,]) + (diag(fc.OLS.PI.3040.12[i,]) %*% H.matrix)
  rec.p <- as.matrix(solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))
  var.for <- as.matrix((smatrix %*% rec.p) %*% Sigma.mat %*% (t(rec.p) %*% t(smatrix)))
  result.var[i,] <- as.vector(diag(var.for))
}

OLS.unrec = reshape2::melt(fc.OLS.3040.12)
OLS.rec = reshape2::melt(fc.rec.3040.12 ) 
Actual =  ally.test.05$`reshape2::melt(ally.test)$value`  
OLS.var.rec = reshape2::melt(result.var) 
OLS.lower.unrec = reshape2::melt(fc.OLS.lwr.3040.12) 
OLS.upr.unrec = reshape2::melt(fc.OLS.upr.3040.12) 
date = rep(1:k, ncol(ally))


fc.OLS.3040.12 <- cbind(OLS.unrec$value,
                  OLS.rec$value, 
                  Actual,  
                  OLS.var.rec$value, 
                  OLS.lower.unrec,
                  OLS.upr.unrec$value, 
                  date)
colnames(fc.OLS.3040.12) <- c('OLS.unrec', 'OLS.rec', 'Actual', 'OLS.var.rec', 'Series', 'OLS.lower.unrec', 'OLS.upper.unrec', 'date')
#write.csv(fc.OLS.3040.12, "fc.OLS.3040.12.PI.csv")

##################### ETS and ARIMA
actual.sim <- read.csv('sim.actual.12.melt.csv', header = TRUE)
actual.sim$cat <- paste0( actual.sim$State, 
                         actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 3040) %>%
  as.data.frame()
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names

actual.05 <-  tibble(actual.05)
actual.05$Date <-  rep(yearmonth("1998 Jan") + 0:227)


actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    level1 = stringr::str_sub(group, 1, 1),
    level2 = stringr::str_sub(group, 1, 2),
    State = stringr::str_sub(group, 1, 3),
    Zone = stringr::str_sub(group, 1, 4),
    Region = stringr::str_sub(group, 1, 5),
    Purpose = stringr::str_sub(group, 6, 9),
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(level1, level2, State, Zone, Region, Purpose))


actual.05gts <- actual.05 %>%
  aggregate_key(Purpose * (level1/ level2/ State/ Zone/ Region), value = sum(value)) 


fc.ets <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(ets = ETS(value)) %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "2 years")

fc.ets <- fc.ets %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")

fc.arima <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(arima = ARIMA(value)) %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "2 years")
fc.arima <- fc.arima %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")

# fc.ets.data <- cbind.data.frame('Level1' = fc.ets$level1, 'Level2' = fc.ets$level2,
#                                 'Purpose' = fc.ets$Purpose, 'State' = fc.ets$State, 
#                                 'Zone' = fc.ets$Zone, 'Region' = fc.ets$Region,  
#                                 'Method' =  fc.ets$.model, 'date' = fc.ets$Date, 'value' = fc.ets$.mean, 
#                                 'in.lower' = fc.ets$`95%_lower`,'in.upper' = fc.ets$`95%_upper`)
# 
# write.csv(fc.ets.data, 'fc.ets.fable.3040.12.csv')
# fc.arima.data <- cbind.data.frame('Level1' = fc.arima$level1, 'Level2' = fc.arima$level2,
#                                   'Purpose' = fc.arima$Purpose, 'State' = fc.arima$State, 
#                                   'Zone' = fc.arima$Zone, 'Region' = fc.arima$Region,  
#                                   'Method' =  fc.arima$.model, 'date' = fc.arima$Date, 'value' = fc.arima$.mean, 
#                                   'in.lower' = fc.arima$`95%_lower`,'in.upper' = fc.arima$`95%_upper`)
# 
# write.csv(fc.arima.data, 'fc.arima.fable.3040.12.csv')

