lapply( c("quantmod","forecast","dplyr","plyr","stringr","hts","data.table"),require,character.only = T)
## reading data
TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
########hierarchy+ARIMA-hierarchy+ets
k<-24
n<-nrow(TourismData)
train_tourist <-window(ausgts,start = c(1, 1),end = c(1, (n-k)))
validation_tourist <-window(ausgts,start = c(1, ((n-k)+1)),end = c(1, n))
ally <- aggts(ausgts)

##### ARIMA & ARIMAlog & ETS & ETSlog

# Set up array for forecasts 
h <- NROW(validation_tourist$bts)
fc.arima.ets <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=4))
dimnames(fc.arima.ets) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("ETS","ETSlog","ARIMA","ARIMAlog")
)
for(i in seq(NCOL(ally))){
  for(j in 1:h)
  { 
    austrain.1 <- window(ausgts, start=c(1,1),end = c(1, (n - h) + (j - 1)))
    ally.1<-aggts(austrain.1)
    # ETS forecast
    fc.arima.ets[j,i,"ETS"] <- forecast(ets(ally.1[,i]), h=1)$mean
    # ETS forecast using logs
    fc.arima.ets[j,i,"ETSlog"] <- exp(forecast(ets(log(ally.1[,i]+1)), h=1)$mean)-1
    # ARIMA forecast
    fc.arima.ets[j,i,"ARIMA"] <- forecast(auto.arima(ally.1[,i]), h=1)$mean
    # ARIMA forecast using logs
    fc.arima.ets[j,i,"ARIMAlog"] <- exp(forecast(auto.arima(log(ally.1[,i]+1)), h=1)$mean)-1
 
  }
}
### setting negative base forecasts zero  
fc.arima.ets[fc.arima.ets<0]<-0
### reconcile the results
forecast.arima.ets <- array(NA, c(Horizon=h, Series=NCOL(TourismData), Method=4,Reconciled=2))
dimnames(forecast.arima.ets) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(TourismData),
  Method = c("ETS","ETSlog","ARIMA","ARIMAlog"),
  Reconciled=c("reconciled","unreconciled")
)
nbts <- NCOL(TourismData)
nseries <- NCOL(ally)
for(i in seq(dim(fc.arima.ets)[3])){
  forecast.arima.ets [,,i,"reconciled"] <- combinef(fc.arima.ets[,,i], groups=ausgts$groups)$bts
  forecast.arima.ets [,,i,"unreconciled"] <- fc.arima.ets[, nseries - nbts + seq(nbts),i]
}

### Saving forecast results - arima and ets
write.csv(forecast.arima.ets,"forecast.arima.ets.rolling.csv")

## Set up array for errors (bottom level only)
errors.arima.ets <- array(NA, c(Horizon=h, Series=NCOL(TourismData), Method=dim(fc.arima.ets)[3], Reconciled=2))
dimnames(errors.arima.ets) <- list(
  Horizon = dimnames(fc.arima.ets)[[1]],
  Series = colnames(TourismData),
  Method = dimnames(fc.arima.ets)[[3]],
  Reconciled = c("reconciled","unreconciled")
)
# Compute errors for unreconciled forecasts
for(i in seq(dim(errors.arima.ets)[3]))
  errors.arima.ets[,,i,"unreconciled"] <- validation_tourist$bts - fc.arima.ets[ ,nseries - nbts + seq(nbts),i]
# Compute errors for reconciled forecasts
for(i in seq(dim(errors.arima.ets)[3]))
{
  revisedfc <- combinef(fc.arima.ets[,,i], groups=train_tourist$groups)$bts
  tsp(revisedfc) <- tsp(validation_tourist$bts)
  errors.arima.ets[,,i,"reconciled"] <- validation_tourist$bts - revisedfc
}

# Compute RMSE across bottom-level series
rmse.arima.ets <- sqrt(apply(errors.arima.ets^2, c(3,4), mean))
rmse.arima.ets
#ETS reconciled is best and reconciliation helps
rmse.arima.ets[,"Unreconciled"] - rmse.arima.ets[,"Reconciled"]
### saving error results - arima and ets
write.csv(errors.arima.ets,"errors.arima.ets.rolling.csv")

#### creating forecast by OLS and OLSlog
#### OLS rolling function
OLSmodel<-function(X,freq,maxlag,h, nolag = NULL){
  X<-as.vector(X)
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  if(maxlag>0)
  {
    Xlag <- quantmod::Lag(X,k=1:maxlag)
    if(length(nolag) == 0)
      X_mat <- cbind.data.frame(X, trend, season)
    else
      X_mat <- cbind.data.frame(X, trend, season, Xlag[,nolag])
  }
  else
    X_mat<-cbind.data.frame(X,trend,season)
  n <- nrow(X_mat)
  fore_base_OLS<-matrix(NA,nrow = h,ncol=1)
  for (i in 1:h) {
    train.1 <- X_mat[1:((n - h) + (i - 1)), ]
    valid.1 <- X_mat[(n - h) + i, ]
    fit <- lm(X ~. , data = train.1)
    fore <- predict.lm( fit , newdata = valid.1)
    fore_base_OLS[i,]<-fore
  }
  return(fore_base_OLS)
}

### computing base forecasts
fc.OLS.base <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=2))
dimnames(fc.OLS.base) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS","OLSlog")
)
for(i in seq(NCOL(ally)))
{
  # OLS forecasts
  fc.OLS.base[,i,"OLS"] <- OLSmodel(ally[,i],12,12,24, nolag = c(1,12))
  # OLS forecasts using logs
  fc.OLS.base[,i,"OLSlog"] <- exp(OLSmodel(log(ally[,i]+1),12,12,24, nolag = c(1,12)))-1
}
### setting negative base forecasts zero
fc.OLS.base[fc.OLS.base<0]<-0

#### OLS reconcile
fc.OLS.rec <- array(NA, c(Horizon=h, Series=NCOL(TourismData), Method=2,Reconciled=1))
dimnames(fc.OLS.rec) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(TourismData),
  Method = c("OLS","OLSlog"),
  Reconciled = c("reconciled")
)

for(i in seq(dim(fc.OLS.base)[3]))
{
  fc.OLS.rec[,,i,"reconciled"] <-combinef(fc.OLS.base[,,i], groups=ausgts$groups)$bts
}
### OLS unreconcile
fc.OLS.un <- array(NA, c(Horizon=h, Series=NCOL(TourismData), Method=2,Reconciled=1))
dimnames(fc.OLS.un) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(TourismData),
  Method = c("OLS","OLSlog"),
  Reconciled = c("unreconciled")
)
for(i in seq(NCOL(TourismData)))
{
  # OLS forecasts
  fc.OLS.un[,i,"OLS","unreconciled"] <- OLSmodel(TourismData[,i],12,12,24)
  # OLS forecasts using logs
  fc.OLS.un[,i,"OLSlog","unreconciled"] <- exp(OLSmodel(log(TourismData[,i]+1),12,12,24))-1
}
## combining OLS arrays
fc.OLS<-abind::abind(fc.OLS.rec,fc.OLS.un)
### saving OLS forecast results
write.csv(fc.OLS,"forecast.OLS.rolling.csv")

## Set up array for errors (bottom level only)
errors.OLS <- array(NA, c(Horizon=h, Series=NCOL(TourismData), Method=dim(fc.OLS)[3], Reconciled=2))
dimnames(errors.OLS) <- list(
  Horizon = dimnames(fc.OLS)[[1]],
  Series = colnames(TourismData),
  Method = dimnames(fc.OLS)[[3]],
  Reconciled = c("reconciled","unreconciled")
)
# Compute errors for unreconciled forecasts
for(i in seq(dim(errors.OLS)[3])){
  errors.OLS[,,i,"reconciled"] <- validation_tourist$bts - fc.OLS[,,i,"reconciled"]
  errors.OLS[,,i,"unreconciled"] <- validation_tourist$bts - fc.OLS[,,i,"unreconciled"]
}

#### saving OLS error results
write.csv(errors.OLS,"errors.OLS.rolling.csv")
# Compute OLS RMSE across bottom-level series
rmse.OLS.rolling <- sqrt(apply(errors.OLS^2,c(3,4),mean))
rmse.OLS.rolling
rmse.OLS.rolling[,"unreconciled"] - rmse.OLS.rolling[,"reconciled"]
#### OLSlog.reconciled is the best and reconceliation helps





