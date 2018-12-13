library(tidyverse)
library(hts)
source("olsfc.R")

# Reading data and adding separate category columns
aus <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],
  start = 1998, frequency = 12)
ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose",
                         "State x Purpose", "Zone x Purpose"))

# Splitting data into training and validation sets
austrain <- window(ausgts, end=c(2014,12))
austest  <- window(ausgts, start=c(2015,1))

easter.info<-as.data.frame(easter(aus,easter.mon = TRUE))
easter.info.train<-easter.info[(1:204),]
easter.info.test<- easter.info[(205:228),]
# Construct matrix of all time series including aggregates
ally <- aggts(austrain)

# Set up array for forecasts
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=8))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("ETS","ETSlog","OLSX","ARIMA","ARIMAlog","ARIMAX","OLS","OLSlog")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  # ETS forecasts
  fc[,i,"ETS"] <- pmax(forecast(ets(ally[,i]), h=h)$mean,0)
  # ETS forecasts using logs
  fc[,i,"ETSlog"] <- pmax(exp(forecast(ets(log(ally[,i]+1)), h=h)$mean)-1,0)
  # ARIMA forecasts
  fc[,i,"ARIMA"] <- pmax(forecast(auto.arima(ally[,i]), h=h)$mean,0)
  # ARIMA forecasts using logs
  fc[,i,"ARIMAlog"] <- pmax(exp(forecast(auto.arima(log(ally[,i]+1)), h=h)$mean)-1,0)
  # ARIMAX forecasts 
  fc[,i,"ARIMAX"] <- pmax(forecast(auto.arima(ally[,i],xreg = easter.info.train), xreg=easter.info.test, h=h)$mean,0)
  # OLS forecasts
  fc[,i,"OLS"] <- pmax(olsfc(ally[,i], h=h),0)
  # OLS forecasts using logs
  fc[,i,"OLSlog"] <- pmax(exp(olsfc(log(ally[,i]+1), h=h)-1),0)
  # OLSX forecasts
  fc[,i,"OLSX"] <- pmax(olsfc.external(ally[,i], externaldata=easter.info, h=h),0)
}

## Set up array for errors (bottom level only)
errors <- array(NA, c(Horizon=h, Series=NCOL(aus), Method=dim(fc)[3], Reconciled=2))
dimnames(errors) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(aus),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled","Unreconciled")
)

# Compute errors for unreconciled forecasts
nbts <- NCOL(aus)
nseries <- NCOL(ally)
for(i in seq(dim(errors)[3]))
  errors[,,i,"Unreconciled"] <- austest$bts - fc[,nseries - nbts + seq(nbts),i]

# Compute errors for reconciled forecasts
for(i in seq(dim(errors)[3]))
{
  revisedfc <- combinef(fc[,,i], groups=austrain$groups)$bts
  tsp(revisedfc) <- tsp(austest$bts)
  errors[,,i,"Reconciled"] <- austest$bts - revisedfc
}

# Compute RMSE across bottom-level series
rmse <- sqrt(apply(errors^2, c(3,4), mean))
rmse
#ETS reconciled is best
# Does reconciliation help?
rmse[,"Unreconciled"] - rmse[,"Reconciled"]
# Better for all but OLS

