### Wikipedia dataset - all the levels Mean(RMSE) 
##########################
### 28-step-ahead
##########################
lapply( c("quantmod","forecast","dplyr","plyr","stringr","hts","data.table"),require,character.only = T)
wikipedia_data <-read.csv("wikipedia_data.csv", header = TRUE)

##Data reshaping
wikipedia_wide <-
  ts(as.data.frame(matrix(wikipedia_data$views, nrow = 394, ncol = 913)),frequency = 7) #394: length of each series and 913: number of series
##############
names <- as.vector(unique(wikipedia_data$cat_column))
names(wikipedia_wide) <- c(names)
##################
## using heirachies and groupings
##################
name_length <- str_length(names)
grouping_hts <- rbind(
  #agent
  str_sub(names, start = name_length - 8, end = name_length - 7),
  #access
  str_sub(names, start = name_length - 15, end = name_length - 9),
  #language
  str_sub(names, start = name_length - 6, end = name_length - 5),
  #purpose
  str_sub(names, start = name_length - 4, end = name_length - 2)
)

wikigts <- gts(wikipedia_wide, groups = grouping_hts)
# Splitting data into training and testation sets
wikitrain<-window(wikigts,end=c(1,366))
wikitest<-window(wikigts,start=c(1,367))
# Construct matrix of all time series including aggregates
ally <- aggts(wikitrain)
ally.test<-aggts(wikitest)
# Set up array for forecasts
h <- NROW(wikitest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=3))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("ETS","ARIMA","OLS")
)
# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fc[,i,"ETS"] <- pmax(forecast(ets(ally[,i]), h=h)$mean,0)
  fc[,i,"ARIMA"] <- pmax(forecast(auto.arima(ally[,i]), h=h)$mean,0)
  fc[,i,"OLS"] <- pmax(olsfc(ally[,i], h=h,maxlag = 7),0)
}

## saving forecasts
forecast.ets.arima.OLS <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=dim(fc)[3], Reconciled=2))
dimnames(forecast.ets.arima.OLS) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(ally),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled","Unreconciled")
)
for(i in seq(dim(fc)[3])){
  forecast.ets.arima.OLS[,,i,"Unreconciled"] <- fc[,,i]
  forecast.ets.arima.OLS[,,i,"Reconciled"] <- combinef(fc[,,i], groups=wikitrain$groups,keep="all")
}

## Set up array for errors (bottom level only)
errors.level <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=dim(fc)[3], Reconciled=2))
dimnames(errors.level) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(ally),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled","Unreconciled")
)

# Compute errors for unreconciled forecasts
nbts <- NCOL(wikipedia_wide)
nseries <- NCOL(ally)

for(i in seq(dim(errors.level)[3]))
  errors.level[,,i,"Unreconciled"] <- ally.test - fc[,,i]

# Compute errors for reconciled forecasts

for(i in seq(dim(errors.level)[3]))
{
  revisedfc <- combinef(fc[,,i], groups=wikitrain$groups,keep = "all")
  colnames(revisedfc)<-names(ally.test)
  tsp(revisedfc) <- tsp(wikitest$bts)
  errors.level[,,i,"Reconciled"] <- ally.test - revisedfc
}


errors.level<-as.data.frame(errors.level)
n.base<-ncol(errors.level)/2
## reconcile
error.rec<-errors.level[1:n.base]

error.rec.level0<-as.data.frame(error.rec[,1])
sqrt(mean(melt(error.rec.level0)$value^2))

error.rec.level1<-error.rec[,2:3]
sqrt(mean(melt(error.rec.level1)$value^2))

error.rec.level2<-error.rec[,4:6]
sqrt(mean(melt(error.rec.level2)$value^2))

error.rec.level3<-error.rec[,7:10]
sqrt(mean(melt(error.rec.level3)$value^2))

error.rec.level4<-error.rec[,11:19]
sqrt(mean(melt(error.rec.level4)$value^2))

error.rec.level5<-error.rec[,20:n.base]
sqrt(mean(melt(error.rec.level5)$value^2))

## unreconcile
error.unrec<-errors.level[(n.base+1):(n.base*2)]

error.unrec.level0<-as.data.frame(error.unrec[,1])
sqrt(mean(melt(error.unrec.level0)$value^2))

error.unrec.level1<-error.unrec[,2:3]
sqrt(mean(melt(error.unrec.level1)$value^2))

error.unrec.level2<-error.unrec[,4:6]
sqrt(mean(melt(error.unrec.level2)$value^2))

error.unrec.level3<-error.unrec[,7:10]
sqrt(mean(melt(error.unrec.level3)$value^2))

error.unrec.level4<-error.unrec[,11:19]
sqrt(mean(melt(error.unrec.level4)$value^2))

error.unrec.level5<-error.unrec[,20:n.base]
sqrt(mean(melt(error.unrec.level5)$value^2))

