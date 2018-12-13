## OLS forecasts
## Assumes x is a ts object with frequency set correctly
## Produces forecasts from a linear model with trend, seasonal dummy variables
## and (optionally) some autoregressive terms
## externaldata (optionally) can be a vector or matrix of external time series - its length should be same as you original data (train and test sets)

olsfc.external <- function(x, externaldata=NULL , h, maxlag = 0) {
  # Set up data frame for modelling
  n <- length(x)
    externaldata<-as.data.frame(externaldata)
    externaltrain<-as.data.frame(externaldata[(1:n),])
    externaltest<-as.data.frame(externaldata[((n+1):(n+h)),])
    modeldata <- data.frame(
      x = as.numeric(x),
      trend = seq_along(x),
      season = factor(cycle(x))
    )
    if(is.null(externaldata) == FALSE){
      externalnames <- paste0("external", seq(ncol(externaldata)))
      for (i in seq_along(externalnames))
        modeldata[[externalnames[i]]] <- externaltrain[,i]
    } 
    else{
      externalnames<-NULL
    }
    if(maxlag > 0)
   {
    lagnames <- paste0("lag", seq(maxlag))
    for (i in seq_along(lagnames))
      modeldata[[lagnames[i]]] <- c(rep(NA, i), x[seq(n - i)])
   } else {
    lagnames <- NULL
   }
    # Set up formula for linear model
    form <- "x ~ trend + season"
  for(i in seq_along(externalnames))
    form <- paste0(form, " + ", "external", i)
  for (i in seq_along(lagnames))
    form <- paste0(form, " + ", "lag", i)
  
  form <- as.formula(form)
  
  # Fit model
  fit <- lm(form, data = modeldata)
  
  # Set up forecast vector
  fc <- ts(numeric(h),
           frequency = frequency(x),
           start = tsp(x)[2] + 1 / frequency(x)
  )
  
  # Set up new data for forecasting
  trend <- length(x) + seq(h)
  season <- factor(cycle(fc))
  newdata <- data.frame(trend = trend[1], season = season[1]) 
  for(i in seq_along(externalnames))
    newdata[[externalnames[i]]] <- externaltest[1,i]
  for (i in seq_along(lagnames))
    newdata[[lagnames[i]]] <- tail(x,i)[1] 
  # Compute forecasts
  for (i in seq_along(fc))
  {
    fc[i] <- predict(fit, newdata = newdata)
    # Update newdata
    if(is.null(externaldata) == FALSE){
      for(j in seq_along(externalnames))
        newdata[[externalnames[j]]] <- externaltest[i,j]
    }
    if(maxlag > 0)
    {
      newdata[["lag1"]] <- fc[i]
      for (j in seq_along(lagnames[-1]))
        newdata[[lagnames[j+1]]] <- newdata[[lagnames[j]]]
    }
    newdata[['trend']] <- trend[i+1]
    newdata[['season']] <- season[i+1]
    }
  return(fc)
}

test.mahsa <- pmax(olsfc.external(ally[,1], h=h, maxlag = 12, externaldata =easter.info),0)
