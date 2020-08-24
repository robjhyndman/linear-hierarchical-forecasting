
## OLS forecasts
## Assumes x is a ts object with frequency set correctly
## Produces forecasts from a linear model with trend, seasonal dummy variables
## and (optionally) some autoregressive terms (lagno = vector of the lag number which we want to include in the model )

olsfc <- function(x, h, maxlag = 0, nolag = NULL ) {
  # Set up data frame for modelling
  n <- length(x)
  modeldata <- data.frame(
    x = as.numeric(x),
    trend = seq_along(x),
    season = factor(cycle(x))
  )
  if(maxlag > 0)
  {
    lagnames <- paste0("lag", seq(maxlag))
    for (i in seq_along(lagnames))
      modeldata[[lagnames[i]]] <- c(rep(NA, i), x[seq(n - i)])
  } else {
    lagnames <- NULL
  }
  # Set up formula for linear model
  if (length(nolag) == 0)
    nolag <- seq(maxlag) 
  form <- "x ~ trend + season"
  for (i in nolag)
    form <- paste0(form, " + ", "lag", i)
  form <- as.formula(form)
  
  # Fit model
  fit <- lm(form, data = modeldata)
  
  # Set up forecast vector
  fc <- ts(numeric(h),
           frequency = frequency(x),
           start = tsp(x)[2] + 1 / frequency(x)
  )
  lwr <- ts(numeric(h),
            frequency = frequency(x),
            start = tsp(x)[2] + 1 / frequency(x)
  )
  upr <- ts(numeric(h),
            frequency = frequency(x),
            start = tsp(x)[2] + 1 / frequency(x)
  )
  se <- ts(numeric(h),
           frequency = frequency(x),
           start = tsp(x)[2] + 1 / frequency(x)
  )
  residual.scale <- c() 
  # Set up new data for forecasting
  trend <- length(x) + seq(h)
  season <- factor(cycle(fc))
  newdata <- data.frame(trend = trend[1], season = season[1])
  for (i in seq_along(lagnames))
    newdata[[lagnames[i]]] <- tail(x,i)[1]
  
  # Compute forecasts
  for (i in seq_along(fc))
  {
    pred <-  predict(fit, newdata = newdata, se.fit = TRUE, 
                     residual.scale = TRUE, interval = "prediction", level = 0.95)
    fc[i] <- pred$fit[,1]
    lwr[i] <- pred$fit[,2]
    upr[i] <- pred$fit[,3]
    se[i] <- pred$se.fit
    residual.scale[i] <- pred$residual.scale
    # Update newdata
    if(maxlag > 0)
    {
      newdata[lagnames[seq(maxlag)]] <- c(fc[i], newdata[lagnames[1:(maxlag-1)]]) 
    }
    newdata[['trend']] <- trend[i+1]
    newdata[['season']] <- season[i+1]
  }
  return(list(fc, lwr, upr, se, residual.scale))
}


