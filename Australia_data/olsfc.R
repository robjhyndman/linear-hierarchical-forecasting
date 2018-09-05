## OLS forecasts
## Assumes x is a ts object with frequency set correctly
## Produces forecasts from a linear model with trend, seasonal dummy variables
## and (optionally) some autoregressive terms

olsfc <- function(x, h, maxlag = 0) {
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
  form <- "x ~ trend + season"
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
  for (i in seq_along(lagnames))
    newdata[[lagnames[i]]] <- tail(x,i)[1]

  # Compute forecasts
  for (i in seq_along(fc))
  {
    fc[i] <- predict(fit, newdata = newdata)
    # Update newdata
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

