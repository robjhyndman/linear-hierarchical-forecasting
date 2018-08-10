setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Wikipedia_data/General_forecasting/Input_Data"
)
lapply(
  c(
    "quantmod",
    "ggplot2",
    "partykit",
    "scales",
    "matrixStats",
    "MASS",
    "GGally",
    "forecast",
    "dplyr",
    "plyr",
    "reshape",
    "reshape2",
    "gridExtra",
    "stringr",
    "hts"
  ),
  require,
  character.only = T
)
set.seed(123)
################
#### Reading data
wikipedia_data <-
  read.csv("wikipedia_data.csv", header = TRUE)#### remove "-" from mobile-app and mobile-web&cleaning categories
#######creating cat_column in main dataset to use in all methods

#cat_unique <- as.vector(unique(wikipedia_data$category))
### spliting name strings
#cat_unique_split <- strsplit(cat_unique, "-")
#cat_unique_split_  <-
# data.frame(matrix(unlist(cat_unique_split), nrow = 913, byrow = T))
#### all the string specified by two characters
#col_1 <- substr(cat_unique_split_[, 1] , start = 1, stop = 7)
#col_2 <- substr(cat_unique_split_[, 2] , start = 1, stop = 2)
#col_3 <- substr(cat_unique_split_[, 3] , start = 1, stop = 2)
#col_4 <- as.character(as.numeric(cat_unique_split_[, 4]))
#col_4 <-
#  str_pad(col_4, 2, pad = "0")### making them all with two digits
#col_5 <- substr(cat_unique_split_[, 5], start = 1, stop = 3)
#names <- as.vector(paste(col_1, col_2, col_3, col_5, col_4, sep = ""))
## adding a category column to the dataset same as hierarchy method series lables
#cat_column<-rep(names,each=394)
#wikipedia_data$cat_column<-cat_column
##############################
#### Hierarchical Method +ARIMA and ETS
##############################
##Data reshaping
wikipedia_wide <-
  as.data.frame(matrix(wikipedia_data$views, nrow = 394, ncol = 913)) #394: length of each series and 913: number of series
##############
names <- as.vector(unique(wikipedia_data$cat_column))
names(wikipedia_wide) <- c(names)
##################
## using heirachies and groupings
##################
#test_reshape_hts <- test_reshape[, -c(1, 2)]
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
total_wikipedia <-
  ts(wikipedia_wide,
     frequency = 7,
     start = c(1, 1),
     c(1, 394))
validation_wikipedia <-
  window(total_wikipedia,
         start = c(1, 367),
         end = c(1, 394))
error_forecast_hierarchy <- function(total_wikipedia) {
  k <-
    28 #### rolling window width
  n <- nrow(total_wikipedia)
  forecast_arima <- list()
  forecast_ets <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia  <-
      window(total_wikipedia,
             start = c(1, 1),
             end = c(1, (n - k) + (i - 1)))
    ### validation set
    validation_wikipedia <-
      window(total_wikipedia,
             start = c(1, (n - k) + i),
             end = c(1, (n - k) + i))
    g_wikipedia_hts <-
      gts(training_wikipedia, groups = grouping_hts)
    ### running the forecast
    fore_hts_arima <-
      forecast.gts(
        g_wikipedia_hts,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    fore_hts_ets <-
      forecast.gts(
        g_wikipedia_hts,
        fmethod = "ets",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    forecast_arima[[length(forecast_arima) + 1]] = as.data.frame(fore_hts_arima$bts)
    forecast_ets[[length(forecast_ets) + 1]] = as.data.frame(fore_hts_ets$bts)
  }
  return(list(forecast_arima, forecast_ets))
}
start.time <- Sys.time()
final_result_hierarchy <-
  error_forecast_hierarchy(total_wikipedia)#### arima: Time difference of 11.03499 hours
#### ets: Time difference of 3.580751 hours
end.time <- Sys.time()
end.time - start.time
### saving hierarchy forecast and error results
forecast_hierarchy_arima <-
  do.call("rbind", final_result_hierarchy[[1]])
error_hierarchy_arima <-
  as.data.frame(validation_wikipedia) - forecast_hierarchy_arima
forecast_hierarchy_ets <-
  do.call("rbind", final_result_hierarchy[[2]])
error_hierarchy_ets <-
  as.data.frame(validation_wikipedia) - forecast_hierarchy_ets
#########################
#### Using multiple linear models
#########################
wikipedia_data <- read.csv("wikipedia_data.csv", header = TRUE)
category_uni <- (unique(wikipedia_data$cat_column))
lag_making <- list()
for (i in seq_along(category_uni))
  lag_making[[i]] <-
  Lag(wikipedia_data$views[wikipedia_data$cat_column == category_uni[i]], 1:7)
lag_making <- do.call(rbind, lag_making)
### dataset with lags
wikipedia_data <- cbind(wikipedia_data, lag_making)
wikipedia_data <- na.omit(wikipedia_data)
List_1 = split(wikipedia_data, wikipedia_data$cat_column)
error_forecast_multiple_OLS <- function(df) {
  k <-
    28 #### rolling-window width
  result <- list()
  error <- list()
  for (i in 1:k) {
    n <- nrow(df)
    df.train <- df[1:((n - k) + (i - 1)),]
    df.valid <- df[(n - k) + i,]
    multiple_OLS_model <-
      lm(views ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
         data = df.train)
    fore_multiple_OLS <-
      predict.lm(multiple_OLS_model, newdata = df.valid)
    error_multiple_OLS <-
      as.vector((df.valid$views - fore_multiple_OLS))
    result[[length(result) + 1]] = as.data.frame(fore_multiple_OLS)
    error[[length(error) + 1]] = as.data.frame(error_multiple_OLS)
  }
  return(list(result, error))
}
start.time <- Sys.time()
final_result_multiple_OLS <-
  lapply(List_1, error_forecast_multiple_OLS)### Time difference of 1.308091 mins
end.time <- Sys.time()
end.time - start.time
### saving multiple-OLS forecast and error results
##### forecast results
List_forecast <- list()
for (i in 1:913) {
  List_forecast[[length(List_forecast) + 1]] = ldply (final_result_multiple_OLS[[i]][1], data.frame)
}
##### making data frame from the above lists
forecast_result_multiple_OLS <-
  do.call("rbind", List_forecast)
names(forecast_result_multiple_OLS) <- NULL
### each series in one column
forecast_result_multiple_OLS <- t(forecast_result_multiple_OLS)
h <- t(as.data.frame(names(final_result_multiple_OLS)))
forecast_result_multiple_OLS <-
  rbind(h, forecast_result_multiple_OLS)
##### error results
List_error <- list()
for (i in 1:913) {
  List_error[[length(List_error) + 1]] = ldply (final_result_multiple_OLS[[i]][2], data.frame)
}
##### making data frame from the above lists
error_result_multiple_OLS <-
  do.call("rbind", List_error)
names(forecast_result_multiple_OLS) <- NULL
### each series in one column
error_result_multiple_OLS <- t(error_result_multiple_OLS)
h <- t(as.data.frame(names(final_result_multiple_OLS)))
error_result_multiple_OLS <- rbind(h, error_result_multiple_OLS)
#########################
#### Using ARIMA models
#########################
#### using long data made in hierarchy approach to keep same labels
wikipedia_long <-
  read.csv("wikipedia_long_hierarchy.csv", header = TRUE)
new_matrix <- wikipedia_long
new_matrix_T <- t(as.matrix(new_matrix))
# each rows as one list member
List_2 <- do.call(c, apply(new_matrix_T, 1, list))
#List_2 <- convertColsToList(new_matrix)
fore_error_arima <- function(df) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  result <- list()
  error <- list()
  for (i in 1:k) {
    n <- length(df)
    simulated_series.ts <- ts(df,
                              start = c(1, 1),
                              end = c(1, 394),
                              freq = 7)
    train.ts <-
      window(simulated_series.ts, end = c(1, (n - k) + (i - 1)))
    valid.ts <-
      window(simulated_series.ts,
             start = c(1, (n - k) + i),
             end = c(1, (n - k) + i))
    #### forecast and error
    fit <- auto.arima(train.ts)
    pred_arima <- forecast(fit, h = 1)
    error_arima <-
      as.numeric(valid.ts) - as.numeric(pred_arima$mean)
    result[[length(result) + 1]] = as.data.frame(pred_arima$mean)
    error[[length(error) + 1]] = as.data.frame(error_arima)
  }
  return(list(result, error))
}
start.time <- Sys.time()
forecast_error_result_arima = lapply(List_2, fore_error_arima) ###Time difference of 15.14 hours
end.time <- Sys.time()
end.time - start.time
##### forecast results
List_forecast <- list()
for (i in 1:913) {
  List_forecast[[length(List_forecast) + 1]] = as.numeric(ldply (forecast_error_result_arima[[i]][1], data.frame))
}
##### making data frame from the above lists
forecast_result_arima <-
  do.call("cbind", List_forecast)
names(forecast_result_arima) <- NULL
### each series in one column
forecast_result_multiple_OLS <- t(forecast_result_arima)
h <- t(as.data.frame(names(forecast_error_result_arima)))
forecast_result_arima <- rbind(h, forecast_result_arima)
##### error results
List_error <- list()
for (i in 1:913) {
  List_error[[length(List_error) + 1]] = ldply (forecast_error_result_arima[[i]][2], data.frame)
}
##### making data frame from the above lists
error_result_arima <-
  do.call("rbind", List_error)
names(forecast_result_arima) <- NULL
### each series in one column
error_result_arima <- t(error_result_arima)
h <- t(as.data.frame(names(forecast_error_result_arima)))
error_result_arima <- rbind(h, error_result_arima)

