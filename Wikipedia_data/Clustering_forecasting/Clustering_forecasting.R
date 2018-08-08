setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/Wikipedia_data/Clustering_forecasting"
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
### Clustering+ARIMA
###############

cluster_1 <- read.csv("cluster_1.csv", header = TRUE)
cluster_2 <- read.csv("cluster_2.csv", header = TRUE)
cluster_3 <- read.csv("cluster_3.csv", header = TRUE)
cluster_4 <- read.csv("cluster_4.csv", header = TRUE)
All_Cluster_Single_Step <-
  list(cluster_1, cluster_2, cluster_3, cluster_4)
forecast_error_arima_cluster <- function(df_1) {
  new_matrix <-
    matrix(df_1$views, nrow = 394, ncol = nrow(df_1) / 394)
  new_matrix_T <- t(as.matrix(new_matrix))
  # each rows as one list member
  List_2 <- do.call(c, apply(new_matrix_T, 1, list))
  #List_2 <- convertColsToList(new_matrix)
  error_MSE_arima <- function(df) {
    k <-
      28 #### number of points that we want to forecast (rolling)
    result <- list()
    error<-list()
    running_time<-list()
    for (i in 1:k) {
      n <- length(df)
      simulated_series.ts <- ts(df,
                                start = c(1, 1),
                                end = c(1, 394),
                                freq = 7)
      train.ts <- window(simulated_series.ts, end = c(1, n - i))
      valid.ts <-
        window(simulated_series.ts,
               start = c(1, ((n - k) + (i-1))),
               end = c(1, ((n - k) + i)))
      ####### forecasting and error
      fit <- auto.arima(train.ts)
      pred_arima <- forecast(fit, h = 1)
      error_arima<-
        as.numeric(valid.ts) - as.numeric(pred_arima$mean)
      result[[length(result) + 1]] = as.data.frame(pred_arima$mean)
      error[[length(error) + 1]] = as.data.frame(error_arima)
    running_time[[length(running_time) + 1]]<-time.taken
    }
    return(list(result,error,running_time))
  }
  start.time <- Sys.time()
  error_fit_arima = lapply(List_2, error_MSE_arima)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
 }

start.time <- Sys.time()
forecast_error_arima_final_results <-
lapply(All_Cluster_Single_Step, forecast_error_arima_cluster) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### ADD SAVING PART

############
### Clustering+single OLS model
###########

cluster_1 <- read.csv("cluster_1.csv", header = TRUE)
cluster_2 <- read.csv("cluster_2.csv", header = TRUE)
cluster_3 <- read.csv("cluster_3.csv", header = TRUE)
cluster_4 <- read.csv("cluster_4.csv", header = TRUE)
All_Cluster_Single_Step <-
  list(cluster_1, cluster_2, cluster_3, cluster_4)
forecast_error_Single_OLS <- function(df_1) {
  ### spliting into training and validation sets
  split_v_t <- split(df_1, df_1$Split)
  training <- as.data.frame(split_v_t[[1]])
  validation <- as.data.frame(split_v_t[[2]])
  training$mean.values <- ave(training$views, training$cat_column)
  split_t <- split(training, training$cat_column)
  #### finding mean and sd for each group in training set
  m <- function(df_2) {
    df_2$views_scale <- scale(df_2$views)
    df_2$mean_value <- mean(df_2$views)
    df_2$sd_value <- sd(df_2$views)
    return(df_2)
  }
  training <- lapply(split_t, m)
  training <- do.call("rbind", training)
  df_1 <- rbind.fill(training, validation)
  cluster_split <- split(df_1, df_1$Split)
  ###### fill the same mean and sd with training set in validation set to use later
  nm <- c("mean_value", "sd_value")
  cluster_split[[2]][nm] <-
    lapply(nm, function(x)
      cluster_split[[1]][[x]][match(cluster_split[[2]]$cat_column, cluster_split[[1]]$cat_column)])
  ##### final df_1 by combining two files
  df_1 <- do.call("rbind", cluster_split)
  
  ##### Lag making
  category_unique <- (unique(df_1$cat_column))
  lag_making <- list()
  for (i in seq_along(category_unique))
    lag_making[[i]] <-
    Lag(df_1$views_scale[df_1$cat_column == category_unique[i]], 1:7)
  
  lag_making <- do.call(rbind, lag_making)
  #### sorting dataset based on the category column since lagged columns are base on sorted data
  df_1 <- df_1[order(df_1$cat_column), ]
  ### final data with error lags
  df_1 <- cbind(df_1, lag_making)
  df_1 <- na.omit(df_1)
  train_model <- subset(df_1, df_1$Split == "training")
  single_model <-
    lm(
      views_scale ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
      data = train_model
    )
  
  List_3 = split(df_1, df_1$cat_column)
  
  ########## running Single_step
  ##### rolling for one series and use lapply to repeat it for all series
  forecast_error_Single_OLS_rolling <- function(df) {
    k <-
      28 #### number of points that we want to forecast finally (rolling)
    result <- list()
    error<-list()
     for (i in 1:k) {
      n <- nrow(df)
      df_1.train <- df[1:((n - k)+(i-1)),]
      df_1.valid <- df[((n - k) + i),]
      fore_single_OLS <-
        (predict.glm(single_model, newdata = df_1.valid) * df_1.valid$sd_value[1]) + df_1.valid$mean_value[1]
      error_single_OLS <-
        as.vector((df_1.valid$views - fore_single_OLS))### rescale the errors
      result[[length(result) + 1]] = fore_single_OLS
      error[[length(error) + 1]] = error_single_OLS 
                }
    return(list(result,error))
  }
 
  return(error_fit_Single_Step = lapply(List_3, forecast_error_Single_OLS_rolling ))
  
}

start.time <- Sys.time()
forecast_error_Single_OLS_final_results <-
  lapply(All_Cluster_Single_Step, forecast_error_Single_OLS) ## Time difference of 1.759134 mins

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### ADD SAVING PART



#############
### clustering + multiple OLS
############

cluster_1 <- read.csv("cluster_1.csv", header = TRUE)
cluster_2 <- read.csv("cluster_2.csv", header = TRUE)
cluster_3 <- read.csv("cluster_3.csv", header = TRUE)
cluster_4 <- read.csv("cluster_4.csv", header = TRUE)
All_Cluster_Single_Step <-
  list(cluster_1, cluster_2, cluster_3, cluster_4)

forecast_error_multiple_OLS <- function(df_1) {
  ##### Lag making
  category_sort <- sort(unique(df_1$category))
  lag_making <- list()
  for (i in seq_along(category_sort))
    lag_making[[i]] <-
    Lag(df_1$views[df_1$category == category_sort[i]], 1:7)
  
  lag_making <- do.call(rbind, lag_making)
  #### sorting dataset based on the category column since lagged columns are base on sorted data
  df_1 <- df_1[order(df_1$category),]
  ### final data with error lags
  df_1 <- cbind(df_1, lag_making)
  df_1 <- na.omit(df_1)
  List_3 = split(df_1, df_1$category)
  
  ########## running Single_step
  ##### rolling cross-validation for one series and use lapply to repeat it for all series
  forecast_error_multiple_OLS_rolling <- function(df) {
    k <-
      28 #### number of points that we want to forecast finally (rolling)
    result <- list()
    error<-list()
    running_time<-list()
    for (i in 1:k) {
      n <- nrow(df)
      df_1.train <- df[1:((n - k)+(i-1)), ]
      df_1.valid <- df[((n - k) + i), ]
      single_model <-
        lm(views ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
           data = df_1.train)
      fore_multiple_OLS <-
        predict.glm(single_model, newdata = df_1.valid)
      single_model.error.valid <-
        as.vector((df_1.valid$views - fore_multiple_OLS))
      result[[length(result) + 1]] = fore_single_OLS
      error[[length(error) + 1]] = error_single_OLS 
      running_time[[length(running_time) + 1]]<-time.taken
    }
    return(list(result,error,running_time))
  }
  
  start.time <- Sys.time()
  return(error_fit_Single_Step_many_models = lapply(List_3, forecast_error_multiple_OLS_rolling))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
}

start.time <- Sys.time()
forecast_error_multiple_OLS_final_results <-
  lapply(All_Cluster_Single_Step, forecast_error_multiple_OLS) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#### ADD SAVING PART


