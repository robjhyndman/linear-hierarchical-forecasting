setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Wikipedia_data/
  Clustering_forecasting/Single_OLS"
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
############
### Clustering+single OLS model
###########

cluster_1 <- read.csv("cluster_1.csv", header = TRUE) #198
cluster_2 <- read.csv("cluster_2.csv", header = TRUE) #198
cluster_3 <- read.csv("cluster_3.csv", header = TRUE) #445
cluster_4 <- read.csv("cluster_4.csv", header = TRUE) #72
All_Cluster_Single_Step <-
  list(cluster_1, cluster_2, cluster_3, cluster_4)

### spliting into training and validation sets
split_v_t <- split(cluster_4, cluster_4$Split)
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
cluster_4 <- rbind.fill(training, validation)
cluster_split <- split(cluster_4, cluster_4$Split)
###### fill the same mean and sd with training set in validation set to use later
nm <- c("mean_value", "sd_value")
cluster_split[[2]][nm] <-
  lapply(nm, function(x)
    cluster_split[[1]][[x]][match(cluster_split[[2]]$cat_column, cluster_split[[1]]$cat_column)])
##### final cluster_4 by combining two files
cluster_4 <- do.call("rbind", cluster_split)

##### Lag making
category_unique <- (unique(cluster_4$cat_column))
lag_making <- list()
for (i in seq_along(category_unique))
  lag_making[[i]] <-
  Lag(cluster_4$views_scale[cluster_4$cat_column == category_unique[i]], 1:7)

lag_making <- do.call(rbind, lag_making)
#### sorting dataset based on the category column since lagged columns are base on sorted data
cluster_4 <- cluster_4[order(cluster_4$cat_column),]
### final data with error lags
cluster_4 <- cbind(cluster_4, lag_making)
cluster_4 <- na.omit(cluster_4)
train_model <- subset(cluster_4, cluster_4$Split == "training")
single_model <-
  lm(views_scale ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
     data = train_model)

List_3 = split(cluster_4, cluster_4$cat_column)

########## running Single_step
##### rolling for one series and use lapply to repeat it for all series
forecast_error_Single_OLS_rolling <- function(df) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  result <- list()
  error <- list()
  for (i in 1:k) {
    n <- nrow(df)
    cluster_4.train <- df[1:((n - k) + (i - 1)), ]
    cluster_4.valid <- df[((n - k) + i), ]
    fore_single_OLS <-
      (
        predict.glm(single_model, newdata = cluster_4.valid) * cluster_4.valid$sd_value[1]
      ) + cluster_4.valid$mean_value[1]
    error_single_OLS <-
      as.vector((cluster_4.valid$views - fore_single_OLS))### rescale the errors
    result[[length(result) + 1]] = as.data.frame(fore_single_OLS)
    error[[length(error) + 1]] = as.data.frame(error_single_OLS)
  }
  return(list(result, error))
}

start.time <- Sys.time()
final_result_multiple_OLS_cluster_4 = lapply(List_3, forecast_error_Single_OLS_rolling)
###cluster_1 Time difference of 23.17273 secs
###cluster_2 Time difference of 23.47116 secs
###cluster_3 Time difference of 1.016873 mins
###cluster_4 Time difference of 8.487066 secs
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### saving multiple-OLS forecast and error results

##### forecast results
List_forecast_cluster_4 <- list()
for (i in 1:72) {
  List_forecast_cluster_4[[length(List_forecast_cluster_4) + 1]] = ldply (final_result_multiple_OLS_cluster_4[[i]][1], data.frame)
}
##### making data frame from the above lists
forecast_result_single_OLS_cluster_4 <-
  do.call("rbind",  List_forecast_cluster_4)
names(forecast_result_single_OLS_cluster_4) <- NULL
### each series in one column
forecast_result_single_OLS_cluster_4 <-
  t(forecast_result_single_OLS_cluster_4)
h <- t(as.data.frame(names(final_result_multiple_OLS_cluster_4)))
forecast_result_single_OLS_cluster_4 <-
  rbind(h, forecast_result_single_OLS_cluster_4)
### saving result
write.csv(
  forecast_result_single_OLS_cluster_4,
  "forecast_result_single_OLS_cluster_4.csv"
)

##### error results
List_error <- list()
for (i in 1:72) {
  List_error[[length(List_error) + 1]] = ldply (final_result_multiple_OLS_cluster_4[[i]][2], data.frame)
}
##### making data frame from the above lists
error_result_single_OLS_cluster_4 <-
  do.call("rbind", List_error)
names(error_result_single_OLS_cluster_4) <- NULL
### each series in one column
error_result_single_OLS_cluster_4 <-
  t(error_result_single_OLS_cluster_4)
h <- t(as.data.frame(names(final_result_multiple_OLS_cluster_4)))
error_result_single_OLS_cluster_4 <-
  rbind(h, error_result_single_OLS_cluster_4)
### saving result
write.csv(error_result_single_OLS_cluster_4,
          "error_result_single_OLS_cluster_4.csv")
