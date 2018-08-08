setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/cluster_4
  /Clustering_forecasting/Multiple_OLS"
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

## reading data

cluster_1<-read.csv("cluster_1.csv",header = TRUE) #198
cluster_2<-read.csv("cluster_2.csv",header = TRUE) #198
cluster_3<-read.csv("cluster_3.csv",header = TRUE) #445
cluster_4<-read.csv("cluster_4.csv",header = TRUE) #72

category_uni <- (unique(cluster_4$cat_column))
lag_making <- list()
for (i in seq_along(category_uni))
  lag_making[[i]] <-
  Lag(cluster_4$views[cluster_4$cat_column == category_uni[i]], 1:7)

lag_making <- do.call(rbind, lag_making)
#### sorting dataset based on the category column since lagged columns are base on sorted data
cluster_4 <- cluster_4[order(cluster_4$cat_column), ]
### final data with error lags
cluster_4 <- cbind(cluster_4, lag_making)
cluster_4 <- na.omit(cluster_4)
List_1 = split(cluster_4, cluster_4$cat_column)

########## running Single_step
##### function to do rolling cross-validation for one series and use lapply to repeat it for all series
error_forecast_multiple_OLS <- function(df) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  result <- list()
  error <- list()
  for (i in 1:k) {
    n <- nrow(df)
    df.train <- df[1:((n - k)+(i-1)),]
    df.valid <- df[(n - k) + i,]
    multiple_OLS_model <-
      lm(views ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
         data = df.train)
    fore_multiple_OLS <-
      predict.glm(multiple_OLS_model, newdata = df.valid)
    error_multiple_OLS <-
      as.vector((df.valid$views - fore_multiple_OLS))
    result[[length(result) + 1]] = as.data.frame(fore_multiple_OLS)
    error[[length(error) + 1]] = as.data.frame(error_multiple_OLS)
    
  }
  return(list(result, error))
}

start.time <- Sys.time()
final_result_multiple_OLS_cluster_4 <-
  lapply(List_1, error_forecast_multiple_OLS)
###cluster_1 Time difference of 47.03025 secs
###cluster_2 Time difference of 47.1389 secs
###cluster_3 Time difference of 1.571421 mins
###cluster_4 Time difference of 16.82806 secs 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### saving multiple-OLS forecast and error results

##### forecast results
List_forecast_cluster_4 <- list()
for (i in 1:72) {
  List_forecast_cluster_4[[length(List_forecast_cluster_4) + 1]] = ldply (final_result_multiple_OLS_cluster_4 [[i]][1], data.frame)
}
##### making data frame from the above lists
forecast_result_multiple_OLS_cluster_4 <-
  do.call("rbind", List_forecast_cluster_4)
names(forecast_result_multiple_OLS_cluster_4)<-NULL
### each series in one column
forecast_result_multiple_OLS_cluster_4<-t(forecast_result_multiple_OLS_cluster_4)
h<-t(as.data.frame(names(final_result_multiple_OLS_cluster_4)))
forecast_result_multiple_OLS_cluster_4<-rbind(h,forecast_result_multiple_OLS_cluster_4)
### saving result
write.csv(forecast_result_multiple_OLS_cluster_4,"forecast_result_multiple_OLS_cluster_4.csv")

##### error results
List_error_cluster_4 <- list()
for (i in 1:72) {
  List_error_cluster_4[[length(List_error_cluster_4) + 1]] = ldply (final_result_multiple_OLS_cluster_4[[i]][2], data.frame)
}
##### making data frame from the above lists
error_result_multiple_OLS_cluster_4 <-
  do.call("rbind", List_error_cluster_4)
names(forecast_result_multiple_OLS_cluster_4)<-NULL
### each series in one column
error_result_multiple_OLS_cluster_4<-t(error_result_multiple_OLS_cluster_4)
h<-t(as.data.frame(names(final_result_multiple_OLS_cluster_4)))
error_result_multiple_OLS_cluster_4<-rbind(h,error_result_multiple_OLS_cluster_4)
### saving result
write.csv(error_result_multiple_OLS_cluster_4,"error_result_multiple_OLS_cluster_4.csv")
