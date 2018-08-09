setwd(
"G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Wikipedia_data/Clustering_forecasting/Input_Data"
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
cluster_1<-read.csv("cluster_1.csv",header = TRUE) #198 series
cluster_2<-read.csv("cluster_2.csv",header = TRUE) #198 series
cluster_3<-read.csv("cluster_3.csv",header = TRUE) #445 series
cluster_4<-read.csv("cluster_4.csv",header = TRUE) #72  series
All_clusters<-list(cluster_1,cluster_2,cluster_3,cluster_4)
forecast_error_all_clusters_multiple_OLS<-function(df){
  category_uni <- (unique(df$cat_column))
  lag_making <- list()
  for (i in seq_along(category_uni))
    lag_making[[i]] <-
    Lag(df$views[df$cat_column == category_uni[i]], 1:7)
    lag_making <- do.call(rbind, lag_making)
  ### data with lags
  df <- cbind(df, lag_making)
  df <- na.omit(df)
  List_1 = split(df, df$cat_column)
  error_forecast_multiple_OLS <- function(df) {
    k <-
      28 #### rolling-windows width
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
return(final_result_multiple_OLS_df <-
    lapply(List_1, error_forecast_multiple_OLS))
  ###cluster_1 Time difference of 47.03025 secs
  ###cluster_2 Time difference of 47.1389 secs
  ###cluster_3 Time difference of 1.571421 mins
  ###df Time difference of 16.82806 secs 
}
start.time <- Sys.time()
forecast_error_multiple_OLS <-
  lapply(All_clusters, forecast_error_all_clusters_multiple_OLS)
end.time <- Sys.time()
end.time - start.time


### saving multiple-OLS forecast and error results
unlist_forecast_error_multiple_OLS<-unlist(forecast_error_multiple_OLS,recursive = FALSE)
##### forecast results
List_forecast <- list()
for (i in 1:913) {
  List_forecast[[length(List_forecast) + 1]] = ldply (unlist_forecast_error_multiple_OLS[[i]][1], data.frame)
}
#####data frame from the above lists
forecast_result_multiple_OLS <-
  do.call("rbind", List_forecast)
names(forecast_result_multiple_OLS)<-NULL; forecast_result_multiple_OLS<-as.data.frame(t(forecast_result_multiple_OLS))
h<-t(as.data.frame(names(unlist_forecast_error_multiple_OLS)));names(forecast_result_multiple_OLS)<-h
### forecast result for each cluster by matching names
name_cluster_1<-unique(cluster_1$cat_column);name_cluster_2<-unique(cluster_2$cat_column)
name_cluster_3<-unique(cluster_3$cat_column);name_cluster_4<-unique(cluster_4$cat_column)
forecast_multiple_OLS_cluster_1<-forecast_result_multiple_OLS[,names(forecast_result_multiple_OLS) %in% (name_cluster_1)]
forecast_multiple_OLS_cluster_2<-forecast_result_multiple_OLS[,names(forecast_result_multiple_OLS) %in% (name_cluster_2)]
forecast_multiple_OLS_cluster_3<-forecast_result_multiple_OLS[,names(forecast_result_multiple_OLS) %in% (name_cluster_3)]
forecast_multiple_OLS_cluster_4<-forecast_result_multiple_OLS[,names(forecast_result_multiple_OLS) %in% (name_cluster_4)]
### saving result
write.csv(forecast_multiple_OLS_cluster_1,"forecast_result_multiple_OLS_cluster_1.csv")
write.csv(forecast_multiple_OLS_cluster_2,"forecast_result_multiple_OLS_cluster_2.csv")
write.csv(forecast_multiple_OLS_cluster_3,"forecast_result_multiple_OLS_cluster_3.csv")
write.csv(forecast_multiple_OLS_cluster_4,"forecast_result_multiple_OLS_cluster_4.csv")
##### error results
List_error <- list()
for (i in 1:913) {
  List_error[[length(List_error) + 1]] = ldply (unlist_forecast_error_multiple_OLS[[i]][2], data.frame)
}
#####data frame from the above lists
error_result_multiple_OLS <-
  do.call("rbind", List_error)
names(error_result_multiple_OLS)<-NULL; error_result_multiple_OLS<-as.data.frame(t(error_result_multiple_OLS))
h<-t(as.data.frame(names(unlist_forecast_error_multiple_OLS)));names(error_result_multiple_OLS)<-h
### error result for each cluster by matching names
name_cluster_1<-unique(cluster_1$cat_column);name_cluster_2<-unique(cluster_2$cat_column)
name_cluster_3<-unique(cluster_3$cat_column);name_cluster_4<-unique(cluster_4$cat_column)
error_multiple_OLS_cluster_1<-error_result_multiple_OLS[,names(error_result_multiple_OLS) %in% (name_cluster_1)]
error_multiple_OLS_cluster_2<-error_result_multiple_OLS[,names(error_result_multiple_OLS) %in% (name_cluster_2)]
error_multiple_OLS_cluster_3<-error_result_multiple_OLS[,names(error_result_multiple_OLS) %in% (name_cluster_3)]
error_multiple_OLS_cluster_4<-error_result_multiple_OLS[,names(error_result_multiple_OLS) %in% (name_cluster_4)]
### saving result
write.csv(error_multiple_OLS_cluster_1,"error_result_multiple_OLS_cluster_1.csv")
write.csv(error_multiple_OLS_cluster_2,"error_result_multiple_OLS_cluster_2.csv")
write.csv(error_multiple_OLS_cluster_3,"error_result_multiple_OLS_cluster_3.csv")
write.csv(error_multiple_OLS_cluster_4,"error_result_multiple_OLS_cluster_4.csv")