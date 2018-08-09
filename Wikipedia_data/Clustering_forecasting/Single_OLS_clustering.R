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
############
### Clustering+single OLS model
###########
cluster_1 <- read.csv("cluster_1.csv", header = TRUE) #198 series
cluster_2 <- read.csv("cluster_2.csv", header = TRUE) #198 series
cluster_3 <- read.csv("cluster_3.csv", header = TRUE) #445 series
cluster_4 <- read.csv("cluster_4.csv", header = TRUE) #72 series
All_Cluster<-
  list(cluster_1, cluster_2, cluster_3, cluster_4)
forecast_error_single_OLS<-function(df){
  ### spliting into training and validation sets
  split_v_t <- split(df,df$Split)
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
  df <- rbind.fill(training, validation)
  cluster_split <- split(df, df$Split)
  ###### fill the same mean and sd with training set in validation set 
  nm <- c("mean_value", "sd_value")
  cluster_split[[2]][nm] <-
    lapply(nm, function(x)
      cluster_split[[1]][[x]][match(cluster_split[[2]]$cat_column, cluster_split[[1]]$cat_column)])
   df <- do.call("rbind", cluster_split)
   ##### Lag making
  category_unique <- (unique(df$cat_column))
  lag_making <- list()
  for (i in seq_along(category_unique))
    lag_making[[i]] <-
    Lag(df$views_scale[df$cat_column == category_unique[i]], 1:7)
  lag_making <- do.call(rbind, lag_making)
  ### final data with error lags
  df <- cbind(df, lag_making)
  df <- na.omit(df)
  train_model <- subset(df, df$Split == "training")
  single_model <-
    lm(views_scale ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4 + Lag.5 + Lag.6 + Lag.7,
       data = train_model)
  List_3 = split(df, df$cat_column)
  forecast_error_single_OLS_rolling <- function(df_3) {
    k <-
      28 #### rolling-windows width
    forecast <- list()
    error <- list()
    for (i in 1:k) {
      n <- nrow(df_3)
      df_3.train <- df_3[1:((n - k) + (i - 1)), ]
      df_3.valid <- df_3[((n - k) + i), ]
      fore_single_OLS <-
        (
          predict.lm(single_model, newdata = df_3.valid) * df_3.valid$sd_value[1]
        ) + df_3.valid$mean_value[1]
      error_single_OLS <-
        as.vector((df_3.valid$views - fore_single_OLS))
      forecast[[length(forecast) + 1]] = as.data.frame(fore_single_OLS)
      error[[length(error) + 1]] = as.data.frame(error_single_OLS)
    }
    return(list(forecast,error))
  }
  return(final_result_single_OLS = lapply(List_3, forecast_error_single_OLS_rolling))
  ###cluster_1 Time difference of 23.17273 secs
  ###cluster_2 Time difference of 23.47116 secs
  ###cluster_3 Time difference of 1.016873 mins
  ###df Time difference of 8.487066 secs
}
start.time <- Sys.time()
forecast_error_single_OLS_result <-
  lapply(All_clusters,forecast_error_single_OLS)
end.time <- Sys.time()
end.time - start.time
### saving multiple-OLS forecast and error results
unlist_forecast_error_single_OLS<-unlist(forecast_error_single_OLS_result,recursive = FALSE)
##### forecast results
List_forecast <- list()
for (i in 1:913) {
  List_forecast[[length(List_forecast) + 1]] = ldply (unlist_forecast_error_single_OLS[[i]][1], data.frame)
}
#####data frame from the above lists
forecast_result_single_OLS <-
  do.call("rbind", List_forecast)
names(forecast_result_single_OLS)<-NULL; forecast_result_single_OLS<-as.data.frame(t(forecast_result_single_OLS))
h<-t(as.data.frame(names(unlist_forecast_error_single_OLS)));names(forecast_result_single_OLS)<-h
### forecast result for each cluster by matching names
name_cluster_1<-unique(cluster_1$cat_column);name_cluster_2<-unique(cluster_2$cat_column)
name_cluster_3<-unique(cluster_3$cat_column);name_cluster_4<-unique(cluster_4$cat_column)
forecast_single_OLS_cluster_1<-forecast_result_single_OLS[,names(forecast_result_single_OLS) %in% (name_cluster_1)]
forecast_single_OLS_cluster_2<-forecast_result_single_OLS[,names(forecast_result_single_OLS) %in% (name_cluster_2)]
forecast_single_OLS_cluster_3<-forecast_result_single_OLS[,names(forecast_result_single_OLS) %in% (name_cluster_3)]
forecast_single_OLS_cluster_4<-forecast_result_single_OLS[,names(forecast_result_single_OLS) %in% (name_cluster_4)]
### saving result
write.csv(forecast_single_OLS_cluster_1,"forecast_result_single_OLS_cluster_1.csv")
write.csv(forecast_single_OLS_cluster_2,"forecast_result_single_OLS_cluster_2.csv")
write.csv(forecast_single_OLS_cluster_3,"forecast_result_single_OLS_cluster_3.csv")
write.csv(forecast_single_OLS_cluster_4,"forecast_result_single_OLS_cluster_4.csv")
##### error results
List_error <- list()
for (i in 1:913) {
  List_error[[length(List_error) + 1]] = ldply (unlist_forecast_error_single_OLS[[i]][2], data.frame)
}
#####data frame from the above lists
error_result_single_OLS <-
  do.call("rbind", List_error)
names(error_result_single_OLS)<-NULL; error_result_single_OLS<-as.data.frame(t(error_result_single_OLS))
h<-t(as.data.frame(names(unlist_forecast_error_single_OLS)));names(error_result_single_OLS)<-h
### error result for each cluster by matching names
name_cluster_1<-unique(cluster_1$cat_column);name_cluster_2<-unique(cluster_2$cat_column)
name_cluster_3<-unique(cluster_3$cat_column);name_cluster_4<-unique(cluster_4$cat_column)
error_single_OLS_cluster_1<-error_result_single_OLS[,names(error_result_single_OLS) %in% (name_cluster_1)]
error_single_OLS_cluster_2<-error_result_single_OLS[,names(error_result_single_OLS) %in% (name_cluster_2)]
error_single_OLS_cluster_3<-error_result_single_OLS[,names(error_result_single_OLS) %in% (name_cluster_3)]
error_single_OLS_cluster_4<-error_result_single_OLS[,names(error_result_single_OLS) %in% (name_cluster_4)]
### saving result
write.csv(error_single_OLS_cluster_1,"error_result_single_OLS_cluster_1.csv")
write.csv(error_single_OLS_cluster_2,"error_result_single_OLS_cluster_2.csv")
write.csv(error_single_OLS_cluster_3,"error_result_single_OLS_cluster_3.csv")
write.csv(error_single_OLS_cluster_4,"error_result_single_OLS_cluster_4.csv")
