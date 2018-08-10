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
################
#### Reading data
cluster_1 <-read.csv("cluster_1.csv", header = TRUE)
cluster_2 <-read.csv("cluster_2.csv", header = TRUE)
cluster_3 <-read.csv("cluster_3.csv", header = TRUE)
cluster_4 <-read.csv("cluster_4.csv", header = TRUE)
#### adding cluster nomber at the end of category names for hierarchy process
cluster_1$cat_column<-paste0(cluster_1$cat_column,"cluster01")
cluster_2$cat_column<-paste0(cluster_2$cat_column,"cluster02")
cluster_3$cat_column<-paste0(cluster_3$cat_column,"cluster03")
cluster_4$cat_column<-paste0(cluster_4$cat_column,"cluster04")
wikipedia_data<-rbind(cluster_1,cluster_2,cluster_3,cluster_4)
##Data reshaping
wikipedia_reshape<-as.data.frame(matrix(wikipedia_data$views,nrow = 394,ncol =913 ))
names <- as.vector(unique(wikipedia_data$cat_column))
names(wikipedia_reshape) <- c(names)
## using heirachies and groupings
name_length <- str_length(names)
grouping_hts <- rbind(
  #cluster group
  str_sub(names, start = name_length -8 , end = name_length),
  #agent
  str_sub(names, start = name_length - 17, end = name_length - 16),
  #access
  str_sub(names, start = name_length - 24, end = name_length - 18),
  #language
  str_sub(names, start = name_length - 15, end = name_length - 14),
  #purpose
  str_sub(names, start = name_length - 13, end = name_length - 11)
  )
wikipedia_total <-
  ts(wikipedia_reshape,
     frequency = 7,
     start = c(1, 1),
     c(1, 394))
wikipedia_validation<-
  window(wikipedia_total, start = c(1, 367), end = c(1, 394))

error_forecast_hierarchy_cluster_group_arima_ets <- function(wikipedia_total) {
  k <-
    28 #### rolling-window width
  n <- nrow(wikipedia_total)
  forecast_arima <- list()
  forecast_ets <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia  <-
      window(wikipedia_total,
             start = c(1, 1),
             end = c(1, (n - k) + (i - 1)))
    ### validation set
    validation_wikipedia<-
      window(wikipedia_total,
             start = c(1, (n - k) + i),
             end = c(1, (n - k) + i))
    wikipedia_hts <-
      gts(training_wikipedia, groups = grouping_hts)
    ### running the forecast
    forecast_arima_gts<-
      forecast.gts(
        wikipedia_hts,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    forecast_ets_gts <-
      forecast.gts(
        wikipedia_hts,
        fmethod = "ets",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    forecast_arima[[length(forecast_arima) + 1]] = as.data.frame(forecast_arima_gts$bts)
    forecast_ets[[length(forecast_ets) + 1]] = as.data.frame(forecast_ets_gts$bts)
  }
  return(list(forecast_arima,forecast_ets))
}
start.time <- Sys.time()
result_fore_error_cluster_group<-error_forecast_hierarchy_cluster_group_arima_ets(wikipedia_total)
end.time <- Sys.time()
end.time - start.time

