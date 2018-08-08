setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/
  Wikipedia_data/Clustering_forecasting/hierarchy_clustering"
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
cluster_2<-read.csv("cluster_2.csv",header=TRUE)

cluster_2_s<- cbind.data.frame(
  views = cluster_2$views
  ,
  category = cluster_2$cat_column,
  split = cluster_2$Split,
  date = cluster_2$date
)

cluster_2_reshape <-
  reshape(
    cluster_2_s,
    idvar = c("date", "split"),
    timevar = "category",
    direction = "wide"
  )
names<- as.vector(unique(cluster_2$cat_column))
names(cluster_2_reshape) <- c("split", "date", names)

name_length <- str_length(names)
grouping_hts <- rbind(
  ## language
  str_sub(names, start = name_length - 6, end = name_length - 5),
  ## Purpose
  str_sub(names, start = name_length - 4, end = name_length - 2)#,
  #str_sub(names, start = name_length - 1, end = name_length)
)

total_wikipedia_cluster_2 <-
  ts(cluster_2_reshape[,-c(1, 2)],
     frequency = 7,
     start = c(1, 1),
     c(1, 394))
#training_wikipedia <-
#window(total_wikipedia_cluster_2,
# start = c(1, 1),
# end = c(1, 366))
validation_wikipedia_cluster_2 <-
  window(total_wikipedia_cluster_2,
         start = c(1, 367),
         end = c(1, 394))
#g_wikipedia_hts <- gts(training_wikipedia, groups = grouping_hts)


error_forecast_hierarchy_arima <- function(total_wikipedia_cluster_2) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  n <- nrow(total_wikipedia_cluster_2)
  result <- list()
  error <- list()
  total_forecast <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia_cluster_2  <-
      window(total_wikipedia_cluster_2,
             start = c(1, 1),
             end = c(1, (n - k)+(i-1)))
    ### validation set
    validation_wikipedia_cluster_2 <-
      window(total_wikipedia_cluster_2,
             start = c(1, (n - k) +i),
             end = c(1, (n - k) + i))
    g_wikipedia_hts_cluster_2 <-
      gts(training_wikipedia_cluster_2, groups = grouping_hts)
    ### running the forecast
    fore_g_wikipedia_hts_cluster_2 <-
      forecast.gts(
        g_wikipedia_hts_cluster_2,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    total_forecast[[length(total_forecast) + 1]] = fore_g_wikipedia_hts_cluster_2
    result[[length(result) + 1]] = as.data.frame(fore_g_wikipedia_hts_cluster_2$bts)
    #result[i,] <- as.data.frame(fore_g_wikipedia_hts$bts)
    
  }
  return(list(result, total_forecast))
}

start.time <- Sys.time()
final_result_hierarchy_arima_cluster_2 <-
  error_forecast_hierarchy_arima(total_wikipedia_cluster_2)#### arima: Time difference of 1.167502 hours 
                                                          #### ets: Time difference of 1.14457 hours
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### saving hierarchy forecast and error results
forecast_result_hierarchy_arima_cluster_2 <-
  do.call("rbind", final_result_hierarchy_arima_cluster_2[[1]])
write.csv(forecast_result_hierarchy_arima_cluster_2, "forecast_result_hierarchy_arima_cluster_2.csv")
error_result_hierarchy_arima_cluster_2 <-
  as.data.frame(validation_wikipedia_cluster_2) - forecast_result_hierarchy_arima_cluster_2
write.csv(error_result_hierarchy_arima_cluster_2, "error_result_hierarchy_arima_cluster_2.csv")
