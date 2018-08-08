setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Wikipedia_data/Clustering_forecasting/Heirarchy_clustering_arima_ets"
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
cluster_4<-read.csv("cluster_4.csv",header=TRUE)

cluster_4_s<- cbind.data.frame(
  views = cluster_4$views
  ,
  category = cluster_4$cat_column,
  split = cluster_4$Split,
  date = cluster_4$date
)

cluster_4_reshape <-
  reshape(
    cluster_4_s,
    idvar = c("date", "split"),
    timevar = "category",
    direction = "wide"
  )
names<- as.vector(unique(cluster_4$cat_column))
names(cluster_4_reshape) <- c("split", "date", names)

name_length <- str_length(names)
grouping_hts <- rbind(
  ## agent
  str_sub(names, start = name_length - 8, end = name_length - 7),
  ## Purpose
  str_sub(names, start = name_length - 4, end = name_length - 2)#,
  #str_sub(names, start = name_length - 1, end = name_length)
)

total_wikipedia_cluster_4 <-
  ts(cluster_4_reshape[,-c(1, 2)],
     frequency = 7,
     start = c(1, 1),
     c(1, 394))
#training_wikipedia <-
#window(total_wikipedia_cluster_4,
# start = c(1, 1),
# end = c(1, 366))
validation_wikipedia_cluster_4 <-
  window(total_wikipedia_cluster_4,
         start = c(1, 367),
         end = c(1, 394))
#g_wikipedia_hts <- gts(training_wikipedia, groups = grouping_hts)


error_forecast_heirarchy_arima <- function(total_wikipedia_cluster_4) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  n <- nrow(total_wikipedia_cluster_4)
  result <- list()
  error <- list()
  total_forecast <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia_cluster_4  <-
      window(total_wikipedia_cluster_4,
             start = c(1, 1),
             end = c(1, (n - k)+(i-1)))
    ### validation set
    validation_wikipedia_cluster_4 <-
      window(total_wikipedia_cluster_4,
             start = c(1, (n - k) +i),
             end = c(1, (n - k) + i))
    g_wikipedia_hts_cluster_4 <-
      gts(training_wikipedia_cluster_4, groups = grouping_hts)
    ### running the forecast
    fore_g_wikipedia_hts_cluster_4 <-
      forecast.gts(
        g_wikipedia_hts_cluster_4,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    total_forecast[[length(total_forecast) + 1]] = fore_g_wikipedia_hts_cluster_4
    result[[length(result) + 1]] = as.data.frame(fore_g_wikipedia_hts_cluster_4$bts)
    #result[i,] <- as.data.frame(fore_g_wikipedia_hts$bts)
    
  }
  return(list(result, total_forecast))
}

start.time <- Sys.time()
final_result_heirarchy_arima_cluster_4 <-
  error_forecast_heirarchy_arima(total_wikipedia_cluster_4)#### arima: Time difference of 12.70374 mins
                                                           ### ets: Time difference of 28.03559 mins
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### saving heirarchy forecast and error results
forecast_result_heirarchy_arima_cluster_4 <-
  do.call("rbind", final_result_heirarchy_arima_cluster_4[[1]])
write.csv(forecast_result_heirarchy_arima_cluster_4, "forecast_result_heirarchy_arima_cluster_4.csv")
error_result_heirarchy_arima_cluster_4 <-
  as.data.frame(validation_wikipedia_cluster_4) - forecast_result_heirarchy_arima_cluster_4
write.csv(error_result_heirarchy_arima_cluster_4, "error_result_heirarchy_arima_cluster_4.csv")
