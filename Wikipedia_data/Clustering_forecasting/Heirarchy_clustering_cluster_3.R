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
cluster_3<-read.csv("cluster_3.csv",header=TRUE)

cluster_3_s<- cbind.data.frame(
  views = cluster_3$views
  ,
  category = cluster_3$cat_column,
  split = cluster_3$Split,
  date = cluster_3$date
)

cluster_3_reshape<-as.data.frame(matrix(cluster_3_s$views,nrow = 394,ncol = 445))

names<- as.vector(unique(cluster_3$cat_column))
names(cluster_3_reshape) <- c(names)
name_length <- str_length(names)
grouping_hts <- rbind(
  ## agent
  str_sub(names, start = name_length - 8, end = name_length - 7),
  ## Purpose
  str_sub(names, start = name_length - 4, end = name_length - 2)#,
  #str_sub(names, start = name_length - 1, end = name_length)
)

total_wikipedia_cluster_3 <-
  ts(cluster_3_reshape[,-c(1, 2)],
     frequency = 7,
     start = c(1, 1),
     c(1, 394))
#training_wikipedia <-
#window(total_wikipedia_cluster_3,
# start = c(1, 1),
# end = c(1, 366))
validation_wikipedia_cluster_3 <-
  window(total_wikipedia_cluster_3,
         start = c(1, 367),
         end = c(1, 394))
#g_wikipedia_hts <- gts(training_wikipedia, groups = grouping_hts)


error_forecast_hierarchy_arima <- function(total_wikipedia_cluster_3) {
  k <-
    28 #### number of points that we want to forecast finally (rolling)
  n <- nrow(total_wikipedia_cluster_3)
  result <- list()
  error <- list()
  total_forecast <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia_cluster_3  <-
      window(total_wikipedia_cluster_3,
             start = c(1, 1),
             end = c(1, (n - k)+(i-1)))
    ### validation set
    validation_wikipedia_cluster_3 <-
      window(total_wikipedia_cluster_3,
             start = c(1, (n - k) +i),
             end = c(1, (n - k) + i))
    g_wikipedia_hts_cluster_3 <-
      gts(training_wikipedia_cluster_3, groups = grouping_hts)
    ### running the forecast
    fore_g_wikipedia_hts_cluster_3 <-
      forecast.gts(
        g_wikipedia_hts_cluster_3,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    total_forecast[[length(total_forecast) + 1]] = fore_g_wikipedia_hts_cluster_3
    result[[length(result) + 1]] = as.data.frame(fore_g_wikipedia_hts_cluster_3$bts)
    #result[i,] <- as.data.frame(fore_g_wikipedia_hts$bts)
    
  }
  return(list(result, total_forecast))
}

start.time <- Sys.time()
final_result_hierarchy_arima_cluster_3 <-
  error_forecast_hierarchy_arima(total_wikipedia_cluster_3)#### arima: Time difference of 1.581321 hours
                                                           ### ets: Time difference of 3.234194 hours
                                                        
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### saving hierarchy forecast and error results
forecast_result_hierarchy_arima_cluster_3 <-
  do.call("rbind", final_result_hierarchy_arima_cluster_3[[1]])
write.csv(forecast_result_hierarchy_arima_cluster_3, "forecast_result_hierarchy_arima_cluster_3.csv")
error_result_hierarchy_arima_cluster_3 <-
  as.data.frame(validation_wikipedia_cluster_3) - forecast_result_hierarchy_arima_cluster_3
write.csv(error_result_hierarchy_arima_cluster_3, "error_result_hierarchy_arima_cluster_3.csv")


