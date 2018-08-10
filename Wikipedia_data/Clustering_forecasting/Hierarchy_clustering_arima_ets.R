
### NEED TO BE FIXED

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
cluster_1 <- read.csv("cluster_1.csv", header = TRUE)
cluster_2 <- read.csv("cluster_2.csv", header = TRUE)
cluster_3 <- read.csv("cluster_3.csv", header = TRUE)
cluster_4 <- read.csv("cluster_4.csv", header = TRUE)
cluster_1_2 <- list(cluster_1, cluster_2)
cluster_3_4 <- list(cluster_3, cluster_4)

forecast_hierarchy_cluster_1_2 <- function(df) {
  df_wide <-
    matrix(df$views, nrow = 394, ncol = length(unique(df$cat_column)))
  names <- as.vector(unique(df$cat_column))
  name_length <- str_length(names)
  grouping_hts <- rbind(
    ## language
    str_sub(names, start = name_length - 6, end = name_length - 5),
    ## Purpose
    str_sub(names, start = name_length - 4, end = name_length - 2)
  )
  total_wikipedia_df <-
    ts(df_wide,
       frequency = 7,
       start = c(1, 1),
       c(1, 394))
  validation_wikipedia_df <-
    window(total_wikipedia_df,
           start = c(1, 367),
           end = c(1, 394))
  error_forecast_hierarchy_arima_ets <- function(total_wikipedia_df) {
  k <-
    28 #### rolling-window width
  n <- nrow(total_wikipedia_df)
  forecast_arima <- list()
  forecast_ets <- list()
  for (i in 1:k) {
    ### training set
    training_wikipedia_df  <-
      window(total_wikipedia_df,
             start = c(1, 1),
             end = c(1, (n - k) + (i - 1)))
    ### validation set
    validation_wikipedia_df <-
      window(total_wikipedia_df,
             start = c(1, (n - k) + i),
             end = c(1, (n - k) + i))
    wikipedia_hts_df <-
      gts(training_wikipedia_df, groups = grouping_hts)
    ### running the forecast
    forecast_arima_df <-
      forecast.gts(
        wikipedia_hts_df,
        fmethod = "arima",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    forecast_ets_df <-
      forecast.gts(
        wikipedia_hts_df,
        fmethod = "ets",
        h = 1,
        method = "comb",
        weights = "sd"
      )
    forecast_arima[[length(forecast_arima) + 1]] = as.data.frame(forecast_arima_df$bts)
    forecast_ets[[length(forecast_ets) + 1]] = as.data.frame(forecast_ets_df$bts)
  }
  return(list(forecast_arima,forecast_ets))
  }
  return(error_forecast_hierarchy_arima_ets(total_wikipedia_df))
}

final_hierarchy_arima_ets <-
  lapply(cluster_1_2, forecast_hierarchy_cluster_1_2)
#### arima:Time difference of 1.314881 hours
#### ets: Time difference of 49.89878 mins