setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Australia_data"
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
    "stringr",
    "hts",
    "reshape",
    "reshape2",
    "plyr",
    "sandwich"
  ),
  require,
  character.only = T
)

######################
###### Reading data and adding separate category columns
######################
Australia_long <- read.csv("Long_TourismData.csv", header = TRUE) ### Season: 12 lags seasonality, Seasonality: 4 lags seasonality
category_unique <- as.vector(Australia_long$Category)
Australia_long$State <- substring(category_unique, 1, 1)
Australia_long$Zone <- substring(category_unique, 1, 2)
Australia_long$Regions <- substring(category_unique, 1, 3)
Australia_long$Purpose <- substring(category_unique, 4, 6)
###################### Spliting data in Training and validation sets
Training_Australia <-
  subset.data.frame(Australia_long, Australia_long$Split == "training")
Validation_Australia <-
  subset.data.frame(Australia_long, Australia_long$Split == "validation")
#################
####### Clustering process on training set
#################
#### outlier detection
splitData = split(Training_Australia, Training_Australia$Category)
outlier_detection = function(df) {
  df$Value_out <- round(tsclean(df$Value))
  return(df)
  
}
new_data_value = lapply(splitData, outlier_detection)
Training_Australia <-
  Reduce(function(x, y)
    rbind.data.frame(x, y), new_data_value)
######### Scaling data & storing mean values
Training_Australia <-
  ddply(Training_Australia,
        "Category",
        transform,
        Value.std = scale(Value_out))
Training_Australia$mean.values <-
  ave(Training_Australia$Value_out, Training_Australia$Category)
##############
##### Single Step method
##############
### Running mob
Training_Australia$Seasonality <- as.factor(Training_Australia$Seasonality)
Training_Australia$State <- as.factor(Training_Australia$State)
Training_Australia$Zone <- as.factor(Training_Australia$Zone)
Training_Australia$Regions <- as.factor(Training_Australia$Regions)
Training_Australia$Purpose <- as.factor(Training_Australia$Purpose)
Training_Australia$Trend <- as.numeric(Training_Australia$Trend)

category_sort <- sort(unique(Training_Australia$Category))

split_data = split(Training_Australia, Training_Australia$Category)

lag_making <- list()
for (i in seq_along(category_sort))
  lag_making[[i]] <-
  Lag(Training_Australia$Value.std[Training_Australia$Category == category_sort[i]], 1:4)
lag_making <- do.call(rbind, lag_making)
#### sorting dataset based on the category column since lagged columns are base on sorted data
Training_Australia <-
  Training_Australia[order(Training_Australia$Category), ]
### final data with error lags
Training_Australia <- cbind(Training_Australia, lag_making)
Training_Australia <- na.omit(Training_Australia)
### linear formula for running MOB
formula_single_step <-
  Value.std ~ Trend + Seasonality + Lag.1 + Lag.2 + Lag.3 + Lag.4|
  State + Zone +  Purpose #+ Regions
### defining fit function
linear <-
  function(y,
           x,
           start = NULL,
           weights = NULL,
           offset = NULL,
           ...) {
    lm(y ~ 0 + x,  ...)
  }
### running tree
tree_single_step <-
  mob(
    formula_single_step,
    data = Training_Australia,
    fit = linear,
    control = mob_control(
      prune = "AIC",
      alpha = 0.01,
      maxdepth = 3
    )
  )
pdf("final_tree.pdf", width = 70, height = 30)
plot(tree_single_step)
dev.off()
tree_first_step

### error finding
Training_Australia$pred_single_step <-
  predict(tree_single_step, type = "response")
Training_Australia$error_single_step <-
  Training_Australia$Value.std - Training_Australia$pred_single_step
split_first <-
  split(Training_Australia, predict(tree_single_step, type = "node"))
Node_number_single_step <-
  c("Node_1", "Node_2", "Node_3", "Node_4")
final_first <-
  Map(cbind, split_first, Node_number_single_step = Node_number_single_step)
Training_Australia <- do.call(rbind, final_first)
split_data_single_step = split(Training_Australia,
                               Training_Australia$Node_number_single_step)
### finding MSE for each node_first_step
MSE_single_step_separate_Nodes <-
  lapply(split_data_single_step, function(df)
    mean((df$error_single_step) ^ 2))
MSE_single_step_separate_Nodes
MSE_total_first <-
  mean((Training_Australia$error_single_step) ^ 2)
MSE_total_first

