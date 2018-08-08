#################################
##Plotting and testing "General_forecasting" results
################################
setwd(
  "G:/My Drive/A New Tree based Method for Clustering Time Series/Forecasting paper_Rob/hierarchical-clustering-forecasting/Wikipedia_data/General_forecasting"
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
    "data.table",
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
###########
#### error
##########
#### reading data (arima, hierarchy+arima, hierarchy+ets, multiple OLS)
error_arima<-read.csv("error_result_arima.csv",header = TRUE)
error_hierarchy_arima<-read.csv("error_result_hierarchy_arima.csv",header = TRUE)
error_hierarchy_ets<-read.csv("error_result_hierarchy_ets.csv",header = TRUE)
error_multiple_OLS<-read.csv("error_result_multiple_OLS.csv",header = TRUE)
#### multiple OLS columns order is different from others -> change its order like other error files
setcolorder(error_multiple_OLS, names(error_arima))
#### computing log(RMSEs) for all the series
## arima
mean_error_arima<-apply(error_arima,2,mean) # applies function 'RMSE' to 2nd dimension (columns)
RMSE_arima<-as.data.frame(log(sqrt((mean_error_arima)^2)))
RMSE_arima$group<-"arima"
names(RMSE_arima)<-c("RMSE","Method")
ave_RMSE_arima<-mean(as.vector(RMSE_arima$RMSE)) ## -0.1029621
## hierarchy+arima
mean_error_hierarchy_arima<-apply(error_hierarchy_arima,2,mean) 
RMSE_hierarchy_arima<-as.data.frame(log(sqrt((mean_error_hierarchy_arima)^2)))
RMSE_hierarchy_arima$group<-"hierarchy-arima"
names(RMSE_hierarchy_arima)<-c("RMSE","Method")
ave_RMSE_hierarchy_arima<-mean(as.vector(RMSE_hierarchy_arima$RMSE)) ## -0.1449021
## hierarchy+ets
mean_error_hierarchy_ets<-apply(error_hierarchy_ets,2,mean) 
RMSE_hierarchy_ets<-as.data.frame(log(sqrt((mean_error_hierarchy_ets)^2)))
RMSE_hierarchy_ets$group<-"hierarchy-ets"
names(RMSE_hierarchy_ets)<-c("RMSE","Method")
ave_RMSE_hierarchy_ets<-mean(as.vector(RMSE_hierarchy_ets$RMSE)) ## -0.4092019
## multiple_OLS
mean_error_multiple_OLS<-apply(error_multiple_OLS,2,mean) 
RMSE_multiple_OLS<-as.data.frame(log(sqrt((mean_error_multiple_OLS)^2)))
RMSE_multiple_OLS$group<-"multiple_OLS"
names(RMSE_multiple_OLS)<-c("RMSE","Method")
ave_RMSE_multiple_OLS<-mean(as.vector(RMSE_multiple_OLS$RMSE)) ## 0.7602953
##########################################
###### box_plot
Long_RMSE<-rbind(RMSE_arima,RMSE_hierarchy_arima,RMSE_hierarchy_ets,RMSE_multiple_OLS)
ggplot() +
  geom_boxplot(data = Long_RMSE,
               aes(x = Method, y = (RMSE), fill = Method),
               alpha = 0.5) +
  xlab("Method") + ylab("log(RMSE)") +
  guides(fill = guide_legend(nrow = 1, bycol = TRUE)) +
  theme_bw() +
  theme(
    text  = element_text(size = 12),
    legend.direction = "horizontal",
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    panel.border = element_rect(colour = "black")
  )

#### histogram
ggplot(Long_RMSE, aes(RMSE)) +
  geom_histogram(fill = "white", color = "grey30")+ 
  facet_wrap(~ Method)+
  xlab("Method") + ylab("log(RMSE)")
#### density plot
ggplot() +
  geom_density(data = Long_RMSE, aes(x = (RMSE), fill = Method), alpha =
                 0.4) +
  xlab("Method") + ylab("Error") +
  #facet_wrap( ~ Method, ncol = 1) +
  guides(fill = guide_legend(nrow = 1, bycol = TRUE)) +
  theme_bw() + #xlim(c(-2, 2)) +
  theme(
    text  = element_text(size = 12),
    legend.direction = "horizontal",
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour = "white", fill = "white"),
    panel.border = element_rect(colour = "black")
  )
