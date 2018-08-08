error_result_heirarchy<-read.csv("error_result_heirarchy.csv",header = TRUE)
error_result_multiple_OLS<-read.csv("error_result_multiple_OLS.csv",header = TRUE)

############### computing RMSEs for all the series
mean_error_heirarchy<-apply(error_result_heirarchy,2,mean) # applies function 'RMSE' to 2nd dimension (columns)
RMSE_heirarchy<-as.data.frame(log(sqrt((mean_error_heirarchy)^2)))
RMSE_heirarchy$group<-"Heirarchy"
names(RMSE_heirarchy)<-c("RMSE","Method")
ave_RMSE_heirarchy<-mean(as.vector(RMSE_heirarchy))


mean_error_multiple_OLS<-apply(error_result_multiple_OLS,2,mean) # applies function 'RMSE' to 2nd dimension (columns)
RMSE_multiple_OLS<-as.data.frame(log(sqrt(mean_error_multiple_OLS^2)))
RMSE_multiple_OLS$group<-"OLS"
names(RMSE_multiple_OLS)<-c("RMSE","Method")
ave_RMSE_multiple_OLS<-mean(as.vector(RMSE_multiple_OLS))

###### box_plot
Long_RMSE<-rbind(RMSE_heirarchy,RMSE_multiple_OLS)
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

##### t-test
mean_test_difference<-t.test(RMSE ~ Method, data = Long_RMSE)### p-value = 1.084e-11
##########################
#### forecast
#########################
forecast_result_heirarchy<-read.csv("forecast_result_heirarchy.csv",header = TRUE)
forecast_result_multiple_OLS<-read.csv("forecast_result_multiple_OLS.csv",header = TRUE)

long_forecast_heirarchy<-melt(forecast_result_heirarchy)
long_forecast_heirarchy$Method<-"heirarchy"
names(long_forecast_heirarchy)<-c("Group","Series","Method")
long_forecast_multiple_OLS<-melt(forecast_result_multiple_OLS)
long_forecast_multiple_OLS$Method<-"OLS"
names(long_forecast_multiple_OLS)<-c("Group","Series","Method")

long_forecast<-rbind.data.frame(long_forecast_heirarchy,long_forecast_multiple_OLS)

ggplot() +
  geom_line(
    data = long_forecast,
    aes(
      x = c(1:28),
      y = long_forecast$Series#,
      #group = long_forecast$Group,
      #color = long_forecast$Method
    ),
    size = 1
  ) +
  xlab("") + ylab("") +
  theme_gray() 
