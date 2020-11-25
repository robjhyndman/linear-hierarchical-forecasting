# library(hts)
# library(Matrix)
# library(reshape)

## To avoid memory error in rolling origin forecast, ets and arima should be run separately and the we combine them in one single file 

library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)
library(lubridate)

# Convert two different date formats
make_date <- function(x) {
  numeric_dates <- !str_detect(x, "\\/")
  output <- Date(length = length(x))
  output[numeric_dates] <- as.Date(as.numeric(x[numeric_dates]), origin = "1899-12-30")
  output[!numeric_dates] <- mdy(x[!numeric_dates])
  return(output)
}

wikigts <- read_csv("wikipedia_data.csv") %>%
  select(date, views, cat_column) %>%
  mutate(date = make_date(date)) %>%
  group_by(date, cat_column) %>%
  summarise(views = mean(views)) %>%
  ungroup() %>%
  mutate(
    Access = str_sub(cat_column, 1, 7),
    Agent = str_sub(cat_column, 8, 9),
    Language = str_sub(cat_column, 10, 11),
    Purpose = str_sub(cat_column, 12, 14),
    Article = str_sub(cat_column, 15, 16),
  ) %>%
  as_tsibble(index = date, key = c(Access, Agent, Language, Purpose, Article)) %>%
  aggregate_key(
    Access + Agent + Language + Purpose +
      Access:Agent + Access:Language + Access:Purpose + Agent:Language + Agent:Purpose + Language:Purpose +
      Access:Agent:Language:Purpose:Article,
    views = sum(views)
  )





## Fixed origin forecasts
# fc.ets <- wiki %>%
#   filter(date <= "2017-06-01") %>%
#   model(ets = ETS(views)) 
# 
# fc.ets %>%
#   filter(
#     is_aggregated(Access),
#     is_aggregated(Agent),
#     is_aggregated(Language),
#     is_aggregated(Purpose),
#     is_aggregated(Article)
#   ) %>%
#   forecast(h = "28 days") %>%
#   autoplot(wiki)
# 
# fc.ets.reconciled <- fc.ets %>%
#   reconcile(ets_adjusted = min_trace(ets, method = "wls_struct")) %>%
#   forecast(h = "28 days") 
# 
# 
# fc.ets.reconciled %>%
#   filter(#.model == "ets",
#     is_aggregated(Access),
#     is_aggregated(Agent),
#     is_aggregated(Language),
#     is_aggregated(Purpose),
#     is_aggregated(Article)
#   ) %>%
#   autoplot(wiki)
# 
# fc.hilo <- fc.ets.reconciled %>% 
#   hilo(level = 95) %>%
#   unpack_hilo("95%")

## Fixed origin forecasts
new_data <- wikigts %>%
  dplyr::filter(date > ymd("2017-06-01")) %>%
  rename(actual = views)
#ETS
## computation time
start_time <- Sys.time()
fc.ets <- wikigts %>%
  filter(date <= ymd("2017-06-01")) %>%
  model(ets = ETS(views ))
end_time <- Sys.time()
end_time - start_time

fc.ets <- fc.ets %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "28 days") 

fc.ets.error <- fc.ets %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.ets <- fc.ets.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

#ARIMA
## computation time
start_time <- Sys.time()
fc.arima <- wikigts %>%
  filter(date <= ymd("2017-06-01")) %>%
  model(arima = ARIMA(views ))
end_time <- Sys.time()
end_time - start_time

fc.arima <- fc.arima %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "28 days") 

fc.arima.error <- fc.arima %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.arima <- fc.arima.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

fc.fix <- bind_rows (fc.arima, fc.ets) %>% 
  distinct(across(-views))

write_csv(fc.fix, 'fc.fix.wiki.ets.arima.test.csv')

### Rolling origin forecasts

gts.rolling <- wikigts %>%
  filter(date < ymd("2017-06-29"))# %>%

gts.rolling2 <- gts.rolling %>%
  tsibble::stretch_tsibble(.init = 366 , .step = 1)

new_data <- wikigts %>%
  dplyr::filter(date >= ymd("2017-06-02")) %>%
  rename(actual = views)%>% 
  arrange(`date`) %>%
  mutate(new_index = dense_rank(date))

# ETS
## computation time
start_time <- Sys.time()
fc.ets <- gts.rolling2 %>%
  model(ets = ETS(views))
end_time <- Sys.time()
end_time - start_time

m <- c(1:28)
fc.ets.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.ets %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  
  result <- result %>% 
    distinct(across(-views)) %>%
    mutate('h' = i)
  new_data2 <- new_data %>% 
    filter(new_index == i)
  
  fc.rec <- result %>%
    left_join(new_data2) %>%
    mutate(error = actual - .mean)
  
  fc.ets.rec <- bind_rows(fc.ets.rec, fc.rec)
}


# ARIMA
## computation time
start_time <- Sys.time()
fc.arima <- gts.rolling2 %>%
  model(arima = ARIMA(views))
end_time <- Sys.time()
end_time - start_time

m <- c(1:28)
fc.arima.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.arima %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  
  result <- result %>% 
    distinct(across(-views)) %>%
    mutate('h' = i)
  new_data2 <- new_data %>% 
    filter(new_index == i)
  
  fc.rec <- result %>%
    left_join(new_data2) %>%
    mutate(error = actual - .mean)
  
  fc.arima.rec <- bind_rows(fc.arima.rec, fc.rec)
}


fc.rolling <- bind_rows(fc.ets.rec, fc.arima.rec)

write_csv(fc.rolling, 'fc.rolling.wiki.ets.arima.csv')
