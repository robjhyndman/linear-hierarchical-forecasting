
library(fable)
library(fabletools)
library(tidyverse)
library(tsibble)

aus <- read.csv('TourismData_v3.csv', header = TRUE)[,-c(1,2)]
aus <-  tibble(aus)
aus$Date <-  rep(yearmonth("1998 Jan") + 0:227)

aus <- aus %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    State = stringr::str_sub(group, 1, 1),
    Zone = stringr::str_sub(group, 1, 2),
    Region = stringr::str_sub(group, 1, 3),
    Purpose = stringr::str_sub(group, 4, 6),
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(State, Zone, Region, Purpose))


ausgts <- aus %>%
  aggregate_key(Purpose * (State/ Zone/ Region), value = sum(value)) 

## fixed
#ETS
fc.ets <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "2 years") #%>%
 #mutate(value = distributional::dist_truncated(value, 0)) 

fc.ets<- fc.ets%>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

#ARIMA
fc.arima <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) %>%
  model(arima = ARIMA(value ))%>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "2 years") #%>%
#mutate(value = distributional::dist_truncated(value, 0)) 

fc.arima<- fc.arima %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")


### Rolling origin forecasts

gts.rolling <- ausgts %>%
  filter(Date < yearmonth ("2016 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 204 , .step = 1)

# ETS
fc.ets <- gts.rolling %>%
  model(ets = ETS(value))

m <- c(1:24)
fc.ets.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.ets %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  rec.res <- cbind.data.frame(result$key, result$index, result$.model, result$.mean, result$`95%_lower`, result$`95%_upper`, 'h' = i)
  fc.ets.rec <- bind_rows(fc.ets.rec, rec.res)
}

# ARIMA
fc.arima <- gts.rolling %>%
  model(arima = ARIMA(value))

m <- c(1:24)
fc.arima.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.arima %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  rec.res <- cbind.data.frame(result$key, result$index, result$.model, result$.mean, result$`95%_lower`, result$`95%_upper`, 'h' = i)
  fc.arima.rec <- bind_rows(fc.arima.rec, rec.res)
}




