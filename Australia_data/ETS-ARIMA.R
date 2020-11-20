
library(fable)
library(fabletools)
library(tidyverse)
library(tsibble)
library(readr)

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

new_data <- ausgts %>%
  dplyr::filter(Date > yearmonth ("2014 Dec")) %>%
  rename(actual = value)
#########
## Fixed origin 
########
#ETS
## computation time
start_time <- Sys.time()
fc.ets <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) %>%
  model(ets = ETS(value ))
end_time <- Sys.time()
end_time - start_time

fc.ets <- fc.ets %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "2 years") 

fc.ets.error <- fc.ets %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.ets <- fc.ets.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

#ARIMA
## computation time
start_time <- Sys.time()
fc.arima <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) %>%
  model(arima = ARIMA(value ))
end_time <- Sys.time()
end_time - start_time

fc.arima <- fc.arima %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "2 years") 

fc.arima.error <- fc.arima %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.arima <- fc.arima.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")


## saving  ets and arima results

fc.fix <- bind_rows (fc.arima, fc.ets)
fc.fix <- fc.fix %>% 
  distinct(across(-value))
write_csv(fc.fix, 'fc.fix.tourism.ets.arima.csv')


#########
## Rolling origin 
########

## rolling window
gts.rolling <- ausgts %>%
  filter(Date < yearmonth ("2016 Dec")) %>%
  stretch_tsibble(.init = 204 , .step = 1)

new_data <- ausgts %>%
  dplyr::filter(Date > yearmonth ("2014 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))

# ETS
## computation time
start_time <- Sys.time()
fc.ets <- gts.rolling %>%
  model(ets = ETS(value))
end_time <- Sys.time()
end_time - start_time

m <- c(1:24)
fc.ets.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.ets %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  
  result <- result %>% 
    distinct(across(-value)) %>%
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
fc.arima <- gts.rolling %>%
  model(arima = ARIMA(value))
end_time <- Sys.time()
end_time - start_time

m <- c(1:24)
fc.arima.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.arima %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method="wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level=95)%>%
    unpack_hilo("95%") 
  
  result <- result %>% 
    distinct(across(-value)) %>%
    mutate('h' = i)
  new_data2 <- new_data %>% 
    filter(new_index == i)
  
  fc.rec <- result %>%
    left_join(new_data2) %>%
    mutate(error = actual - .mean)
  
  fc.arima.rec <- bind_rows(fc.arima.rec, fc.rec)
}

fc.rolling <- bind_rows (fc.arima.rec, fc.ets.rec)

write_csv(fc.rolling, 'fc.rolling.tourism.ets.arima.csv')
