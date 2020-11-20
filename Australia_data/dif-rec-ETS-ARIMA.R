
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

new_data <- ausgts %>%
  dplyr::filter(Date > yearmonth ("2014 Dec")) %>%
  rename(actual = value)



## fixed origin forecast
#ETS
fc.ets <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) 

### mint_shrink reconciliation
fc.ets.mint.shrink <- fc.ets %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="mint_shrink"))%>%
  forecast(h = "2 years") 

fc.ets.mint.shrink.error <- fc.ets.mint.shrink %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.ets.mint.shrink <- fc.ets.mint.shrink.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

### wls_var reconciliation
fc.ets.wls.var <- fc.ets %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_var"))%>%
  forecast(h = "2 years") 

fc.ets.wls.var.error <- fc.ets.wls.var %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.ets.wls.var <- fc.ets.wls.var.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

#ARIMA
fc.arima <- ausgts %>%
  filter(Date <= yearmonth ("2014 Dec")) %>%
  model(arima = ARIMA(value ))
### mint_shrink reconciliation
fc.arima.mint.shrink <- fc.arima %>%
  reconcile(arima_adjusted = min_trace(arima, method="mint_shrink"))%>%
  forecast(h = "2 years") 

fc.arima.mint.shrink.error <- fc.arima.mint.shrink %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.arima.mint.shrink <- fc.arima.mint.shrink.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

### wls_var reconciliation
fc.arima.wls.var <- fc.arima %>%
  model(arima = ARIMA(value ))%>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_var"))%>%
  forecast(h = "2 years") 

fc.arima.wls.var.error <- fc.arima.wls.var %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.arima.wls.var <- fc.arima.wls.var.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")
## mint_shrink results
fc.fix.mint.shrink <- bind_rows (fc.arima.mint.shrink, fc.ets.mint.shrink) %>% 
  distinct(across(-value))

write_csv(fc.fix.mint.shrink, 'fc.fix.ets.arima.mint.shrink.csv')
## wls_var results
fc.fix.wls.var <- bind_rows (fc.arima.wls.var, fc.ets.wls.var) %>% 
  distinct(across(-value))

write_csv(fc.fix.wls.var, 'fc.fix.ets.arima.wls.var.csv.csv')

### Rolling origin forecasts

gts.rolling <- ausgts %>%
  filter(Date < yearmonth ("2016 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 204 , .step = 1)

new_data <- ausgts %>%
  dplyr::filter(Date > yearmonth ("2014 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))

# ETS
fc.ets.mint.shrink <- gts.rolling %>%
  model(ets = ETS(value))
## mint_shrink
m <- c(1:24)
fc.ets.mint.shrink.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.ets.mint.shrink %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method="mint_shrink")) %>%
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
  
  fc.ets.mint.shrink.rec <- bind_rows(fc.ets.mint.shrink.rec, fc.rec)
}

## wls_var
m <- c(1:24)
fc.ets.wls.var.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.ets.wls.var %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method="mint_shrink")) %>%
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
  
  fc.ets.wls.var.rec <- bind_rows(fc.ets.wls.var.rec, fc.rec)
}
# ARIMA
fc.arima.mint.shrink <- gts.rolling %>%
  model(arima = ARIMA(value))
## mint_shrink
m <- c(1:24)
fc.arima.mint.shrink.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.arima.mint.shrink %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method="mint_shrink")) %>%
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
  
  fc.arima.mint.shrink.rec <- bind_rows(fc.arima.mint.shrink.rec, fc.rec)
}

## wls_var

m <- c(1:24)
fc.arima.wls.var.rec <- data.frame(a=c(), b=c())
for(i in m){
  result <- fc.arima.wls.var %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method="mint_shrink")) %>%
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
  
  fc.arima.wls.var.rec <- bind_rows(fc.arima.wls.var.rec, fc.rec)
}

## saving results
fc.rolling.mint.shrink <- bind_rows (fc.arima.mint.shrink.rec, fc.ets.mint.shrink.rec)

write_csv(fc.rolling.mint.shrink, 'fc.rolling.mint.shrink.ets.arima.csv')

fc.rolling.wls.var <- bind_rows (fc.arima.wls.var.rec, fc.ets.wls.var.rec)

write_csv(fc.rolling.wls.var, 'fc.rolling.wls.var.ets.arima.csv')


