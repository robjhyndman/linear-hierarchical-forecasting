
library(fable)
library(fabletools)
library(tsibble)
library(tidyverse)
### 
actual.sim <- read.csv('sim.1520.csv', header = TRUE)

## 8 levels
actual.sim$cat <- paste0(actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 10 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 12 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$level2
                          , actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

## 18 levels
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$level2
                          , actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose, actual.sim$level3)

actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 1520) %>%
  as.data.frame()
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names

actual.05 <-  tibble(actual.05)
actual.05$Date <-  rep(yearmonth("1998 Jan") + 0:227)
### Defining hierarchy structure
## 8 levels
actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    State = stringr::str_sub(group, 1, 1),
    Zone = stringr::str_sub(group, 1, 2),
    Region = stringr::str_sub(group, 1, 3),
    Purpose = stringr::str_sub(group, 4, 7)
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(State, Zone, Region, Purpose))

actual.05gts <- actual.05 %>%
  aggregate_key( Purpose * (State/ Zone/ Region), value = sum(value))

## 10 levels
actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    level1 = stringr::str_sub(group, 1, 1),
    State = stringr::str_sub(group, 1, 2),
    Zone = stringr::str_sub(group, 1, 3),
    Region = stringr::str_sub(group, 1, 4),
    Purpose = stringr::str_sub(group, 5, 8)
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(level1,  State, Zone, Region, Purpose))

actual.05gts <- actual.05 %>%
  aggregate_key( Purpose * (level1/ State/ Zone/ Region), value = sum(value))

## 12 levels
actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    level1 = stringr::str_sub(group, 1, 1),
    level2 = stringr::str_sub(group, 1, 2),
    State = stringr::str_sub(group, 1, 3),
    Zone = stringr::str_sub(group, 1, 4),
    Region = stringr::str_sub(group, 1, 5),
    Purpose = stringr::str_sub(group, 6, 9)
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(level1, level2, State, Zone, Region, Purpose))

actual.05gts <- actual.05 %>%
  aggregate_key( Purpose * (level1/ level2/ State/ Zone/ Region), value = sum(value))

## 18 levels

actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    level1 = stringr::str_sub(group, 1, 1),
    level2 = stringr::str_sub(group, 1, 2),
    State = stringr::str_sub(group, 1, 3),
    Zone = stringr::str_sub(group, 1, 4),
    Region = stringr::str_sub(group, 1, 5),
    Purpose = stringr::str_sub(group, 6, 9),
    level3 = stringr::str_sub(group, 10, 11)
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(level1, level2, State, Zone, Region, Purpose, level3))

actual.05gts <- actual.05 %>%
  aggregate_key( (level1 / level2/ State/ Zone / Region) + Purpose + level3 + level1:Purpose
                 + level2:Purpose + State:Purpose + Zone:Purpose + Purpose:level3 + level1:level3
                 + level2:level3 + State:level3 + Zone:level3
                 + Purpose:level1:level2:level3:State:Zone:Region , value = sum(value))

### Fixed origin forecasts

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2014 Dec")) %>%
  rename(actual = value)

## computation time
start_time <- Sys.time()
fc.ets <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(ets = ETS(value)) 
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

## computation time
start_time <- Sys.time()
fc.arima <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(arima = ARIMA(value)) 
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


fc.fixsim15208 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.fixsim152010 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.fixsim152012 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.fixsim152018 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))

write_csv(fc.fixsim15208, 'fc.fix.sim.ets.arima.levelNS15208.csv')
write_csv(fc.fixsim152010, 'fc.fix.sim.ets.arima.levelNS152010.csv')
write_csv(fc.fixsim152012, 'fc.fix.sim.ets.arima.levelNS152012.csv')
write_csv(fc.fixsim152018, 'fc.fix.sim.ets.arima.levelNS152018.csv')


### Rolling origin forecasts
# ETS
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2014 Dec"))

gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2014 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))

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
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2014 Dec"))

gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2014 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))

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

fc.rollingsim15208 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.rollingsim152010 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.rollingsim152012 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))
fc.rollingsim152018 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))

write_csv(fc.rollingsim15208, 'fc.rolling.sim.ets.arima.levelNS15208.csv')
write_csv(fc.rollingsim152010, 'fc.rolling.sim.ets.arima.levelNS152010.csv')
write_csv(fc.rollingsim152012, 'fc.rolling.sim.ets.arima.levelNS152012.csv')
write_csv(fc.rollingsim152018, 'fc.rolling.sim.ets.arima.levelNS152018.csv')