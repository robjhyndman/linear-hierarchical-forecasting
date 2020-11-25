

library(fable)
library(fabletools)
library(tidyverse)
library(tsibble)

actual.sim <- read.csv('actual.sim.noise.FH.csv', header = TRUE)
## Choosing series with different added noises (0.01, 0.1, 0.5, 1)
actual.sim.001 <- actual.sim[actual.sim$Sim == 'sig0.01',]
actual.sim.01 <- actual.sim[actual.sim$Sim == 'sig0.1',]
actual.sim.05 <- actual.sim[actual.sim$Sim == 'sig0.5',]
actual.sim.1 <- actual.sim[actual.sim$Sim == 'sig1',]

actual.05 <- actual.sim.05$value %>%
  matrix(nrow = 228, ncol = 304) %>%
  as.data.frame() 
colnames(actual.05) <- unique(actual.sim.05$Var2)

actual.05 <-  tibble(actual.05)
actual.05$Date <-  rep(yearmonth("1998 Jan") + 0:227)


actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    State = stringr::str_sub(group, 1, 1),
    Zone = stringr::str_sub(group, 1, 2),
    Region = stringr::str_sub(group, 1, 3),
    Purpose = stringr::str_sub(group, 4, 6),
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(State, Zone, Region, Purpose))


actual.05gts <- actual.05 %>%
  aggregate_key(Purpose * (State/ Zone/ Region), value = sum(value)) 

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2014 Dec")) %>%
  rename(actual = value)


### fixed origin forecasts
## Change h based on the desired forecast horizon (1 year(12 months), 2 years(24 months), 3 years (36 months) and 4 years(48 months))

# ETS
## computation time
start_time <- Sys.time()
fc.ets <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec")) %>%
  model(ets = ETS(value ))
end_time <- Sys.time()
end_time - start_time

fc.ets <- fc.ets %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "4 years") 

fc.ets.error <- fc.ets %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.ets <- fc.ets.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")


# ARIMA
## computation time
start_time <- Sys.time()
fc.arima <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec")) %>%
  model(arima = ARIMA(value ))
end_time <- Sys.time()
end_time - start_time

fc.arima <- fc.arima %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "4 years") 

fc.arima.error <- fc.arima %>%
  left_join(new_data) %>%
  mutate(error = actual - .mean)

fc.arima <- fc.arima.error %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

## k = 48
fc.fixsim001.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '48')
fc.fixsim01.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '48')
fc.fixsim05.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '48')
fc.fixsim1.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '48')
## k = 36
fc.fixsim001.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '36')
fc.fixsim01.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '36')
fc.fixsim05.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '36')
fc.fixsim1.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '36')

## k = 24
fc.fixsim001.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '24')
fc.fixsim01.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '24')
fc.fixsim05.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '24')
fc.fixsim1.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '24')

## k = 12
fc.fixsim001.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '12')
fc.fixsim01.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '12')
fc.fixsim05.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '12')
fc.fixsim1.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '12')

fc.fix.noise.FH <- bind_rows(fc.fixsim001.48, fc.fixsim01.48, fc.fixsim05.48, fc.fixsim1.48,
                    fc.fixsim001.36, fc.fixsim01.36, fc.fixsim05.36, fc.fixsim1.36,
                    fc.fixsim001.24, fc.fixsim01.24, fc.fixsim05.24, fc.fixsim1.24,
                    fc.fixsim001.12, fc.fixsim01.12, fc.fixsim05.12, fc.fixsim1.12)

write_csv(fc.fix.noise.FH, 'fc.fix.sim.ets.arima.noiseFH.csv')


### Rolling origin forecasts

## h = 12
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2013 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2013 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))

## h = 24
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

## h = 36
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2015 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2015 Dec")) %>%
  rename(actual = value)%>% 
  arrange(`Date`) %>%
  mutate(new_index = dense_rank(Date))
## h = 48
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2016 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

new_data <- actual.05gts %>%
  dplyr::filter(Date > yearmonth ("2012 Dec")) %>%
  dplyr::filter(Date <= yearmonth ("2016 Dec")) %>%
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
## set k based on the desired forecast horizon (12, 24, 36, 48)
k = 48
m <- c(1:k)
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
## set k based on the desired forecast horizon (12, 24, 36, 48)
k = 48
m <- c(1:k)
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

## k = 48
fc.rollingsim001.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '48')
fc.rollingsim01.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '48')
fc.rollingsim05.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '48')
fc.rollingsim1.48 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '48')
## k = 36
fc.rollingsim001.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '36')
fc.rollingsim01.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '36')
fc.rollingsim05.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '36')
fc.rollingsim1.36 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '36')

## k = 24
fc.rollingsim001.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '24')
fc.rollingsim01.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '24')
fc.rollingsim05.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '24')
fc.rollingsim1.24 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '24')

## k = 12
fc.rollingsim001.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value)) %>%
  mutate(noise = '0.01', FH = '12')
fc.rollingsim01.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.1', FH = '12')
fc.rollingsim05.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '0.5', FH = '12')
fc.rollingsim1.12 <- bind_rows (fc.arima, fc.ets) %>%
  distinct(across(-value))%>%
  mutate(noise = '1', FH = '12')

fc.rolling.noise.FH <- bind_rows(fc.rollingsim001.48, fc.rollingsim01.48, fc.rollingsim05.48, fc.rollingsim1.48,
                             fc.rollingsim001.36, fc.rollingsim01.36, fc.rollingsim05.36, fc.rollingsim1.36,
                             fc.rollingsim001.24, fc.rollingsim01.24, fc.rollingsim05.24, fc.rollingsim1.24,
                             fc.rollingsim001.12, fc.rollingsim01.12, fc.rollingsim05.12, fc.rollingsim1.12)

write_csv(fc.rolling.noise.FH, 'fc.rolling.sim.ets.arima.noiseFH.csv')

