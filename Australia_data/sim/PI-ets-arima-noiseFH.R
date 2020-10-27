

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

### fixed origin forecasts
## Change h based on the desired forecast horizon (1 year(12 months), 2 years(24 months), 3 years (36 months) and 4 years(48 months))

# ETS
fc.ets <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(ets = ETS(value)) %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "4 years")

fc.ets <- fc.ets %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")
# ARIMA
fc.arima <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(arima = ARIMA(value)) %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "4 years")

fc.arima <- fc.arima %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")

### Rolling origin forecasts

## h = 12
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2013 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)
## h = 24
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2014 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)
## h = 36
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2015 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)
## h = 48
gts.rolling <- actual.05gts %>%
  filter(Date < yearmonth ("2016 Dec"))
gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 180 , .step = 1)

# ETS
fc.ets <- gts.rolling %>%
  model(ets = ETS(value))
## set k based on the desired forecast horizon (12, 24, 36, 48)
k = 12
m <- c(1:k)
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
## set k based on the desired forecast horizon (12, 24, 36, 48)
k = 12
m <- c(1:k)
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




