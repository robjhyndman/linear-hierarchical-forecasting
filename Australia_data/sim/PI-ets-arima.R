
library(fable)
library(fabletools)

actual.sim <- read.csv('sim.actual.10.melt.csv', header = TRUE)
actual.sim$cat <- paste0( actual.sim$level1, actual.sim$level2
                          , actual.sim$State, actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)

actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 3040) %>%
  as.data.frame()
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names

actual.05 <-  tibble(actual.05)
actual.05$Date <-  rep(yearmonth("1998 Jan") + 0:227)


actual.05 <- actual.05 %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    level1 = stringr::str_sub(group, 1, 1),
    level2 = stringr::str_sub(group, 1, 2),
    State = stringr::str_sub(group, 1, 3),
    Zone = stringr::str_sub(group, 1, 4),
    Region = stringr::str_sub(group, 1, 5),
    Purpose = stringr::str_sub(group, 6, 9),
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(level1, level2, State, Zone, Region, Purpose))


actual.05gts <- actual.05 %>%
  aggregate_key(Purpose * (level1/level2/State/ Zone/ Region), value = sum(value)) 


fc.ets <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(ets = ETS(value)) %>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "2 years")

fc.ets <- fc.ets %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")

fc.arima <- actual.05gts %>%
  filter(Date <= yearmonth ("2012 Dec"))%>%
  model(arima = ARIMA(value)) %>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "2 years")

fc.arima <- fc.arima %>% 
  hilo(level=95)%>% 
  unpack_hilo("95%")

