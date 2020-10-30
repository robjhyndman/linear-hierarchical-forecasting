# library(hts)
# library(Matrix)
# library(reshape)
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

wiki <- read_csv("wikipedia_data.csv") %>%
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
fc.ets <- wiki %>%
  filter(date <= "2017-06-01") %>%
  model(ets = ETS(views)) 

fc.ets %>%
  filter(
    is_aggregated(Access),
    is_aggregated(Agent),
    is_aggregated(Language),
    is_aggregated(Purpose),
    is_aggregated(Article)
  ) %>%
  forecast(h = "28 days") %>%
  autoplot(wiki)

fc.ets.reconciled <- fc.ets %>%
  reconcile(ets_adjusted = min_trace(ets, method = "wls_struct")) %>%
  forecast(h = "28 days") 


fc.ets.reconciled %>%
  filter(#.model == "ets",
         is_aggregated(Access),
         is_aggregated(Agent),
         is_aggregated(Language),
         is_aggregated(Purpose),
         is_aggregated(Article)
  ) %>%
  autoplot(wiki)

fc.hilo <- fc.ets.reconciled %>% 
   hilo(level = 95) %>%
   unpack_hilo("95%")

fc.arima <- wiki %>%
  filter(date <= "2017-06-01") %>%
  model(arima = ARIMA(views)) %>%
  reconcile(arima_adjusted = min_trace(arima, method = "wls_struct")) %>%
  forecast(h = "28 day") %>%
  hilo(level = 95) %>%
  unpack_hilo("95%")


## Rolling origin forecasts
gts.rolling <- wiki %>%
  filter(date < "2017-06-29") %>%
  stretch_tsibble(.init = 20, .step = 1)

# ETS
fc.ets <- gts.rolling %>%
  model(ets = ETS(views))

m <- c(1:24)
fc.ets.rec <- data.frame(a = c(), b = c())
for (i in m) {
  result <- fc.ets %>%
    filter(.id == i) %>%
    reconcile(ets_adjusted = min_trace(ets, method = "wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level = 95) %>%
    unpack_hilo("95%")
  rec.res <- cbind.data.frame(result$key, result$index, result$.model, result$.mean, result$`95%_lower`, result$`95%_upper`, "h" = i)
  fc.ets.rec <- bind_rows(fc.ets.rec, rec.res)
}

# ARIMA
fc.arima <- gts.rolling %>%
  model(arima = ARIMA(views))

m <- c(1:24)
fc.arima.rec <- data.frame(a = c(), b = c())
for (i in m) {
  result <- fc.arima %>%
    filter(.id == i) %>%
    reconcile(arima_adjusted = min_trace(arima, method = "wls_struct")) %>%
    forecast(h = 1) %>%
    hilo(level = 95) %>%
    unpack_hilo("95%")
  rec.res <- cbind.data.frame(result$key, result$index, result$.model, result$.mean, result$`95%_lower`, result$`95%_upper`, "h" = i)
  fc.arima.rec <- bind_rows(fc.arima.rec, rec.res)
}
