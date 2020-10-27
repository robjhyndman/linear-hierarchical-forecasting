
library(hts)
library(Matrix)
library(reshape)
library(tsibble)
library(fabletools)
library(fable)
library(tidyverse)
library(lubridate)

wikidata <- read.csv('wikipedia_data.csv', header = TRUE)
wiki <- wikidata$views %>%
  matrix(nrow = 394, ncol = 913) %>%
  as.data.frame() 
colnames(wiki) <- unique(wikidata$cat_column) %>% substr(1,16)
wiki <- repair_names(wiki)
wiki <-  tibble(wiki)
wiki$Date <-  rep(ymd("2016/06/01") + 0:393)

wiki <- wiki %>%
  pivot_longer(-Date, names_to = "group", values_to = "value") %>%
  mutate(
    Access = stringr::str_sub(group, 1, 7),
    Agent = stringr::str_sub(group, 8, 9),
    Language = stringr::str_sub(group, 10, 11),
    Purpose = stringr::str_sub(group, 12, 14),
    Article = stringr::str_sub(group, 15, 16),
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(Access, Agent, Language, Purpose, Article))


wikigts <- wiki %>%
  aggregate_key(Access + Agent + Language + Purpose + Access:Agent + Access:Language + Access:Purpose + 
                  Agent:Language + Agent:Purpose + Language:Purpose + Access:Agent:Language:Purpose:Article,
                value = sum(value)) 

##Fixed origin forecasts
fc.ets<- wikigts %>%
  filter(Date <= ymd ("2017/06/01")) %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "28 day") #%>%
  #mutate(value = distributional::dist_truncated(value, 0)) %>%
fc.ets<- fc.ets %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")
 

fc.arima<- wikigts %>%
  filter(Date <= ymd ("2017/06/01")) %>%
  model(arima = ARIMA(value ))%>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "28 day") #%>%
  #mutate(value = distributional::dist_truncated(value, 0)) %>%
fc.arima <- fc.arima %>%  
  hilo(level=95) %>% 
  unpack_hilo("95%")

##Rolling origin forecasts

gts.rolling <- wikigts %>%
  filter(Date < ymd ("2017/06/29")) 

gts.rolling  <- gts.rolling %>%
  stretch_tsibble(.init = 366 , .step = 1)

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





