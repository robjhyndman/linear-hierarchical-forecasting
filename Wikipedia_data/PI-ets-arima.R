
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


fc.ets<- wikigts %>%
  filter(Date <= ymd ("2017/06/01")) %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "28 day") %>%
  #mutate(value = distributional::dist_truncated(value, 0)) %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

fc.ets.data <- cbind.data.frame('Access' = fc.ets$Access, 'Agent' = fc.ets$Agent,
                                'Purpose' = fc.ets$Purpose, 'Language' = fc.ets$Language, 
                                'Article' = fc.ets$Article, 
                                'Method' =  fc.ets$.model, 'date' = fc.ets$Date, 'value' = fc.ets$.mean, 
                                'in.lower' = fc.ets$`95%_lower`,'in.upper' = fc.ets$`95%_upper`)
write.csv(fc.ets.data, 'fc.ets.wiki.csv')  

fc.arima<- wikigts %>%
  filter(Date <= ymd ("2017/06/01")) %>%
  model(arima = ARIMA(value ))%>%
  reconcile(arima_adjusted = min_trace(arima, method="wls_struct"))%>%
  forecast(h = "28 day") %>%
  #mutate(value = distributional::dist_truncated(value, 0)) %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")


