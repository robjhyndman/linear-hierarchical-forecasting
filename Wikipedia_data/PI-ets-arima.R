
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
colnames(wiki) <- unique(wikidata$cat_column) %>% substr(1,14)
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
  ) %>%
  select(-group) %>%
  as_tsibble(index = Date, key=c(Access, Agent, Language, Purpose))


wikigts <- wiki %>%
  aggregate_key(Access * Agent + Access * Language + Access * Purpose + 
                  Agent * Language + Agent * Purpose + Language * Purpose , value = sum(value)) 

dim(wikigts)/394
write.csv(wikigts, 'test.csv')

fc.ets<- wikigts %>%
  filter(Date <= ymd ("2017/06/01")) %>%
  model(ets = ETS(value ))%>%
  reconcile(ets_adjusted = min_trace(ets, method="wls_struct"))%>%
  forecast(h = "28 day") %>%
  mutate(value = distributional::dist_truncated(value, 0)) %>%
  hilo(level=95) %>% 
  unpack_hilo("95%")

