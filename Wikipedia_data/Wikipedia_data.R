### Wikipedia dataset - all the levels Mean(RMSE)
##########################
### 28-step-ahead
##########################

lapply(c("quantmod", "forecast", "dplyr", "plyr", "stringr", "hts", "data.table"),
  require,
  character.only = TRUE
)
wikipedia_data <- read.csv("wikipedia_data.csv", header = TRUE)

## Data reshaping
# 394: length of each series and 913: number of series
wikipedia_wide <- wikipedia_data$views %>%
  matrix(nrow = 394, ncol = 913) %>%
  as.data.frame() %>%
  ts(frequency = 7)
colnames(wikipedia_wide) <- unique(wikipedia_data$cat_column) %>% substr(1,14)

##################
## using hierarchies and groupings up to 2-way combinations
##################
wikigts <- gts(wikipedia_wide, character=c(7,2,2,3),
               gnames = c("Access",
                          "Agent",
                          "Language",
                          "Purpose",
                          "Access x Agent",
                          "Access x Language",
                          "Access x Purpose",
                          "Agent x Language",
                          "Agent x Purpose",
                          "Language x Purpose"))
                          
# Splitting data into training and test sets
wikitrain <- window(wikigts, end = c(1, 366))
wikitest <- window(wikigts, start = c(1, 367))

# Construct matrix of all time series including aggregates
ally <- aggts(wikitrain)
ally.test <- aggts(wikitest)

# Set up array for forecasts
h <- NROW(wikitest$bts)
fc <- array(NA, c(Horizon = h, Series = NCOL(ally), Method = 3))
dimnames(fc) <- list(
  Horizon = paste0("h=", seq(h)),
  Series = colnames(ally),
  Method = c("ETS", "ARIMA", "OLS")
)

# Create forecasts for all methods
for (i in seq(NCOL(ally))) {
  fc[, i, "ETS"] <- pmax(forecast(ets(ally[, i]), h = h)$mean, 0)
  fc[, i, "ARIMA"] <- pmax(forecast(auto.arima(ally[, i]), h = h)$mean, 0)
  fc[, i, "OLS"] <- pmax(olsfc(ally[, i], h = h, maxlag = 7), 0)
}

## saving forecasts
forecast.ets.arima.OLS <- array(
  NA,
  c(Horizon = h, Series = NCOL(ally), Method = dim(fc)[3], Reconciled = 2)
)
dimnames(forecast.ets.arima.OLS) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(ally),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled", "Unreconciled")
)
for (i in seq(dim(fc)[3])) {
  forecast.ets.arima.OLS[, , i, "Unreconciled"] <- fc[, , i]
  forecast.ets.arima.OLS[, , i, "Reconciled"] <- combinef(fc[, , i], groups = wikitrain$groups, keep = "all")
}

## Set up array for errors (bottom level only)
errors.level <- array(
  NA,
  c(Horizon = h, Series = NCOL(ally), Method = dim(fc)[3], Reconciled = 2)
)
dimnames(errors.level) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(ally),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled", "Unreconciled")
)

# Compute errors for unreconciled forecasts
nbts <- NCOL(wikipedia_wide)
nseries <- NCOL(ally)

for (i in seq(dim(errors.level)[3])) {
  errors.level[, , i, "Unreconciled"] <- ally.test - fc[, , i]
}

# Compute errors for reconciled forecasts
for (i in seq(dim(errors.level)[3])) {
  revisedfc <- combinef(fc[, , i], groups = wikitrain$groups, keep = "all")
  colnames(revisedfc) <- names(ally.test)
  tsp(revisedfc) <- tsp(wikitest$bts)
  errors.level[, , i, "Reconciled"] <- ally.test - revisedfc
}


##### How to define the groups if you need to include all 2-and 3-way combinations (it is not possible with using above method and package can not support)
wikipedia_data <-read.csv("wikipedia_data.csv", header = TRUE)
wikipedia_wide <- wikipedia_data$views %>%
  matrix(nrow = 394, ncol = 913) %>%
  as.data.frame() %>%
  ts(frequency = 7)
colnames(wikipedia_wide) <- unique(wikipedia_data$cat_column) %>% substr(1,14)
## using hierarchies and groupings up to 2-way and 3-way combinations
col_1<-substr(wikipedia_data$access , start = 1, stop = 7)
col_2<-substr(wikipedia_data$agent, start = 1, stop = 2)
col_3 <-substr(wikipedia_data$language , start = 1, stop = 2)
col_4 <-substr(wikipedia_data$Purpose , start = 1, stop = 3)
m<-as.character(as.numeric(wikipedia_data$article,length=2))
col_5 <-str_pad(m, 2, pad = "0")
wikipedia_data$cat<-paste(col_1, col_2, col_3, col_4, col_5, sep = "")

names <- as.vector(unique(wikipedia_data$cat))
names(wikipedia_wide) <- c(names)

name_length <- str_length(names)
grouping_hts <- rbind(
  #access
  str_sub(names, start = name_length - 15, end = name_length - 9),
  #agent
  str_sub(names, start = name_length - 8, end = name_length - 7),
  #language
  str_sub(names, start = name_length - 6, end = name_length - 5),
  #purpose
  str_sub(names, start = name_length - 4, end = name_length - 2),
  #Access x Agent
  str_sub(names, start = name_length - 15, end = name_length - 9),
  #Access x Language
  paste(str_sub(names, start = name_length - 15, end = name_length - 9), str_sub(names, start = name_length - 6, end = name_length - 5), sep = ""),
  #Access x Purpose
  paste(str_sub(names, start = name_length - 15, end = name_length - 9), str_sub(names, start = name_length - 4, end = name_length - 2), sep = ""),
  #Agent x Language
  str_sub(names, start = name_length - 8, end = name_length - 5),
  #Agent x Purpose
  paste(str_sub(names, start = name_length - 8, end = name_length - 7), str_sub(names, start = name_length - 4, end = name_length - 2), sep = ""),
  #Language x Purpose
  str_sub(names, start = name_length - 6, end = name_length - 2),
  #Access x Agent x Language
  paste(str_sub(names, start = name_length - 15, end = name_length - 9), str_sub(names, start = name_length - 8, end = name_length - 7),
        str_sub(names, start = name_length - 6, end = name_length - 5), sep = ""),
  #Access x Agent x Purpose
  paste(str_sub(names, start = name_length - 15, end = name_length - 9), str_sub(names, start = name_length - 8, end = name_length - 7),
        str_sub(names, start = name_length - 4, end = name_length - 2), sep = ""),
  #Access x Language x Purpose
  paste(str_sub(names, start = name_length - 15, end = name_length - 9), str_sub(names, start = name_length - 6, end = name_length - 5),
        str_sub(names, start = name_length - 4, end = name_length - 2), sep = ""),
  #Agent x Language x Purpose
  paste(str_sub(names, start = name_length - 8, end = name_length - 7), str_sub(names, start = name_length - 6, end = name_length - 5),
        str_sub(names, start = name_length - 4, end = name_length - 2), sep = "")
)
wikigts <- gts(wikipedia_wide, groups = grouping_hts) 
