## Here we have the codes which reconciled the base forecasts using weighted matrix lambda (we have the base forecasts)

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
########OLS-rec-matrix
k<-28
ally <- aggts(wikigts)
gmat<-GmatrixG(wikigts$groups)
smatrix<-SmatrixM(gmat)
wvec<- InvS4g(wikigts$groups)

lambda <- diag(wvec)

rec.adj.test <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

forecast.wiki <- read.csv("forecast.wiki.csv", header = TRUE)

forecast.wiki.1 <- forecast.wiki[forecast.wiki$ForecastInterval == "28Step",]

forecast.wiki.1.OLS <- as.matrix(forecast.wiki.1$OLS.unrec)
#forecast.wiki.1.actual <- forecast.wiki.1$Actual

forecast.wiki.1.OLS.test <- matrix(forecast.wiki.1.OLS, ncol = 1035, nrow = 28)
#forecast.wiki.1.actual.test <- matrix(forecast.wiki.1.actual, ncol = 555, nrow = 24)

ftar.1 <- matrix(NA, nrow = 28, ncol = 1035)

for(i in 1:nrow(forecast.wiki.1.OLS.test)){
  fta.1 <- matrix(forecast.wiki.1.OLS.test[i,], ncol = 1, nrow = ncol(forecast.wiki.1.OLS.test))
  ftar.1 [i,] <- rec.adj.test %*% fta.1
}

write.csv(melt(ftar.1), "OLS_rec.csv")
