## Here we have the codes which reconciled the base forecasts using weighted matrix lambda (we have the base forecasts)

TourismData <-ts(read.csv("TourismData_v3.csv", header = TRUE)[-c(1,2)],start=1,frequency =12)
ausgts <- gts(TourismData, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
########OLS-rec-matrix
k<-24
ally <- aggts(ausgts)
gmat<-GmatrixG(ausgts$groups)
smatrix<-SmatrixM(gmat)
wvec<- InvS4g(ausgts$groups)

lambda <- diag(wvec)

rec.adj.test <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))

forecast.tourism <- read.csv("forecast.tourism.csv", header = TRUE)

forecast.tourism.24 <- forecast.tourism[forecast.tourism$ForecastInterval == "24Step",]

forecast.tourism.24.ets <- as.matrix(forecast.tourism.24$ETS.unrec)
#forecast.tourism.24.actual <- forecast.tourism.24$Actual

forecast.tourism.24.ets.test <- matrix(forecast.tourism.24.ets, ncol = 555, nrow = 24)
#forecast.tourism.24.actual.test <- matrix(forecast.tourism.24.actual, ncol = 555, nrow = 24)

ftar.24 <- matrix(NA, nrow = 24, ncol = 555)

for(i in 1:nrow(forecast.tourism.24.ets.test)){
  fta.24 <- matrix(forecast.tourism.24.ets.test[i,], ncol = 1, nrow = ncol(forecast.tourism.24.ets.test))
  ftar.24 [i,] <- rec.adj.test %*% fta.24
}

#write.csv(melt(ftar.24), "ETS_rec.csv")