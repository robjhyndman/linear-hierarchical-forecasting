
actual.sim <- read.csv('actual.sim.csv', header = TRUE)
actual.sim.001 <- actual.sim[actual.sim$Sim == 'sig0.01',]
actual.sim.01 <- actual.sim[actual.sim$Sim == 'sig0.1',]
actual.sim.05 <- actual.sim[actual.sim$Sim == 'sig0.5',]
actual.sim.1 <- actual.sim[actual.sim$Sim == 'sig1',]

actual.001 <- actual.sim.001$value %>%
  matrix(nrow = 228, ncol = 304) %>%
  as.data.frame() %>%
  ts(frequency = 12)
colnames(actual.001) <- unique(actual.sim.001$Var2)


actual.001gts <- gts(actual.001, characters = list(c(1, 1, 1), 3),
                     gnames = c("State", "Zone", "Region", "Purpose","State x Purpose", "Zone x Purpose"))
k<-24
n<-nrow(actual.001)
train.001 <-window(actual.001gts,start = c(1, 1),end = c(1, (n-48)))
test.001 <-window(actual.001gts,start = c(1, ((n-48)+1)),end = c(1, n-(48-k)))
ally <- aggts(train.001 )
ally.test<-aggts(test.001)
ally.test.001 <- as.data.frame(reshape2::melt(ally.test)$value)
write.csv(ally.test.001 , "ally.test.001.csv")
#ally.test.001 <- rbind.data.frame(ally.test.001, ally.test.001)
h <- NROW(test.001$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=2))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("OLS", "OLS.se")
)
#start.time <- Sys.time()
# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fit.OLS <- olsfc(ally[,i], h=h, maxlag = 12, nolag = c(1,12))
  fc[,i,"OLS"] <- fit.OLS[[1]]
  fc[,i,"OLS.se"] <- fit.OLS[[2]]
  # fit.ETS <- forecast(ets(ally[,i]), h=h)
  # fc[,i,"ETS"] <- fit.ETS$mean
  # fit.ARIMA <- forecast(auto.arima(ally[,i], nmodels = 200), h=h)
  # fc[,i,"ARIMA"] <- fit.ARIMA$mean
}
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
fc.OLS.base.24.001 <- as.data.frame(fc[,,"OLS"])
fc.OLS.se.24.001 <- as.data.frame(fc[,,"OLS.se"])
# fc.ETS.base.12.001 <- as.data.frame(fc[,,"ETS"])
# fc.ARIMA.base.12.001 <- as.data.frame(fc[,,"ARIMA"])
colnames(fc.OLS.base.24.001) <- colnames(ally)
colnames(fc.OLS.se.24.001) <- colnames(ally)

############################################


gmat <- GmatrixG(actual.001gts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))


## computing reconciled forecasts
fc.OLS.rec.36.001 <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.OLS.base.36.001)){
  f.1 <- matrix(as.numeric(fc.OLS.base.36.001[i,]), ncol = 1, nrow = ncol(fc.OLS.base.36.001))
  fc.OLS.rec.36.001 [i,] <- rec.adj.lambda %*% f.1
}
