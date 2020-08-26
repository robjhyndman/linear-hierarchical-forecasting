
actual.sim <- read.csv('sim.actual.10.melt.csv', header = TRUE)
actual.sim$cat <- paste0(actual.sim$State, 
                         actual.sim$Zone, actual.sim$Region, actual.sim$Purpose)
actual.05 <- actual.sim$value %>%
  matrix(nrow = 228, ncol = 3040) %>%
  as.data.frame() %>%
  ts(frequency = 12)
names <- as.vector(unique(actual.sim$cat))
colnames(actual.05) <- names
sim.05gts <- gts(actual.05, characters = list(c( 1, 1, 1), 4),
                 gnames = c("State", "Zone", "Region", "Purpose", 
                            "State x Purpose", "Zone x Purpose"))
##### Adding hierarchy factors (level1 and level2)
sim.05gts <- gts(actual.05, characters = list(c(1, 1, 1, 1, 1), 3),
                 gnames = c("level1", "level2", "State", "Zone", "Region", "Purpose",
                            "level1 x Purpose", "level2 x Purpose", "State x Purpose", "Zone x Purpose"))

##################### Adding grouping factor (G1)
name_length <- str_length(names)
grouping_gts <- rbind(
  #level1
  str_sub(names, start = name_length - 10, end = name_length - 10),
  #level2
  str_sub(names, start = name_length - 10, end = name_length - 9),
  #State
  str_sub(names, start = name_length - 10, end = name_length - 8),
  #Zone
  str_sub(names, start = name_length - 10, end = name_length - 7),
  #Region
  str_sub(names, start = name_length - 10, end = name_length - 6),
  #Purpose
  str_sub(names, start = name_length - 5, end = name_length - 2),
  #level3
  str_sub(names, start = name_length - 1, end = name_length),
  #level1 x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 10), 
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #level2 x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 9),
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #State x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 8), 
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #Zone x Purpose
  paste(str_sub(names, start = name_length - 10, end = name_length - 7),
        str_sub(names, start = name_length - 5, end = name_length - 2), sep = ""),
  #level3 x Purpose
  paste(str_sub(names, start = name_length - 5, end = name_length - 2),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #level1 x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 10),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #level2 x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 9),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #State x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 8),
        str_sub(names, start = name_length - 1, end = name_length), sep = ""),
  #Zone x level3
  paste(str_sub(names, start = name_length - 10, end = name_length - 7),
        str_sub(names, start = name_length - 1, end = name_length), sep = "")
 )


sim.05gts <- gts(actual.05,  groups = grouping_gts)

#############################





k<-24
n<-nrow(actual.05)
train.05 <-window(sim.05gts,start = c(1, 1),end = c(1, (n-48)))
test.05 <-window(sim.05gts,start = c(1, ((n-48)+1)),end = c(1, n-(48-k)))
ally <- aggts(train.05)
ally.test<-aggts(test.05)
ally.test.05 <- as.data.frame(reshape2::melt(ally.test)$value)

h <- NROW(test.05$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=1))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("ETS")#, "OLS.se")
)
start.time <- Sys.time()
# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  # fit.OLS <- olsfc(ally[,i], h=h, maxlag = 12, nolag = c(1,12))
  # fc[,i,"OLS"] <- fit.OLS[[1]]
  # fc[,i,"OLS.se"] <- fit.OLS[[2]]
  fit.ETS <- forecast(ets(ally[,i]), h=h)
  fc[,i,"ETS"] <- fit.ETS$mean
  # fit.ARIMA <- forecast(auto.arima(ally[,i], nmodels = 200), h=h)
  # fc[,i,"ARIMA"] <- fit.ARIMA$mean
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#fc.OLS.base.3040.8 <- as.data.frame(fc[,,"OLS"])
#fc.OLS.se.3040.8 <- as.data.frame(fc[,,"OLS.se"])
fc.ETS.base.3040.8 <- as.data.frame(fc[,,"ETS"])
#fc.ARIMA.base.3040.8 <- as.data.frame(fc[,,"ARIMA"])
#colnames(fc.OLS.base.3040.8) <- colnames(ally)
#colnames(fc.OLS.se.3040.8) <- colnames(ally)

gmat <- GmatrixG(sim.05gts$groups)
smatrix <- as.matrix(SmatrixM(gmat))
lambda <- diag(rowSums(smatrix))

rec.adj.lambda <- as.matrix(smatrix%*%solve(t(smatrix)%*%solve(lambda)%*%smatrix)%*%t(smatrix)%*%solve(lambda))


## computing reconciled forecasts
fc.ETS.rec.3040.8  <- matrix(NA, nrow = k, ncol = ncol(ally))
for(i in 1:nrow(fc.ETS.base.3040.8 )){
  f.1 <- matrix(as.numeric(fc.ETS.base.3040.8 [i,]), ncol = 1, nrow = ncol(fc.ETS.base.3040.8 ))
  fc.ETS.rec.3040.8  [i,] <- rec.adj.lambda %*% f.1
}


