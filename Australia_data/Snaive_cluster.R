## with clustering information
library(tidyverse)
library(hts)
# Reading data and adding separate category columns
aus <- ts(readr::read_csv("TourismData_v3.csv")[, -(1:2)],
          start = 1998, frequency = 12)
ausgts <- gts(aus, characters = list(c(1, 1, 1), 3),
              gnames = c("State", "Zone", "Region", "Purpose",
                         "State x Purpose", "Zone x Purpose"))
clusters_<-read.csv("Cluster_Tourism.csv",header = TRUE)
clusters<-as.vector(clusters_$Cluster)
# Recode clusters so they are ordered
clusters <- as.numeric(factor(clusters, levels=unique(clusters)))
ausgts$groups<-rbind(ausgts$groups[1:7,],Clusters=clusters,Bottom=ausgts$groups[8,])
class(ausgts$groups) <- "gmatrix"
ausgts$labels$Clusters<-paste("Cluster/",1:6)
# Splitting data into training and validation sets
austrain <- window(ausgts, end=c(2014,12))
austest  <- window(ausgts, start=c(2015,1))
# Construct matrix of all time series including aggregates
ally <- aggts(austrain)
# Set up array for forecasts
h <- NROW(austest$bts)
fc <- array(NA, c(Horizon=h, Series=NCOL(ally), Method=1))
dimnames(fc) <- list(
  Horizon = paste0("h=",seq(h)),
  Series = colnames(ally),
  Method = c("Snaive")
)

# Create forecasts for all methods
for(i in seq(NCOL(ally)))
{
  fc[,i,"Snaive"] <- pmax(forecast(snaive(ally[,i]), h=h)$mean,0)
}

## Set up array for errors (bottom level only)
errors <- array(NA, c(Horizon=h, Series=NCOL(aus), Method=dim(fc)[3], Reconciled=2))
dimnames(errors) <- list(
  Horizon = dimnames(fc)[[1]],
  Series = colnames(aus),
  Method = dimnames(fc)[[3]],
  Reconciled = c("Reconciled","Unreconciled")
)

# Compute errors for unreconciled forecasts
nbts <- NCOL(aus)
nseries <- NCOL(ally)
for(i in seq(dim(errors)[3]))
  errors[,,i,"Unreconciled"] <- austest$bts - fc[,nseries - nbts + seq(nbts),i]

# Compute errors for reconciled forecasts
for(i in seq(dim(errors)[3]))
{
  revisedfc <- combinef(fc[,,i], groups=austrain$groups)$bts
  tsp(revisedfc) <- tsp(austest$bts)
  errors[,,i,"Reconciled"] <- austest$bts - revisedfc
}
# Compute RMSE across bottom-level series
rmse <- sqrt(apply(errors^2, c(3,4), mean))
rmse


