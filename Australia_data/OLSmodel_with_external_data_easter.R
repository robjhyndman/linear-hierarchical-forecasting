## X is a dataframe including all the series (ally)
## h is forecast horizon and g is the grouping dataset
## externaldata (optionally) can be a vector or matrix of external time series - its length should be same as you original data (train and test sets)
## Using "combinef" inside the function


OLSmodel.external<-function(X, freq, maxlag, externaldata=NULL, h, g){
  fc<-matrix(NA,nrow=h,ncol = NCOL(X))
  for(j in 1:NCOL(X))
  { Y<-as.vector(X[,j]) 
  trend<-seq(NROW(Y))
  season<-forecast::seasonaldummy(ts(Y,frequency = freq))
  if(is.null(externaldata) == FALSE)
  {
    externaldata<-as.data.frame(externaldata)
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(Y,k=1:maxlag)
      X_mat<-cbind.data.frame(Y,trend,season,externaldata,Xlag)
    }
    else
      X_mat<-cbind.data.frame(Y,trend,season,externaldata)
  }
  else
  {
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(Y,k=1:maxlag)
      X_mat<-cbind.data.frame(Y,trend,season,Xlag)
    }
    else
      X_mat<-cbind.data.frame(Y,trend,season)
  }
  n <- nrow(X_mat)
  fore_base_OLS<-matrix(NA,nrow = h,ncol=1)
  for (i in 1:h) 
  {
    train.1 <- X_mat[1:((n - h) + (i - 1)), ]
    valid.1 <- X_mat[(n - h) + i, ]
    fit <- lm(Y ~. , data = train.1)
    fore <- predict.lm( fit , newdata = valid.1)
    fore_base_OLS[i,]<-fore
  }
  fc[,j]<-pmax(fore_base_OLS,0)
  }
  colnames(fc)<-colnames(X)
  fc.rec<-combinef(fc, groups=g)$bts
  return(fc.rec)
}

## Using easter information for the external data
easter.info<-easter(aus,easter.mon = TRUE)

OLS<-OLSmodel.external(ally,12,12,24,externaldata = easter.info,ausgts$groups)


## Without reconceliation inside (2 steps)

OLSmodel.external<-function(X,freq,maxlag,externaldata=NULL,h){
  X<-as.vector(X)
  trend<-seq(NROW(X))
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  if(is.null(externaldata) == FALSE)
  {
    externaldata<-as.data.frame(externaldata)
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(X,k=1:maxlag)
      X_mat<-cbind.data.frame(X,trend,season,externaldata,Xlag)
    }
    else
      X_mat<-cbind.data.frame(X,trend,season,externaldata)
  }
  else
  {
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(X,k=1:maxlag)
      X_mat<-cbind.data.frame(X,trend,season,Xlag)
    }
    else
      X_mat<-cbind.data.frame(X,trend,season)
  }
  n <- nrow(X_mat)
  fore_base_OLS<-matrix(NA,nrow = h,ncol=1)
  for (i in 1:h) {
    train.1 <- X_mat[1:((n - h) + (i - 1)), ]
    valid.1 <- X_mat[(n - h) + i, ]
    fit <- lm(X ~. , data = train.1)
    fore <- predict.lm( fit , newdata = valid.1)
    fore_base_OLS[i,]<-fore
  }
  return(fore_base_OLS)
}



