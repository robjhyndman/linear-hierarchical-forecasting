

OLSmodel<-function(X,freq,maxlag,h, nolag = NULL){
  X<-as.vector(X)
  trend1 <- seq(NROW(X))
  trend2 <- seq(NROW(X))^2
  season<-forecast::seasonaldummy(ts(X,frequency = freq))
  if(maxlag>0)
  {
    Xlag <- quantmod::Lag(X,k=1:maxlag)
    if(length(nolag) == 0)
      #X_mat <- cbind.data.frame(X, trend, season)
      X_mat <- cbind.data.frame(X, trend1, trend2, season)
    else
      #X_mat <- cbind.data.frame(X, trend, season, Xlag[,nolag])
      X_mat <- cbind.data.frame(X, trend1, trend2, season, Xlag[,nolag])
  }
  else
    #X_mat<-cbind.data.frame(X, trend, season)
    X_mat<-cbind.data.frame(X,trend1, trend2,season)
  n <- nrow(X_mat)
  fc<-matrix(NA,nrow = h,ncol=1)
  lwr<-matrix(NA,nrow = h,ncol=1)
  upr<-matrix(NA,nrow = h,ncol=1)
  se<-matrix(NA,nrow = h,ncol=1)
  residual.scale <- c() 
  for (i in 1:h) {
    train.1 <- X_mat[1:((n - h) + (i - 1)), ]
    valid.1 <- X_mat[(n - h) + i, ]
    fit <- lm(X ~. , data = train.1)
    pred <-  predict(fit, newdata = valid.1, se.fit = TRUE, 
                     residual.scale = TRUE, interval = "prediction", level = 0.95)
    fc[i,] <- pred$fit[,1]
    lwr[i,] <- pred$fit[,2]
    upr[i,] <- pred$fit[,3]
    se[i,] <- pred$se.fit
    residual.scale[i] <- pred$residual.scale
    # fore <- predict( fit , newdata = valid.1, se.fit = TRUE)
    # fore_base_OLS[i,]<-fore$fit
    # fore_se_OLS[i,]<-fore$se.fit

  }
  return(list(fc, lwr, upr, se, residual.scale))
}
