## X is a dataframe including all the series (ally)
## h is forecast horizon and g is the grouping dataset
OLSmodel.rec<-function(X,freq,maxlag,h,g){
  fore_base_total<-matrix(NA,nrow=h,ncol = NCOL(X))
  for(j in 1:NCOL(X))
  { Y<-as.vector(X[,j]) 
    trend<-seq(NROW(Y))
    season<-forecast::seasonaldummy(ts(Y,frequency = freq))
    Xlag<-quantmod::Lag(Y,k=1:maxlag)
    X_mat<-cbind.data.frame(Y,trend,season,Xlag)
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
   fore_base_total[,j]<-pmax(fore_base_OLS,0)
  }
  colnames(fore_base_total)<-colnames(X)
  fore_rec<-combinef(fore_base_total, groups=g)$bts
  return(fore_rec)
}


OLS<-OLSmodel.rec(ally,12,4,24,ausgts$groups)

