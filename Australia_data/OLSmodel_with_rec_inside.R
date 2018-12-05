## X is a dataframe including all the series (ally)
## h is forecast horizon and g is the grouping dataset

### Using "combinef" inside the function

OLSmodel.rec.com<-function(X,freq,maxlag,h,g){
  fc<-matrix(NA,nrow=h,ncol = NCOL(X))
  for(j in 1:NCOL(X))
  { Y<-as.vector(X[,j]) 
    trend<-seq(NROW(Y))
    season<-forecast::seasonaldummy(ts(Y,frequency = freq))
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(X,k=1:maxlag)
      X_mat<-cbind.data.frame(X,trend,season,Xlag)
    }
    else
      X_mat<-cbind.data.frame(X,trend,season)
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
  colnames(fore_base_total)<-colnames(X)
  fc.rec<-combinef(fore_base_total, groups=g)$bts
  return(fc.rec)
}

OLS<-OLSmodel.rec.com(ally,12,12,24,ausgts$groups)

### without Using "combinef" inside the function
### this function returns reconciled forecasts (all the levels) and same results as above with algorithm="lu"

OLSmodel.rec<-function(X,freq,maxlag,h,g){
  gmat<-GmatrixG(g)
  smatrix<-SmatrixM(gmat)
  rec.adj<-as.matrix(smatrix%*%solve(((t(smatrix))%*%smatrix))%*%t(smatrix))
  fc<-matrix(NA,nrow = h,ncol=NCOL(X))
  for(j in seq(NCOL(X))){
    X<-as.vector(X[,j])
    trend<-seq(NROW(X))
    season<-forecast::seasonaldummy(ts(X,frequency = freq))
    if(maxlag>0)
    {
      Xlag<-quantmod::Lag(X,k=1:maxlag)
      X_mat<-cbind.data.frame(X,trend,season,Xlag)
    }
    else
      X_mat<-cbind.data.frame(X,trend,season)
    n <- nrow(X_mat)
    for (i in 1:h) {
      train.1 <- X_mat[1:((n - h) + (i - 1)), ]
      valid.1 <- X_mat[(n - h) + i, ]
      fit <- lm(X ~. , data = train.1)
      fore <- predict.lm( fit , newdata = valid.1) 
      fc[i,j]<-fore
    }
  }
  fc[fc<0]<-0
  fc.rec<- t(rec.adj%*%t(fc))
  return(fc.rec)
}

OLS<-OLSmodel.rec(ally,12,12,24,ausgts$groups)


######################################
#### functions for gmat and smatrix - available at "hts" package
#####################################
# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to integer
    gmat <- t(apply(xmat, 1, function(x) as.integer(factor(x, unique(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  nc.xmat <- ncol(xmat)
  gmat <- rbind(
    if (all(gmat[1,] == rep(1L, nc.xmat))) NULL else rep(1L, nc.xmat),
    gmat,
    if (all(gmat[NROW(gmat),] == seq(1L, nc.xmat))) NULL else seq(1L, nc.xmat)
  )
  #gmat <- gmat[!duplicated(gmat), , drop = FALSE] # Remove possible duplicated... make smarter above.
  return(structure(gmat, class = "gmatrix"))
}

SmatrixM <- function(gmat) { 
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- sparseMatrix(i = ia, j = ja, x = ra)
  })
  sparse <- do.call("rbind", sparse.S)
  return(sparse)
}