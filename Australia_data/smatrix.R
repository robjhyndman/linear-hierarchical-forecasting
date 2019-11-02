
## Here we have the functions for comuting summing matrix (GmatrixG & SmatrixM) and lambda in reconciliation equation (Mlevel & InvS4g)

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

Mlevel <- function(xgroup) {
  m <- apply(xgroup, 1, function(x) length(unique(x)))
  return(m)
}

# A function to get the inverse of row sums of S matrix
InvS4g <- function(xgroup) {
  mlevel <- Mlevel(xgroup)
  len <- length(mlevel)
  repcount <- mlevel[len]/mlevel
  inv.s <- 1/unlist(mapply(rep, repcount, mlevel, SIMPLIFY = FALSE))
  return(inv.s)
}
