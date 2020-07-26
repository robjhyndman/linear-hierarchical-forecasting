
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


GmatrixH <- function(xlist) {
  l.xlist <- length(xlist)
  num.bts <- sum(xlist[[l.xlist]])
  nlist <- unlist(lapply(xlist, length))
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = l.xlist, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1L, num.bts)
  # Insert the middle levels in the reverse order
  if (l.xlist > 1L) {
    repcount <- xlist[[l.xlist]]
    for (i in (l.xlist - 1L):1L) {
      gmat[i, ] <- rep(1L:nlist[i + 1], repcount)
      repcount <- rowsum(repcount, rep(1L:nlist[i], xlist[[i]]))
    }
  }
  # Insert the top level
  gmat <- rbind(rep(1L, num.bts), gmat)
  
  dimnames(gmat) <- list(paste("Level", 0L:(nrow(gmat) - 1L)), colnames(xlist))
  class(gmat) <- "gmatrix"
  return(gmat)
}

InvS4h <- function(xlist) {
  gmat <- GmatrixH(xlist)
  uniq <- apply(gmat, 1, unique)
  len <- nrow(gmat)
  inv.s <- vector(length = len, mode = "list")
  for (i in 1L:len) {
    inv.s[[i]] <- sapply(uniq[[i]], function(x) length(gmat[i, gmat[i, ] == x]))
  }
  inv.s <- 1/unlist(inv.s)
  return(inv.s)
}
