call.pnls <- function(nr, nc, np, x, y, const) {
  fit <- nlminb(start = rnorm(nr * (nc - 1)), 
               obj = call.difp,  
               nr = nr, nc = nc, np = np, x = x,
               y = y, const = const, hessian = TRUE)
  fit$par
}

call.difp <- function(par, x, y, nr, nc, np, const){
  diff <- 0
  a <- .Fortran("difp",
		nr = nr,
		nc = nc,
		np = np,
		x = x,
		y = y,
		p = par,
		const = const,
		diff = diff,
		CLASSES = c(rep("integer", 3), rep("numeric", 5)))
  return(a$diff)
}

ss.nc <- function(g, x, y, np, nr, nc) {
  expo <- array(0, dim = c(nr, nc - 1, np))
  eb <- array(0, dim = c(nr, nc - 1, np))
  m <- matrix(0, np, nc - 1)
  m.der <- array(0, dim = c(nr * (nc - 1), nc - 1, np))
  grad <- matrix(0, nr * (nc - 1), np)
  v <- matrix(0, nr * (nc - 1), nr * (nc - 1))
  del.G <- matrix(0, nr * (nc - 1), nr * (nc - 1))
  for(i in 1:nr)
    for(j in 1:(nc - 1)) {
      expo[i, j,  ] <- exp(g[i, j])
    }
  for(i in 1:nr) {
    s <- 1 + apply(expo[i,  ,  ], 2, sum)
    for(j in 1:(nc - 1)) {
      eb[i, j,  ] <- expo[i, j,  ]/s
    }
  }
  for(j in 1:(nc - 1))
    for(i in 1:nr) {
      m[, j] <- m[, j] + x[, i] * eb[i, j,  ]
    }
  for(j in 1:(nc - 1)) {
    ncount <- 0
    for(i in 1:nr) {
      s <- 1 + apply(expo[i,  ,  ], 2, sum)
      for(k in 1:(nc - 1)) {
        ncount <- ncount + 1
        if(k == j) {
          if(nc > 3)
            s1 <- 1 + apply(expo[i,  - k, ], 2, sum)
          else s1 <- 1 + expo[i,  - k,  ]
          m.der[ncount, j,  ] <- (expo[i, j, ] * x[, i] * s1)/s^2
        }
        else {
          m.der[ncount, j,  ] <- ( - x[, i] *
                                  expo[i, j,  ] * expo[i, k,
                                                       ])/s^2
        }
      }
    }
  }
  ncount <- 0
  for(i in 1:nr)
    for(j in 1:(nc - 1)) {
      ncount <- ncount + 1
      for(k in 1:(nc - 1)) {
        grad[ncount,  ] <- grad[ncount,  ] + (y[, k] -
                                              m[, k]) * m.der[ncount, k,  ]
      }
      grad[ncount,  ] <- -2 * grad[ncount,  ]
    }
  for(i in 1:np) {
    v <- v + grad[, i] %*% t(grad[, i])
  }
  for(i in 1:np)
    for(j in 1:(nc - 1)) {
      del.G <- del.G + 2 * m.der[, j, i] %*% t(m.der[, j,
                                                     i])
    }
	#svd.v <- svd(v)
	#v.inv <- svd.v$v %*% diag(1/svd.v$d) %*% t(svd.v$u)
	#svd.b <- svd(t(del.G) %*% v.inv %*% del.G)
	#big.inv <- svd.b$v %*% diag(1/svd.b$d) %*% t(svd.b$u)
	#	sqrt(diag(big.inv))
	#	v.inv <- solve(v)
	#	sqrt(diag(solve(t(del.G) %*% v.inv %*% del.G)))
	#svd.G <- svd(del.G)
	#G.inv <- svd.G$v %*% diag(1/svd.G$d) %*% t(svd.G$u)
	#diag(G.inv %*% v %*% t(G.inv))
  del.G.inv <- solve(del.G)
  sqrt(diag(del.G.inv %*% v %*% t(del.G.inv)))
}

find.par.nc <- function(p, nr, nc) {
  gam <- matrix(0, nr, nc - 1)
  ncount <- 0
  for(i in 1:nr)
    for(j in 1:(nc - 1)) {
      ncount <- ncount + 1
      gam[i, j] <- p[ncount]
    }
  gam
}

pred.mean.nc <- function(g, nr, nc) {
  expo <- matrix(0, nrow = nr, ncol = nc - 1)
  eb <- matrix(0, nrow = nr, ncol = nc)
  for(i in 1:nr)
    for(j in 1:(nc - 1)) {
      expo[i, j] <- exp(g[i, j])
    }
  for(i in 1:nr) {
    s <- 1 + sum(expo[i,  ])
    for(j in 1:(nc - 1)) {
      eb[i, j] <- expo[i, j]/s
    }
    eb[i, nc] <- 1 - sum(eb[i,  ])
  }
  eb
}

