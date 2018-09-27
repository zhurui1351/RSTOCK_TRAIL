#sigma=matrix(c(0.931,1.68,1.14,1.68,3.048,2.164,1.14,2.164,2.361),3,3)
#MTS VARMAsim
#posdefify sfsmisc
x = matrix(rnorm(9),3,3)
sigma = x %*% t(x)

u = matrix(c(1.3,3.5,-3),3,1)

e = matrix(rnorm(3),3,1)



q = chol(sigma)
u + q%*%e

function(n = 1, mu, Sigma)
{
  
}

function (n = 1, mu, Sigma) 
{
  p <- length(mu)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
 
  X <- matrix(rnorm(p), p)
  
  X <- mu + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% X
  return(X)
}


function (w, A, C, N) 
{
  m = dim(A)[1]
  p = (dim(A)[2])/m
  At = matrix(nrow = m * p, ncol = m * p)
  if (p == 1) {
    At = A
  }
  else {
    At[seq(1, m), seq(1, m * p)] = A
    At[seq(m + 1, m * p), seq(1, m * p - m)] = diag(1, (p - 
                                                          1) * m)
    At[seq(m + 1, m * p), seq(m * p - m + 1, m * p)] = 0
  }
  l = (eigen(At, only.values = TRUE))$values
  if ((any(Mod(l) > 1))) 
    warning("unstable AR model")
  nd = 1000
  U = chol(C)
  require(MASS)
  noisevec = mvrnorm(nd + N, rep(0, m), C)
  matw = rep(1, nd + N) %*% t(w)
  vec = noisevec + matw
  if (any(w != 0)) {
    B = diag(1, m)
    for (j in seq(1, p)) {
      B = B - A[, seq(m * j - m + 1, j * m)]
    }
    mproc = as.vector(solve(B) %*% w)
    xi = (matrix(1, nrow = p, ncol = 1)) %*% mproc
  }
  else {
    xi = matrix(nrow = p, ncol = m)
    xi[, ] = 0
  }
  u = matrix(nrow = p + nd + N, ncol = m)
  u[seq(1, p), seq(1, m)] = xi
  u[seq(p + 1, p + nd + N), seq(1, m)] = 0
  Atr = t(A)
  x = matrix(ncol = m, nrow = p)
  for (k in seq(p + 1, nd + N + p)) {
    for (j in seq(1, p)) {
      x[j, ] = u[k - j, ] %*% Atr[seq(m * j - m + 1, m * 
                                        j), ]
    }
    u[k, ] = as.matrix(apply(x, 2, sum) + vec[k - p, ])
  }
  v = u[seq(nd + p + 1, nd + p + N), ]
  simulated = data.frame(v[, seq(1, m)])
  return(simulated)
}



function (nobs, arlags = NULL, malags = NULL, cnst = NULL, phi = NULL, 
          theta = NULL, skip = 200, sigma) 
{
  if (!is.matrix(sigma)) 
    sigma = as.matrix(sigma)
  k = nrow(sigma)
  nT = nobs + skip
  at = rmvnorm(nT, rep(0, k), sigma)
  nar = length(arlags)
  p = 0
  if (nar > 0) {
    arlags = sort(arlags)
    p = arlags[nar]
  }
  q = 0
  nma = length(malags)
  if (nma > 0) {
    malags = sort(malags)
    q = malags[nma]
  }
  ist = max(p, q) + 1
  zt = matrix(0, nT, k)
  if (length(cnst) == 0) 
    cnst = rep(0, k)
  for (it in ist:nT) {
    tmp = matrix(at[it, ], 1, k)
    if (nma > 0) {
      for (j in 1:nma) {
        jdx = (j - 1) * k
        thej = theta[, (jdx + 1):(jdx + k)]
        atm = matrix(at[it - malags[j], ], 1, k)
        tmp = tmp - atm %*% t(thej)
      }
    }
    if (nar > 0) {
      for (i in 1:nar) {
        idx = (i - 1) * k
        phj = phi[, (idx + 1):(idx + k)]
        ztm = matrix(zt[it - arlags[i], ], 1, k)
        tmp = tmp + ztm %*% t(phj)
      }
    }
    zt[it, ] = cnst + tmp
  }
  zt = zt[(1 + skip):nT, ]
  at = at[(1 + skip):nT, ]
  VARMAsim <- list(series = zt, noises = at)
}