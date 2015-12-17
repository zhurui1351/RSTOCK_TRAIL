climb <- function(y, x0) {
  x <- index(y)
  n <- length(y)
  
  if (n == 0) { return(NA) }
  
  if (x0 < min(x) | x0 > max(x)) {
    warning(sprintf("x0 of %f lies outside data range [%f, %f]", x0, min(x), max(x)))
  }
  
  # TODO: beautify this.
  w <- which.min(abs(as.numeric(x - x0)))
  i <- x[w]
  ii <- index(x)[w] # 1-based index
  l <- x[ii-1]
  r <- x[ii+1]
  yl <- if (ii == 1) { -Inf } else { as.numeric(y[l]) }
  yr <- if (ii == n) { -Inf } else { as.numeric(y[r]) }
  yi <- as.numeric(y[i])
  
  # At a maximum?
  if (yl <= yi && yr <= yi) {
    return(i)
  }
  
  # Nope; go in the direction of greatest increase, breaking ties to the right.
  if (yl > yr) {
    return(climb(y, l))
  } else {
    return(climb(y, r))
  }
}