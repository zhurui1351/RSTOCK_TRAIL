#ou have an urn with 50 white balls and 9 black balls. The black balls are individually numbered. Balls are drawn from the urn without replacement. What is the probability that
# the last ball drawn from the urn is a black ball (Event 1) and
#when the urn is refilled, the first ball drawn will be the
#sameblack ball (Event 2).
NBALLS = 59
NWHITE = 50
NBLACK = NBALLS - NWHITE
P2 = 1 / NBALLS
P2
#超几何分布的概率密度
P1 = dhyper(NWHITE, NWHITE, NBLACK, NBALLS-1)
P1 * P2
#模拟
BAG <- c(rep(NA, NWHITE), 1:NBLACK)
NSIM <- 1000000
hits <- lapply(1:NSIM, function(n) {
  last <- sample(BAG)[NBALLS]
  first <- sample(BAG, 1)
  last == first
  })
hits <- unlist(hits)
# The resulting vector has entries with the following possible values
# TRUE -> last ball is black and is the same ball that comes out first from repopulated bag
# FALSE -> last ball is black and first ball from repopulated ball is black but different
# NA -> either of these balls is white
hits <- ifelse(is.na(hits), FALSE, hits)
library(binom)
stats <- binom.confint(x = cumsum(hits), n = 1:NSIM, methods = "wilson")
NBLACK / NBALLS
