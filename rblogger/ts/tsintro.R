#http://zoonek2.free.fr/UNIX/48_R/15.html#1
plot(LakeHuron,
     ylab = "",
     main = "Level of Lake Huron")

x <- window(sunspots, start=1750, end=1800)
plot(x,
     ylab = "",
     main = "Sunspot numbers")

plot(x, 
     type = 'p', 
     ylab = "",
     main = "Sunspot numbers")
k <- 20
lines( filter(x, rep(1/k,k)), 
       col = 'red', 
       lwd = 3 )

data(UKgas)
plot.band <- function (x, ...) {
  plot(x, ...)
  a <- time(x)
  i1 <- floor(min(a))
  i2 <- ceiling(max(a))
  y1 <- par('usr')[3]
  y2 <- par('usr')[4]
  if( par("ylog") ){
    y1 <- 10^y1
    y2 <- 10^y2
  }
  for (i in seq(from=i1, to=i2-1, by=2)) {
    polygon( c(i,i+1,i+1,i), 
             c(y1,y1,y2,y2), 
             col = 'grey', 
             border = NA )
  }
  par(new=T)
  plot(x, ...)
}
plot.band(UKgas, 
          log = 'y', 
          ylab = "",
          main = "UK gas consumption")



x <- LakeHuron
op <- par(mfrow = c(1,2),
          mar = c(5,4,1,2)+.1,
          oma = c(0,0,2,0))
hist(x, 
     col = "light blue",
     xlab = "",
     main = "")
qqnorm(x,
       main = "")
qqline(x, 
       col = 'red')
par(op)
mtext("Lake Huron levels", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)



x <- diff(LakeHuron)
op <- par(mfrow = c(1,2),
          mar = c(5,4,1,2)+.1,
          oma = c(0,0,2,0))
hist(x, 
     col = "light blue",
     xlab = "",
     main = "")
qqnorm(x,
       main = "")
qqline(x, 
       col = 'red')
par(op)
mtext("Lake Huron level increments", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)


boxplot(x,
        horizontal = TRUE, 
        col = "pink",
        main = "Lake Huron levels")


plot(x,
     ylab = "",
     main = "Lake Huron levels")



n <- length(x)
k <- 5
m <- matrix(nr=n+k-1, nc=k)
colnames(m) <- c("x[i]", "x[i-1]", "x[i-2]",
                 "x[i-3]", "x[i-4]")
for (i in 1:k) {
  m[,i] <- c(rep(NA,i-1), x, rep(NA, k-i))
}
pairs(m, 
      gap = 0,
      lower.panel = panel.smooth,
      upper.panel = function (x,y) {
        panel.smooth(x,y)
        par(usr = c(0, 1, 0, 1))
        a <- cor(x,y, use='pairwise.complete.obs')
        text(.1,.9, 
             adj=c(0,1),
             round(a, digits=2),
             col='blue',
             cex=2*a)
      })
title("Lake Huron levels: autocorrelations", 
      line = 3)


op <- par(mfrow = c(3,1),
          mar = c(2,4,1,2)+.1)
acf(x,      xlab = "")
pacf(x,     xlab = "")
spectrum(x, xlab = "", main = "")
par(op)



op <- par(mfrow = c(3,3),
          mar = .1 + c(0,0,0,0))

n <- 100
k <- 5
N <- k*n
x <- (1:N)/n
y1 <- rnorm(N)
plot(ts(y1), 
     xlab="", ylab="", main="", axes=F)
box()

y2 <- cumsum(rnorm(N))
plot(ts(y2),
     xlab="", ylab="", main="", axes=F)
box()

y3 <- cumsum(rnorm(N))+rnorm(N)
plot(ts(y3),
     xlab="", ylab="", main="", axes=F)
box()

y4 <- cumsum(cumsum(rnorm(N)))
plot(ts(y4),
     xlab="", ylab="", main="", axes=F)
box()

y5 <- cumsum(cumsum(rnorm(N))+rnorm(N))+rnorm(N)
plot(ts(y5),
     xlab="", ylab="", main="", axes=F)
box()

# With a trend
y6 <- 1 - x + cumsum(rnorm(N)) + .2 * rnorm(N)
plot(ts(y6),
     xlab="", ylab="", main="", axes=F)
box()

y7 <- 1 - x - .2*x^2 + cumsum(rnorm(N)) + 
  .2 * rnorm(N)
plot(ts(y7),
     xlab="", ylab="", main="", axes=F)
box()


# With a seasonnal component
y8 <- .3 + .5*cos(2*pi*x) - 1.2*sin(2*pi*x) +
  .6*cos(2*2*pi*x) + .2*sin(2*2*pi*x) +
  -.5*cos(3*2*pi*x) + .8*sin(3*2*pi*x)
plot(ts(y8+ .2*rnorm(N)),
     xlab="", ylab="", main="", axes=F)
box()
lines(y8, type='l', lty=3, lwd=3, col='red')

y9 <- y8 + cumsum(rnorm(N)) + .2*rnorm(N)
plot(ts(y9),
     xlab="", ylab="", main="", axes=F)
box()

par(op)

#acf
my.acf <- function (
  x, 
  lag.max = ceiling(5*log(length(x)))
) {
  m <- matrix( 
    c( NA, 
       rep( c(rep(NA, lag.max-1), x),
            lag.max ),
       rep(NA,, lag.max-1)
    ),
    byrow=T,
    nr=lag.max)
  x0 <- m[1,]
  apply(m,1,cor, x0, use="complete")
}
n <- 200
x <- rnorm(n)
plot(my.acf(x), 
     xlab = "Lag",
     type = 'h')
abline(h=0)

op <- par(mfrow=c(2,1))
acf(x, main="ACF of white noise")
x <- LakeHuron
acf(x, main="ACF of a time series (Lake Huron)")
par(op)

op <- par(mfrow=c(2,1))
set.seed(1)
x <- rnorm(100)
# Default plot
acf(x, main = "ACF with a distracting horizontal line")
# Without the axis, with larger bars
r <- acf(x, plot = FALSE)
plot(r$lag, r$acf, 
     type = "h", lwd = 20, col = "grey",
     xlab = "lag", ylab = "autocorrelation",
     main = "Autocorrelation without the y=0 line")
ci <- .95 
clim <- qnorm( (1+ci) / 2 ) / sqrt(r$n.used)
abline(h = c(-1,1) * clim, 
       lty = 2, col = "blue", lwd = 2)


op <- par(mfrow=c(3,3), mar=c(0,0,0,0))
for (y in sample(list(y1,y2,y3,y4,y5,y6,y7,y8,y9))) {
  acf(y, 
      xlab="", ylab="", main="", axes=F)
  box(lwd=2)
}
par(op)