#http://www.r-bloggers.com/aic-bic-vs-crossvalidation/
crossvalidate <- function(x, y, degree)
{
  preds <- numeric(length(x))
  for(i in 1:length(x))
  {
    x.in <- x[-i]
    x.out <- x[i]
    y.in <- y[-i]
    y.out <- x[i]
    m <- lm(y.in ~ poly(x.in, degree=degree) )
    new <- data.frame(x.in = seq(-3, 3, by=0.1))
    preds[i]<- predict(m, newdata=data.frame(x.in=x.out))
  }
  # the squared error:
  return(sum((y-preds)^2))
}

# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)

# generate the x predictor
x <- runif(100,-2,2)
# generate the y response
y <- 2*x^3 + x^2 - 2*x +5 + rnorm(100)
xy <- data.frame(x=x, y=y)
# specify the maximum polynomial degree that will be explored
max.poly <- 7

# cretaing data.frame which will store model predictions
# that will be used for the smooth curves in Fig. 1
x.new <- seq(min(x), max(x), by=0.1)
degree <- rep(1:max.poly, each=length(x.new))
predicted <- numeric(length(x.new)*max.poly)
new.dat <- data.frame(x=rep(x.new, times=max.poly),
                      degree,
                      predicted)

# fitting lm() polynomials of increasing complexity
# (up to max.degree) and storing their predictions
# in the new.dat data.frame
for(i in 1:max.poly)
{
  sub.dat <- new.dat[new.dat$degree==i,]
  new.dat[new.dat$degree==i,3] <- predict(lm(y~poly(x, i)),
                                          newdata=data.frame(x=x.new))
}

# plotting the data and the fitted models
p <- ggplot()
p + geom_point(aes(x, y), xy, colour="darkgrey")
p + geom_line(aes(x, predicted,
                  colour=as.character(degree)),
              new.dat)
p + scale_colour_discrete(name = "Degree")
p

# creating empty data.frame that will store
# AIC and BIC values of all of the models
AIC.BIC <- data.frame(criterion=c(rep("AIC",max.poly),
                                  rep("BIC",max.poly)),
                      value=numeric(max.poly*2),
                      degree=rep(1:max.poly, times=2))

# calculating AIC and BIC values of each model
for(i in 1:max.poly)
{
  AIC.BIC[i,2] <- AIC(lm(y~poly(x,i)))
  AIC.BIC[i+max.poly,2] <- BIC(lm(y~poly(x,i)))
}

# function that will perform the "leave one out"
# crossvalidation for a y~poly(x, degree) polynomial
crossvalidate <- function(x, y, degree)
{
  preds <- numeric(length(x))
  for(i in 1:length(x))
  {
    x.in <- x[-i]
    x.out <- x[i]
    y.in <- y[-i]
    y.out <- x[i]
    m <- lm(y.in ~ poly(x.in, degree=degree) )
    new <- data.frame(x.in = seq(-3, 3, by=0.1))
    preds[i]<- predict(m, newdata=data.frame(x.in=x.out))
  }
  # the squared error:
  return(sum((y-preds)^2))
}

# crossvalidating all of the polynomial models
# and storing their squared errors in
# the "a" object
a <- data.frame(cross=numeric(max.poly))
for(i in 1:max.poly)
{
  a[i,1] <- crossvalidate(x, y, degree=i)
}

# plotting AIC and BIC against model complexity
# (which is the polynomial degree)
AIC.plot <- qplot(degree, value, data=AIC.BIC,
                  geom="line", linetype=criterion) +
  xlab("Polynomial degree") +
  ylab("Criterion value") +
  labs(title="Information theory & Bayes")+
  geom_segment(aes(x=3, y=400,
                   xend=3, yend=325),
               arrow = arrow(length = unit(0.3, "cm"),
                             angle=20, type="closed")) +
  theme(legend.position=c(0.8,0.5))
AIC.plot

# plotting crossvalidated squared errors agains
# model complexity
cross.plot <- qplot(1:max.poly,cross, data=a, geom=c("line"))+
  xlab("Polynomial degree") +
  ylab("Squared error") +
  geom_segment(aes(x=3, y=400,
                   xend=3, yend=200),
               arrow = arrow(length = unit(0.3, "cm"),
                             angle=20, type="closed")) +
  labs(title="Crossvalidation")
cross.plot