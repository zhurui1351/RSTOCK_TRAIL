#http://datascienceplus.com/managing-longitudinal-data-conversion-between-the-wide-and-the-long/
#http://datascienceplus.com/analysing-longitudinal-data-multilevel-growth-models-ii/
#longitudinal dataset in wide-format
library(MASS)
dat.tx.a <- mvrnorm(n=250, mu=c(30, 20, 28), 
                    Sigma=matrix(c(25.0, 17.5, 12.3, 
                                   17.5, 25.0, 17.5, 
                                   12.3, 17.5, 25.0), nrow=3, byrow=TRUE))
dat.tx.b <- mvrnorm(n=250, mu=c(30, 20, 22), 
                    Sigma=matrix(c(25.0, 17.5, 12.3, 
                                   17.5, 25.0, 17.5, 
                                   12.3, 17.5, 25.0), nrow=3, byrow=TRUE))
dat <- data.frame(rbind(dat.tx.a, dat.tx.b))
names(dat) = c("measure.1", "measure.2", "measure.3")
dat <- data.frame(subject.id=factor(1:500), tx=rep(c("A", "B"), each=250), dat)
rm(dat.tx.a, dat.tx.b)
#wide to long
dat <- reshape(dat, varying=c("measure.1", "measure.2", "measure.3"), 
               idvar="subject.id", direction="long")

ggplot(dat, aes(x=time,y=measure,colour=tx,group=subject.id))+geom_line(alpha=.5)

#长表转宽表
dat <- reshape(dat, v.names="measure", timevar="time", 
               idvar="subject.id", direction="wide")
with(dat, t.test(measure.2~tx))
