library(ggplot2)
mtcars$cylinder <- as.factor(mtcars$cyl)
qplot(cylinder, mpg, data=mtcars, geom=c("boxplot", "jitter"),
      fill=cylinder,
      main="Box plots with superimposed data points",
      xlab= "Number of Cylinders",
      ylab="Miles per Gallon")



transmission <- factor(mtcars$am, levels=c(0, 1),
                       labels=c("Automatic", "Manual"))
qplot(wt,mpg, data=mtcars,
      color=transmission, shape=transmission,
      geom=c("point", "smooth"),
      method="lm", formula=y~x,
      xlab="Weight", ylab="Miles Per Gallon",
      main="Regression Example")

#分面图
mtcars$cyl <- factor(mtcars$cyl, levels=c(4, 6, 8),
                     labels=c("4 cylinders", "6 cylinders", "8 cylinders"))
mtcars$am <- factor(mtcars$am, levels=c(0, 1),
                    labels=c("Automatic", "Manual"))
qplot(wt,mpg, data=mtcars, facets=am~cyl, size=hp)


data(singer, package="lattice")
qplot(height, data=singer, geom=c("density"),
      facets=voice.part~., fill=voice.part)

#ggplot2 数据分析与图形艺术
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds),100),]
dsmall
#基本散点图
qplot(carat,price,data=diamonds)
qplot(log(carat),log(price),data=diamonds)
#按不同的分类添加颜色或不同的形状
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,shape=cut)
#设置透明度
qplot(carat,price,data=dsmall,alpha=I(1/10))
#添加平滑曲线
qplot(carat,price,data=dsmall,geom=c('point','smooth'))
qplot(carat,price,data=diamonds,geom=c('point','smooth'))
#不同的拟合方法
qplot(carat,price,data=diamonds,geom=c('point','smooth'),method='gam',formula=y~s(x))
#扰动图