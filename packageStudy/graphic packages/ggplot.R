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
qplot(color,price/carat,data=diamonds,geom='jitter',alpha=I(1/5))
#直方图
qplot(carat,data=diamonds,geom='histogram')
qplot(carat,data=diamonds,geom='density')
#不同的宽度
qplot(carat,data=diamonds,geom='histogram',binwidth=1,xlim=c(0,3))
qplot(carat,data=diamonds,geom='histogram',binwidth=0.1,xlim=c(0,3))
#分组
qplot(carat,data=diamonds,geom='histogram',binwidth=0.1,xlim=c(0,3),fill=color)
qplot(carat,data=diamonds,geom='density',binwidth=0.1,xlim=c(0,3),color=color)
#使用economics数据集绘制时间图
qplot(date,unemploy/pop,data=economics,geom='line')
qplot(date,unemploy,data=economics,geom='line')
#路径图
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy/pop,uempmed,data=economics,geom=c('point','path'))
qplot(unemploy/pop,uempmed,data=economics,geom=c('point','path'),color=year(date))
#分面,将图形分成若干子集
qplot(carat,data=diamonds,facets=color~.,geom='histogram',binwidth=0.1,xlim=c(0,3))
qplot(carat,..density..,data=diamonds,facets=color~.,geom='histogram',binwidth=0.1,xlim=c(0,3))
#添加平滑曲线层
qplot(displ,hwy,data=mpg,facets=.~year) + geom_smooth()

#ggplot 按图层绘图
#映射图形属性
p = ggplot(diamonds,aes(carat,price,colour=cut))
#添加图层
p <-p +layer(geom='point')
#复杂图层
p = ggplot(diamonds,aes(x=carat))
p = p + layer(geom="bar",geom_params=list(fill='steelblue'),stat='bin',stat_params=list(binwidth=2))
summary(p)
ggplot(msleep,aes(sleep_rem/sleep_total,awake)) + geom_point()

#图层对象可以保存为R对象，替换数据集后可以复用
p = ggplot(mtcars,aes(mpg,wt,color=cyl)) + geom_point()
p
mtcar <- transform(mtcars,mpg=mpg^2)
p %+% mtcar
#设定和映射的区别
p = ggplot(mtcars,aes(mpg,wt))
p + geom_point(color='darkblue')
p + geom_point(aes(color="darkblue"))
#分组
require('nlme')
head(Oxboys)
p = ggplot(Oxboys,aes(age,height,group=Subject)) + geom_line()
p + geom_smooth(aes(group=Subject),method='lm',se=F)
p + geom_smooth(aes(group=1),method='lm',size=2,se=F)
#统计变换
d <- ggplot(diamonds,aes(carat)) + xlim(0,3)
d + stat_bin(aes(ymax = ..count..),binwidth=0.1,geom='area')
d + stat_bin(aes(size = ..density..),binwidth=0.1,geom='point',position='identity')
d + stat_bin(aes(y=1,fill = ..count..),binwidth=0.1,geom='tile',position='identity')


df = data.frame(
  x = c(3,1,5),y=c(2,4,6),label=c('a','b','c')
  )
p = ggplot(df,aes(x,y)) + xlab(NULL) + ylab(NULL)
p + geom_point() + labs(title='geom_point')
p + geom_bar(stat='identity') + labs(title='geom_bar(stat=identity)')
p + geom_line() + labs(title='geom_line')
p + geom_area() + labs(title='area')
p + geom_path() + labs(title='path')
p + geom_text(aes(label=label)) + labs(title='text')
p + geom_tile() + labs(title='tile')
p + geom_polygon() + labs(title='polygon')