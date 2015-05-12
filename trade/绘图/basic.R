data(state)
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

plot(dose, drugA, type="b",
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A",
     sub="This is hypothetical data",
     xlab="Dosage", ylab="Drug Response",
     xlim=c(0, 60), ylim=c(0, 70))
#添加说明
title(main="main title", sub="sub-title",
      xlab="x-axis label", ylab="y-axis label")
#自定义轴
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)
par(mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE)
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)

#自定义坐标刻度
#xaxt="n" x坐标不输出刻度
plot(1:5,xaxt="n")
#自定义x轴坐标刻度
axis(1,c(seq(1,1.8,by=0.2),2:5),c(seq(1,1.8,by=0.2),2:5))

#在图上添加直线
abline(h=c(1,5,7))
#添加竖线
abline(v=seq(1, 10, 2), lty=2, col="blue")
#添加说明
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))

#添加文字信息
attach(mtcars)
plot(wt, mpg,
     main="Mileage vs. Car Weight",
     xlab="Weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg,
     row.names(mtcars),
     cex=0.6, pos=4, col="red")
detach(mtcars)

#数学符号
demo(plotmath)

#mfcol 和 mfrow 可以分割画图区域 做多个联合图形
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)
#用layout可以指定一些比较复杂的多图布局
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
#显示当前布局 layout.show(1)
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#精确控制绘图位置 使用fig= 参数
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(fig=c(0, 0.8, 0, 0.8), new=TRUE)
plot(wt,mpg, xlab="Miles per callon",ylab="car weight")
par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
boxplot(wt,horizontal = T,axes=F)

par(fig=c(0.65, 1, 0, 0.8), new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
par(opar)
detach(mtcars)


#--------------------------basic gragh-------------------------------------
#bar 
#装载关节炎病患
library(vcd)
#得到每种类型的计数
counts <- table(Arthritis$Improved)
barplot(counts,
        main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")
#水平并列,当然对于因子也可以直接使用plot
barplot(counts,
        main="Horizontal Bar Plot",
        xlab="Frequency", ylab="Improvement",
        horiz=TRUE)
#栈图和分组图
counts <- table(Arthritis$Improved, Arthritis$Treatment)
#堆栈图
barplot(counts,
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow","green"),
        legend=rownames(counts))
#分组图
barplot(counts,
        main="Grouped Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)
#从小到大画平均收入图
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
#排序
means <- means[order(means$x),]
barplot(means$x, names.arg=means$Group.1)
title("Mean Illiteracy Rate")
#旋转
par(mar=c(5,8,4,2))
par(las=2)
counts <- table(Arthritis$Improved)
barplot(counts,
        main="Treatment Outcome",
        horiz=TRUE, cex.names=0.8,
        names.arg=c("No Improvement", "Some Improvement",
                    "Marked Improvement"))

library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main="Spinogram Example")
detach(Arthritis)

#饼图
par(mfrow=c(2, 2))
#基本饼图
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie( slices, labels = lbls,
     main="Simple Pie Chart")

#百分比图
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep="")
pie(slices, labels=lbls2, col=rainbow(length(lbls2)), 
    main="Pie Chart with Percentages")
#3D
library(plotrix)
pie3D(slices, labels=lbls,explode=0.1,
      main="3D Pie Chart ")

mytable <- table(state.region)
lbls3 <- paste(names(mytable), "\n", mytable, sep="") 
pie(mytable, labels = lbls3,
    main="Pie Chart from a Table\n (with sample sizes)")

#fan 图,可以更好的看出差异,各个扇形图相互重叠，容易看出差异
library(plotrix)
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
fan.plot(slices, labels = lbls, main="Fan Plot")

#直方图
par(mfrow=c(2,2))
hist(mtcars$mpg)
hist(mtcars$mpg,
     breaks=12, 
     col="red",
     xlab="Miles Per Gallon",
     main="Colored histogram with 12 bins")
#概率密度图
hist(mtcars$mpg,
     freq=FALSE, 
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Histogram, rug plot, density curve")

rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col="blue", lwd=2)
#与正态分布对比
x <- mtcars$mpg 
h<-hist(x,
        breaks=12,
        col="red",
        xlab="Miles Per Gallon",
        main="Histogram with normal curve and box")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()
#核密度
par(mfrow=c(2,1))
d <- density(mtcars$mpg)
plot(d)
d <- density(mtcars$mpg)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")
rug(mtcars$mpg, col="brown")
#对核密度进行对比
par(lwd=2)
library(sm) 
attach(mtcars)
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", 
                           "8 cylinder"))
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")  
title(main="MPG Distribution by Car Cylinders")
colfill<-c(2:(1+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)  
detach(mtcars)

#Box图
boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallon")
#分组比较
boxplot(mpg ~ cyl, data=mtcars,
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")
#漏斗图
boxplot(mpg ~ cyl, data=mtcars,
        notch=TRUE,
        varwidth=TRUE,
        col="red",
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")
#多因素分组
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels=c(4,6,8),
                       labels=c("4","6","8"))
mtcars$am.f <- factor(mtcars$am,
                      levels=c(0,1),
                      labels=c("auto", "standard"))
boxplot(mpg ~ mtcars$am.f *mtcars$cyl.f,
        data=mtcars,
        varwidth=TRUE,
        col=c("gold","darkgreen"),
        main="MPG Distribution by Auto Type",
        xlab="Auto Type")
#vioplot 联合box和density
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]

vioplot(x1, x2, x3,
        names=c("4 cyl", "6 cyl", "8 cyl"),
        col="gold")
title("Violin Plots of Miles Per Gallon")
#点图，在水平轴上画
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7,
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon")
#分组 排序后的点图
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")