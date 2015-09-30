#R数据可视化手册 ggplot2数据分析与图形艺术
#https://github.com/cosname/ggplot2-translation/blob/master/Rcode
library(ggplot2)
library(mgcv)
library(splines)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds),100),]
dsmall
#quick plot qplot
qplot(carat,price,data=diamonds)
#对变量进行变换
qplot(log(carat),log(price),data=diamonds)
#体积和重量之间的关系
qplot(carat,x*y*z,data=diamonds)
#关系图中加入颜色和切工信息，而在plot里面需要手工绘制细节
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,shape=cut)
qplot(carat,price,data=dsmall,shape=cut,color=color)
#设置透明度
qplot(carat,price,data=diamonds,alpha=I(1/10))
qplot(carat,price,data=diamonds,alpha=I(1/100))
#改变geom几何对象，产生各种图形
#加入拟合曲线,通过methd改变不同的拟合方法 loess是局部回归,数据量小时适用
qplot(carat,price,data=dsmall,geom=c('point','smooth'))
qplot(carat,price,data=diamonds,geom=c('point','smooth'))
#通过span来控制平滑度，参见?loess

qplot(carat,price,data=dsmall,geom=c('point','smooth'),span=0.2)
qplot(carat,price,data=dsmall,geom=c('point','smooth'),span=1)
#大数据可以用广义可加 gam
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='gam',formular=y~s(x))
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='gam',formular=y~s(x,bs='cs'))
#线性回归
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='lm')
#自然样条
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='lm',formular=y~ns(x,5))

## 利用扰动点图(左)和箱线图(右)来考察以颜色为条件的每克拉价格的分布。
## 随着颜色的改变(从左到右)，每克拉价格的跨度逐渐减小，但分布的中位数没有明显的变化。
qplot(color, price/carat, data = diamonds, geom = "jitter")
qplot(color, price/carat, data = diamonds, geom = "boxplot")
#改变 alpha 的取值，从左到右分别为 1/5，1/50 和
## 1/200。随着不透明度的降低，
## 我们可以看出数据集中的地方。然而，箱线图依然是一个更好的选择。
qplot(color, price/carat, data = diamonds, geom = "jitter", alpha = I(1/5))
qplot(color, price/carat, data = diamonds, geom = "jitter", alpha = I(1/50))
qplot(color, price/carat, data = diamonds, geom = "jitter", alpha = I(1/200))
# 展示钻石重量的分布。使用的是 geom='histogram'直方图
## geom=' density' 密度图。
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")
#变动直方图的组距可以显示出有意思的模式。从左到右，组距分别为
## 1，0.1 和 0.01。只有重量在 0 到 3 克拉之间的钻石显示在图中。
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, xlim = c(0, 3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim = c(0,3))
## 当一个分类变量被映射到某个图形属性上，几何对象会自动按这个变量进行拆分。
## 左图是重叠的密度曲线图，右图是堆叠起来的直方图。
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)
# 钻石颜色的条形图。左图显示的是分组的计数，右图是按 weight=carat
## 进行加 权，展示了每种颜色的钻石的总重量。
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")
## 衡量失业程度的两张时序图。左图是失业人口的比例，右图是失业星期数的中位
## 数。图形是用 geom='line' 进行绘制的。
qplot(date, unemploy/pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

## 展示失业率和失业时间长度之间关系的路径图。左图是重叠在一起的的散点图和路
## 径图，右图只有路径图，其中年份用颜色进行了展示。
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy/pop, uempmed, data = economics, geom = c("point", "path"))
qplot(unemploy/pop, uempmed, data = economics, geom = "path", colour = year(date))
## 展示以颜色为条件的重量的直方图。左图展示的是频数，右图展示的是频率。频率
## 图可以使得比较不同组的分布时不会受该组样本量大小的影响。高质量的钻石
## (颜色 D) 在小
## 尺寸上的分布是偏斜的，而随着质量的下降，重量的分布会变得越来越平坦。
#..density..告诉ggplot2，将密度而不是频率映射到y轴
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, 
      xlim = c(0, 3))

qplot(carat, ..density.., data = diamonds, facets = color ~ ., geom = "histogram", 
      binwidth = 0.1, xlim = c(0, 3))
#其他属性
qplot(carat, price, data = dsmall, xlab = "Price ($)", ylab = "Weight (carats)", 
      main = "Price-weight relationship")

qplot(carat, price/carat, data = dsmall, ylab = expression(frac(price, carat)), 
      xlab = "Weight (carats)", main = "Small diamonds", xlim = c(0.2, 1))

qplot(carat, price, data = dsmall, log = "xy") 


#发动机排量(以升为单位 displ)对高速公路耗油量(英里每加仑
                           ## hwy)散点图。点
                           ## 根据汽缸数目着色。该图可以发现影响燃油经济性最重要的因素：发动机排量大 ## 小。
qplot(displ, hwy, data = mpg, colour = factor(cyl))
                           
#这幅图在图3.1的基础上对每个
## 组添加了回归线。这个图应该叫什么名字呢？
qplot(displ, hwy, data = mpg, colour = factor(cyl)) + geom_smooth(data = subset(mpg, cyl != 5), method = "lm")
#一个含有分面和多个图层的复杂图形
qplot(displ, hwy, data = mpg, facets = . ~ year) + geom_smooth()
# 四种不同标度的图例。从左到右依次是:连续型变量映射到大小和颜色，离散型变
## 量映射到形状和颜色。
x <- 1:10
y <- factor(letters[1:5])
qplot(x, x, size = x)
qplot(x, x, 1:10, colour = x)
qplot(y, y, 1:10, shape = y)
qplot(y, y, 1:10, colour = y)
## 三种不同坐标系的坐标轴和网格线:笛卡尔(Cartesian)、半对数(semi-log)和极
## 坐标系(polar)。极坐标系展示了非笛卡尔坐标系的缺点:很难画好坐标轴。
x1 <- c(1, 10)
y1 <- c(1, 5)
p <- qplot(x1, y1, geom = "blank", xlab = NULL, ylab = NULL) + theme_bw()
p
p + coord_trans(y = "log10")
p + coord_polar()

p <- qplot(displ, hwy, data = mpg, colour = factor(cyl))
summary(p)
# 保存图形对象
#save(p, file = "plot.rdata")
# 读入图形对象
#load("plot.rdata")
# 将图片保存成png格式
#ggsave("plot.png", width = 5, height = 5) 

## 通过ggplot创建图形对象 aes中放入图形属性映射信息,此时无法显示需要加入一个图层，包含几何对象
p <- ggplot(diamonds, aes(carat, price, colour = cut))
## 添加“点”几何对象
p <- p + layer(geom = "point")

## 例：手动创建图形对象并添加图层
p <- ggplot(diamonds, aes(x = carat))
p <- p + layer(geom = "bar", geom_params = list(fill = "steelblue"), stat = "bin", 
               stat_params = list(binwidth = 2))
p
#以上代码过于细致，可以用现成的函数生成一定统计变换或几何对象的默认图层
## 应用“快捷函数”，得到与上例相同的图形
p + geom_histogram(binwidth = 2, fill = "steelblue")

# 在用ggplot创建的图形对象上添加图层
ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point()
# 等价于
qplot(sleep_rem/sleep_total, awake, data = msleep)

# 也可以给qplot添加图层
qplot(sleep_rem/sleep_total, awake, data = msleep) + geom_smooth()
# 等价于
qplot(sleep_rem/sleep_total, awake, data = msleep, geom = c("point", "smooth"))
# 或
ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point() + geom_smooth()

## 例：summary给出图形对象的默认设置和每个图层的信息
p <- ggplot(msleep, aes(sleep_rem/sleep_total, awake))
summary(p)
p <- p + geom_point()
summary(p)
## 例：用不同的数据初始化后添加相同的图层
library(scales)
bestfit <- geom_smooth(method = "lm", se = F, colour = alpha("steelblue", 0.5), 
                       size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit
