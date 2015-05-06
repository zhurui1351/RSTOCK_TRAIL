# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 最后修改时间：2014-04-23(17.3)



# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   01、课程介绍 ##########################################################################################

# 课程内容 - 介绍课程的意义、基本结构、课程讲述方法
{
  
# 为什么选择R语言做金融大数据处理？
  
   # R语言是免费、开源、自由的计算平台使用成本低

   # R的基础语法简单、学习速度快、上手容易

   # R语言的数据可视化能力强，提供了丰富的绘图函数

   # 扩展软件包发展速度快、更新快，目前已有数千个扩展包覆盖几乎所有的科学计算领域

   # 虽然运行速度比C/C++等慢，但是可以快速测试各种算法，节约项目实验、研究时间。
   # 很多时候只需一行代码就可以实行一项复杂的功能

   # R语言的学习、研究社区发展快、学习资料丰富

  

# 课程整体结构

   # R博大精深，内容极其丰富，即使单个方向要充分掌握也需要数年时间

   # 本课程的目的是帮助对程序编写有一定了解的学员快速掌握R语言基础知识和金融大数据处理的基本方法

   # 课程整体上分成两部分：《基础篇》 和 《扩展软件包篇》

   # 现在大家看到的10节课属于《基础篇》介绍R语言的基础知识，帮助大家建立基本的知识框架，可以马上上手使用

   # 后期我们还将推出的《扩展软件包篇》课程以R语言中的重要软件包为分类方式，每个软件包用4-6节课做详细介绍



# 本讲座的特点

   # 本讲座不使用PPT而是在RStudio中分享知识、直接运行代码块展示结果，让大家看到立竿见影的效果

   # 贴近实战，在介绍R语言知识的同时中也分享金融大数据处理实战心得
 
   # 因为加百力从2002年就在做数据分析、数据库管理相关工作
   # 近几年建立了自己的公司做对冲基金管理和金融大数据处理工作
   # 所以本讲座还会分享建立数据分析、挖掘公司的理念、工作体系和企业管理经验



# RStudio 重要快捷键

  # Ctrl + L              # 清除控制台输出
  # Ctrl + Enter          # 运行光标所在行的R代码 或者 当前选中行的R代码
  # Ctrl + Shift + S      # 运行当前脚本文件
  # Ctrl + D              # 删除整行

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   02、R基础知识 ##########################################################################################

# 课程内容：介绍：变量赋值、分支结构、循环结构、函数使用、获取帮助等知识

{

# 通过赋值生成一个新变量
x  <- 1.5;print(x)
y1 <- c(1.5,2.3,8.6,7.4,9.2);print(y1)
y2 <- c("MSFT","GOOG","AAPL");print(y2)
y3 <- c(T,F,T,T,F,F);print(y3)

3.1415926 -> z;print(z)               # 数据在左，变量名在右赋值，但比较少用

assign("t",1.414);print(t)            # assign()函数给变量赋值

szSymbolName <- 'GSPC'                # assign()函数的有趣用途：将数值赋值给保存在字符串变量中的变量名
assign(szSymbolName,1860);print(GSPC)


# 可一次输入多个数据
cat("x = ",x,"\n",sep="")


# 显示当前内存中的所有变量、函数名称
ls()

# 删除内存中的指定名称变量
rm(GSPC)

# 删除内存中加载的所有变量和函数，慎用
rm(list=ls())


# 分支结构

a <- 1
if(a==1) print("a==1")


a <- 2
if(a > 1)  print("a > 1")   else   print("a <= 1")


a <- 3
if( a == 1)
{
   print("a == 1")
}else                       # 注意这里，else必须紧跟在上一个大括号后面
{
   print("a != 1")
}


# 多重分支结构，同样每个else必须和前面的}紧紧粘在一起
a <- 4
if( a == 1)
{
   print("a == 1")
}else if( a == 2)
{
   print("a == 2")
}else
{
  print("Not 1 & 2")
}


# ifelse()计算第一个逻辑表达式得到结果如果为T则返回第二个参数；否则返回第三个参数
a <- 2
print( ifelse(a > 1,3.1416,1.414) )


# switch语句的多重分支结构
n <- 1

switch(n,
       print("选项1"),
       print("选项2"),
       print("选项3"),
       print("选项4"),
       print("选项5")
       )



# for 循环结构

iTotal <-  0
for(i in 1:100)   # 使用关键词in枚举向量中的每一整数
{
   iTotal <- iTotal + i
} 
cat("1-100的累加和为：",iTotal,"\n",sep="")



# 字符串也同样可以成功枚举十分方便
szSymbols <- c("MSFT","GOOG","AAPL","INTL","ORCL","SYMC")
for(SymbolName in szSymbols)
{
   cat(SymbolName,"\n",sep="")
}


# while循环
i <- 1
iTotal <- 0
while(i <= 100)
{
   iTotal <- iTotal + i
   i <- i + 1
}
cat("1-100的累加和为：",iTotal,"\n",sep="")


# repeat循环
i <- 1
iTotal <- 0
repeat                               # 无条件循环，必须在程序内部设法退出
{
  iTotal <- iTotal + i
  i <- i + 1
  
  if(i <= 100) next else break       # 注意：next,break的用法
}
cat("1-100的累加和为：",iTotal,"\n",sep="")



# 自定义函数
# 注意：建立功能丰富、庞大、专业的自定义函数库、类库是公司的核心竞争力
#pt <- function() { szCurTime <- as.character.Date(Sys.time()); options(prompt=paste(szCurTime,">",sep="")) }
#pt()


# 定义自己的二元运算符，%anything%，两个百分号之间可以是任何字符串
# 定义二元运算符的过程和编写自定义函数本质相同
"%g%" <- function(x,y)
{
   print(x+y)
   print(x-y)
   print(x*y)
   print(x/y)
}  

3%g%5


# 获取帮助信息

?print            # 在RStudio右侧打开相关帮助界面
example(print)

?quantmod         # 打开扩展包整体帮助信息

apropos("print*")  # 在搜索路径下查找满足正则表达式的所有函数信息

demo(graphics)

# 计算机英语还是比较容易，况且要想拥有国际水平的金融大数据处理能力英语关必须过
# 还可以安装一个金山词霸，一边读帮助文档一边将生词计入生词库，多复习、记忆就好了

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   03、R常用数据结构 ###############################################################################################

# 课程内容：介绍向量、数组、列表、数据框、日期时间等数据结构的基础知识，以及生成这些结构的方法

# R语言的内部数据结构很丰富、使用灵活但是也很复杂比较容易出错。函数和数据结构不对应很容易报错

{

# 向量

x <- c(1,2,3,4,5);print(x)

x <- 1:10;print(x)

x <- seq(from=1,to=10);print(x)

x <- seq(from=1,to=10,by=2);print(x)

x <- seq(from=1,to=10,length.out=50);print(x)



# 生成日期变量并用生成的日期变量生成一组日期序列数据
s <- as.Date("2014-1-1")
e <- as.Date("2014-4-1")
dtVec <- seq(from=s,to=e,by=1)
print(dtVec)
print(class(dtVec))



# 生成一组重复数据。使用常数初始化指定长度的向量
x <- rep(1,10);print(x)

x <- rep("USD",5);print(x)


# sample()取样函数从一组数据中随机取出指定数量的数据
# replace参数决定是否可以重复取数
y <- sample(x=1:100,size=10,replace=F);print(y)

y <- sample(x=c(0,1),size=20,replace=T);print(y)



# 使用正态分布随机数生成函数生成指定数量、平均值、标准差的随机数
x <- rnorm(n=10,mean=100,sd=20);print(x)



# 数组

# 生成向量然后通过设置行列数据转换成二维数组
x <- 1:20
dim(x) <- c(5,4)
print(x)

# 直接生成数组并初始化每个元素为3.14
x <- array(3.14,dim=c(5,4))
print(x)


# 可以生成三维数组
# 使用正态分布随机数初始化整个数组
x <- array(rnorm(40,10,5),dim=c(5,4,2))
print(x)
print(length(x))  # 对于矩阵length()函数得到的是所有元素的总数量



# 数据框
# 数据框的形式和Excel比较相似
# 每一列可以看做是一个向量，类型相同；不同列数据之间可以有不同类型
# 同列表的重要区别是：数据框要求每一列数据长度相同
# 数据框的每一行可以看作是一条记录，每一列看做是记录的某一个属性
# 数据框是金融大数据处理中非常重要的数据结构，用途很广必须深入了解
df  <- data.frame(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency=rep("USD",3),country=rep("USA",3),type=rep("STOCK",3))
df
print(class(df))
cat("df数据框行数为：",nrow(df),"\ndf数据框列数为：",ncol(df),"\n",sep="")
cat("df数据框总元素数量为：",length(df),"\n",sep="")  # 得到的不是总元素个数，也不是行数而是列数（属性）数量


# 列表
# 列表使用list()函数来定义，列表中的每个元素可以是单个变量或者是向量，甚至是另一个列表
# 列表中每个向量的长度可以不同，这是列表和数据框的重要区别
# 数据框是一种形式特殊的列表
lst <- list(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency="USD",country="USA",type="STOCK" )
lst
print(length(lst))  # 共5个元素



# 日期类变量
dtVar <- Sys.Date()
print(class(dtVar));print(dtVar)

dtVar <- Sys.time()
print(class(dtVar));print(dtVar)

dtVar <- as.Date("2014-4-17",tz="UTC")
print(class(dtVar));print(dtVar)

dtVar <- as.Date("2014/4/17",tz="CST")
print(class(dtVar));print(dtVar)



# 用数字直接生成日期对象
# ISOdate()函数得到的是一个POSIXct对象
t <- ISOdate(2014,4,17)
print(class(t));print(t)


# 允许继续加入小时、分钟、秒数信息
t <- ISOdatetime(2014,4,17,15,28,48)
print(class(t));print(t)



# 将Date日期转换成儒略日期
# 在R中这是自1970-1-1以来的天数
d <- as.Date("2014-4-17")
as.integer(d)
julian(d)



# 提取日期中的各个组成部分信息
d <- Sys.time()
p <- as.POSIXlt(d)
print(class(p));print(mode(p));print(p)

print(p$year+1900)   # p$year 自1900年以来的年份
print(p$mon+1)       # p$mon  表示0-11月
print(p$mday)        # p$mday 该月的第几天

print(p$yday+1)      # p$yday 该年的第几天，0-365天，元旦当天是第0天
print(p$wday)        # p$wday 对应周几，0-6，周日为0，其他时间和我们日常习惯一致

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   04、R的数据操作 ###############################################################################################

# 课程内容：R提供了极其灵活的方式，访问、修改、扩充、删除、计算向量、列表、数据框等基础数据结构

{

# 访问向量中元素

x <- 1:10;print(x)

print(x[5])

print(x[-5])

print(x[c(1,5,8)])

print(x[1:5])

print(x[1:5*2])          # 冒号优先级更高，首先得到1-5这5个数据再乘以2

print(x[ x > 5 ])        # 逻辑表达式用x中每一个元素计算逻辑表达式的值做索引，得到的是原数组中x大于5的元素

print(mean(x))
print(x[ x > mean(x) ] ) # 逻辑表达中可以使用函数


names(x) <- LETTERS[1:10];print(x)   # 使用字符常数数组给向量命名，再通过变量名访问向量中的元素
print(x["A"])

View(x)


# 计算向量的长度
print(length(x))



# 修改指定位置的向量元素
x <- 1:10;print(x)
x[5] <- 100;print(x)


# 追加向量元素增加向量长度
x <- 1:10;print(x)
x <- c(x,11,12,13);print(x)

# 直接在向量末尾写入数据
x <- 1:10;print(x)
x[length(x)+1] <- 11;print(x)


# 在向量指定位置插入数据
# 并不是在原来的向量中插入数据而是返回一个新的向量
# after参数表示在原向量中的那个向量后面插入数据
x <- 1:10;print(x)
y <- append(x,11:13,after=0);print(x);print(y)
y <- append(x,11:13,after=5);print(x);print(y)




# 访问列表
lst <- list(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency="USD",country="USA",type="STOCK" )
print(lst[1])
print(lst$symbols)

print(lst[[1]])        # 该列表的第一个元素是一个向量symbols

print(lst$symbols[1])  # 该列表中symbols向量的第一个元素


# 在列表末尾添加一个新的元素TradeDate
names(lst)
lst$TradeDate <- as.Date(rep("2014-4-17",3))
names(lst)


# 在列表任意位置添加一个新的元素
z <- list(plantform = rep("FXCM",3))
lst <- append(lst,z,after=0)
names(lst)
str(lst)


# 删除列表中的指定项
names(lst)
lst$price <- NULL
names(lst)




# 矩阵访问

data(EuStockMarkets)

print(head(EuStockMarkets))
print(class(EuStockMarkets))
print(summary(EuStockMarkets))

print(EuStockMarkets[,"DAX"])    # 输出所有行，DAX列数据，直接使用列名称访问
print(EuStockMarkets[,1])        # 输出所有行，第一列（德国DAX指数）数据，编写大型程序时显然不够直观

# 查看整个矩阵的数据，坏处是还需要手动关闭打开的页面
View(EuStockMarkets)

# rowSums()计算矩阵每一行的和生成一个新向量
rowTotal <- rowSums(EuStockMarkets)
# 以列方式将每一行和向量接在矩阵之后形成有5列的新矩阵
EuStockMarkets <- cbind(EuStockMarkets,rowTotal)
View(EuStockMarkets)

# colSums()计算矩阵每一列的和生成一个新向量
colTotal <- colSums(EuStockMarkets)
# 以行方式将每一行和向量接在矩阵之后形成一个有新累加和行的矩阵
EuStockMarkets <- rbind(EuStockMarkets,colTotal)
print(tail(EuStockMarkets))


# 访问数据框

df  <- data.frame(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency=rep("USD",3),country=rep("USA",3),type=rep("STOCK",3))
df

print(class(df))

print(df[1,])   # 通过行列号访问数据框的一行
print(df[,2])   # 通过行列号访问数据框的一列


print(df$price) # 用列名称是更好、更直观的方式


# 如果需要多次访问数据框中的数据列可以将数据框名称加入到搜索路径中
# 这样就可以直接使用数据列名称
attach(df)

  print(currency)
  print(type)

detach(df)

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   05、R的重要数据分析函数 #######################################################################################

# 课程内容：介绍R中很多重要、常用的数据分析函数

{
  
# 显示加载到内存中的变量和函数名称
ls()

# 显示加载到内存中的变量和函数的详细信息
ls.str()


# 计算返回向量中最大、最小的元素的索引值
x <- 1:100
print(which.max(x))
print(which.min(x))

# 如果向量中存在多个相等的最大、最小值返回的是第一个最大、最小值的索引
x <- c(1,2,5,2,1,5)
print(which.max(x))
print(which.min(x))

# 返回最大、最小值
print(max(x))
print(min(x))



# 对向量进行排序，默认采用升序排序方式
x <- c(3,5,2,8,6,9,7,4)
print(sort(x))
print(sort(x,decreasing=T))   # 默认的降序参数设置为F，如果设置为T则采用降序排序


# 日期字符串向量
szDate <- c("2014-1-1","2014-3-1","2014-3-18","2014-2-14","2014-4-26","2014-4-1","2014-1-24")
# 通过as.Date()函数生成日期序列
# 注意：R语言中很多函数都可以使用向量做参数
t <- as.Date(szDate)
# sort()函数可以对日期序列做排序操作
t <- sort(t);print(t)


# rev()函数对向量做逆序处理
x <- c(3,5,2,8,6,9,7,4)
print(x);print(rev(x))


# 上例中的日期型数据同样可以逆序排列
print(t);print(rev(t))


# 获取游程的信息
x <- c(1,-1,1,1,1,-1,-1,1,1,1,1,1,1,1)
y <- rle(x)
print(mode(y))
str(y)


# 按照大小对向量数据做分类处理，可以用于绘制直方图
x <- rnorm(n=10,mean=10,5)
print(x)
breaks <- c(-20,-10,0,10,20)   # 分割数据的边界数值向量
y <- cut(x,breaks)
print(summary(y))


# 在向量中查找指定数据（数据可用向量形式给出）,此函数返回一个和原向量等长的向量
# 存在待匹配数据的位置上标记为N(第N个待匹配数据就标记N)
x <- c(2,5,1,4,6,4,3)
print(match(x,4))
print(match(x,c(4,5)))


# 计算组合数/排列数
print(choose(5,2))
print(factorial(3))


# 符号函数
# 每日股市上涨下跌收益率有正有负可以用sign()函数生成1,-1,0组成的游程序列
cat("sign()函数运算结果：",sign(0),",",sign(3.14159),",",sign(-1024),sep="")



# 删除向量、矩阵、数据框中的NA值
x <- c(1,2,NA,4,5,NA,7,6,8,9)

y <- na.omit(x);print(y);
print(length(y));print(class(y))

dim(x) <- c(5,2)
print(x)
print(na.omit(x)) # 删除数组中的NA值的规则是删除含有NA值的整行数据

# 交易品的历史数据中可能存在NA数值，使用na.omit()函数将会删除整条历史记录


# 检测向量、矩阵、数据框中是否包含NA数值，如果包含返回错误信息
x <- c(1,2,NA,4,5,NA,7,6,8,9)
y <- na.omit(x)
na.fail(x)
na.fail(y)

dim(x) <- c(2,5)
na.fail(x)


# 去除向量中的重复数据
x <- c(1,2,NA,2,5,NA,4,3,4,9)
print(unique(x))


# subset()从数据框中取出满足条件的子集
# 对于历史行情数据、历史交易记录可以很方便的取出其中感兴趣的数据
data(airquality)
head(airquality)
newset <- subset(airquality,Temp > 80 & Month == 5,select = c(Ozone:Day))
head(newset)


}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   06、R基础绘图技术 ##############################################################################################

# 课程内容： 详细介绍基础的R绘图技术

{

x <- 1:100
y <- 100 + x*5
windows(300,200);plot(y)

windows(300,200);plot(y,type="l")


# 生成100个正态分布随机数并绘图
x <- rnorm(100,10,5)
plot(x)


# 绘制1000个正态分布随机数的频数直方图
x <- rnorm(1000,30,10)
hist(x)



# 使用核密度估计函数density()，在直方图上绘制密度曲线
# hist()函数必须设置freq参数为F才能显示密度曲线
x <- rnorm(1000,30,10)
hist(x,freq=F)
lines(density(x),col="blue")
box()


# 茎叶图很直观的表现出数据的分布情况
x <- rnorm(100,5,1)
stem(x)


# 绘制10个正态分布随机数的条形图
x <- rnorm(10,30,10);barplot(x)
box() # 在当前图上加个方框


# 绘制饼图
x <-1:5;pie(x,col=rainbow(5))
box()


# 绘制箱线图
# 中间黑线为中位数位置；上下框线为上下四分位数位置；上下触须为1.5倍四分位数间距；如果有孤立点表示异常值
x <-rnorm(10,10,3);boxplot(x)


# 绘制向日葵图
data(iris)
sunflowerplot(iris[,3:4])


# 绘制矩阵或数据框的二元图
data(iris)
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])



# 绘制QQ图
# 如果向量为正态分布则数据点基本在一条直线上
x <- rnorm(500,10,5);qqnorm(x)



# 根据指定函数绘制指定范围的曲线图
curve(sin, -2*pi, 2*pi, xname = "t")




# 低水平绘图函数：在高水平绘图函数绘制的图形上做补充和修饰

# 生成50个正态分布随机数并绘图
x <- 1:50
y <- rnorm(50,10,5)
plot(x,y,type="n",xlab="数据索引",ylab="随机数点",xlim=c(1,50),ylim=c(-20,20),main="实验图",sub="随机数绘图")
points(x,y,col="blue")
lines(x,y,col="red")

text(5,max(y),"随机数据点")           # 在指定坐标位置放置文本

mtext("横轴下方文字",side=1)          # 横轴、纵轴上放置文字
mtext("纵轴左方文字",side=2)
mtext("横轴上方文字",side=3)
mtext("纵轴右方文字",side=4)

segments(10,10,50,-10)                # 根据起点、终点坐标绘制线段

arrows(10,-10,40,20,angle=15,code=1) # 绘制带箭头线段，可以设置箭头角度，有几个箭头（1起点箭头、2终点箭头，3双箭头）

abline(-20,.5) # 在图上绘制直线，第一个参数为截距，第二个参数为斜率

abline(h=0,col="red") # 在图上绘制水平线或垂直线，可以标示临界位置，很好用的功能

legend(0,max(y),"随机点连线")




# 坐标定位模式获得鼠标点击位置的坐标
x <- 1:50
y <- rnorm(50,10,5)
plot(x,y,type="n",xlab="数据索引",ylab="随机数点",xlim=c(1,50),ylim=c(-20,20),main="实验图")
print(locator(3,type="p")) # 图像进入定位模式，第一个参数决定获取几个点的坐标信息，第二个参数决定绘图方式

rect(5,5,20,20) # 在已经成功绘制的图形内部绘制一个长方形

polygon(c(20,10,30,40,45),c(-10,0,15,4,-10)) # 在高级图形内部绘制多边形




# 生成一个绘图窗口在其中绘制图形后用savePlot()函数保存
windows()
plot(1:10)
rect(1, 5, 3, 7, col="blue")
savePlot("test01", type="jpg",device=dev.cur(),restoreConsole=TRUE)



# 直接在jpeg设备上绘制图形，完成后使用dev.off()关闭设备，存盘退出
jpeg(file="myplot.jpeg")
plot(1:10)
rect(1, 5, 3, 7, col="blue")
dev.off()

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   07、金融数据获取 ###############################################################################################

# 课程内容：从雅虎金融等数据网站获取：各国股票、股指、债券、ETF基金、汇率、金属、期权等交易品种历史数据和上市公司年报数据

{

library(quantmod)


# 不设置来源则默认从雅虎金融下载；
# 雅虎金融上大量指数品种都以"^"开头
# from,to参数设置读取历史数据的时间段
getSymbols("^GSPC",src="yahoo",from="1994-1-1",to=Sys.Date()) 
print(head(GSPC));print(tail(GSPC))
print(class(GSPC))
print(is.OHLC(GSPC))
print(is.OHLCV(GSPC))


# 从雅虎金融读取著名的苹果公司的全部股票数据
getSymbols("AAPL",src="yahoo",from="1900-1-1",to=Sys.Date()) 
print(head(AAPL));print(tail(AAPL))


# 从雅虎金融读取著名港股长江实业的股票数据
# 港股和大陆A股类似使用数字编号，雅虎金融上面有全球几十个市场的数据需要进行区分
# 美股不使用后缀而其他国家或地区的股票需要使用后缀:大陆沪市使用:".SS"，深市使用:".SZ"，香港使用:".HK"
setSymbolLookup(CJSY=list(name="0001.HK",src="yahoo"))   # 在函数内部用列表指明股票代码和查询网站并指定一个变量名便于保存
getSymbols("CJSY",from="1900-1-1",to=Sys.Date()) 
print(head(CJSY));print(tail(CJSY))



# 用字符串向量保存股票代码一次下载一组股票数据
# 下载股票数量超过5种时系统会自动暂停1秒
szSymbols <- c("MSFT","ORCL","GOOG","INTL","AAPL","CSCO","SYMC","TSLA")
getSymbols(szSymbols,src="yahoo",from="2008-1-1",to=Sys.Date()) 


# 美国10年期债券收益率
getSymbols("^TNX",src="yahoo",from="1900-1-1",to=Sys.Date()) 
print(head(TNX));print(tail(TNX))



# ETF基金，伊斯兰地区ETF基金，2008-03-28
getSymbols("ACWI",src="yahoo",from="1900-1-1",to=Sys.Date()) 
print(head(ACWI));print(tail(ACWI))



# 获取美元兑日元汇率数据
# 只能获取最近1年多的汇率历史数据，并且只有收盘价
getFX("USD/JPY")
print(head(USDJPY));print(tail(USDJPY))



# 获取欧元兑美元汇率数据
getSymbols("EUR/USD",src="oanda")
print(head(EURUSD));print(tail(EURUSD))



# 运行这一段代码会出现错误，该数据源每次请求只能获取500天以内的数据
# 外汇是极其重要的交易品种，每天成交量超过5.3万亿美金，大型对冲基金都要操作
# 获取超过30年，每天OHLC历史数据的方法在第9课中详细介绍
getSymbols("EUR/USD",src="oanda",from="2005-01-01")


# 获取交易品当前最新的详细报价数据信息
tmp <- getQuote("AAPL");print(tmp);print(class(tmp))


# 获取财报信息
getFinancials("TSLA")
viewFin(TSLA.f)

viewFin(TSLA.f,"CF","A")    #  每年的现金流


# 获得股票的股息历史数据
getDividends("AAPL")


# 获得股票的拆分信息
getSplits("BIDU")



# 对股票进行除权除息调整
# 除权除息对于早期历史数据影响更明显
getSymbols("BIDU", from="2005-01-01", src="yahoo")
head(BIDU)
head(BIDU.a  <- adjustOHLC(BIDU))   # 默认调整方式不使用Adjusted列的数据
head(BIDU.uA <- adjustOHLC(BIDU, use.Adjusted=T)) 


# 计算除权除息之后的开盘价收盘价收益率和收盘价收益率，保持不变
head(cbind(OpCl(BIDU),OpCl(BIDU.a),OpCl(BIDU.uA)))
head(cbind(ClCl(BIDU),ClCl(BIDU.a),ClCl(BIDU.uA)))



# 获取期权信息
# 期权是极为重要的交易品种

BIDU.OPT <- getOptionChain("BIDU")
print(class(BIDU.OPT))  # 获取的期权链数据保存在列表中
print(BIDU.OPT)         # 显示全部期权链数据量非常大
print(BIDU.OPT$symbol)  # 显示期权链列表中的symbol数据
print(BIDU.OPT$calls)   # 显示期权链列表中的看涨期权数据


}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   08、金融数据绘图与技术指标 #####################################################################################

# 课程内容：金融时间序列的绘图、quantmod软件包中的专用函数绘图、把常用技术指标、成交量等内容加入图形

{
  
  # 
  getSymbols("^GSPC",src="yahoo",from="2004-1-1",to="2014-1-1")
  
  tail(GSPC)
  
  GSPCClose <- Cl(GSPC)
  
  tail(GSPCClose)  
  
  # plot()是泛型函数能够根据输入自变量的类型不同调用不同模块绘制图形
  # 只能对收盘价这样的单个数据点绘图
  windows()
  plot(GSPCClose)
  
  
  # chartSeries() 绘图
  # 函数可以直接接受OHLCV时间序列作为输入
  windows()
  chartSeries(GSPC)
  
  
  # 只用收盘价数据也没问题
  windows()
  chartSeries(GSPCClose)  
  
  
  # chartSeries()详细参数使用
  windows()
  chartSeries(GSPC,
              name="标普500走势图",
              type="candlesticks",
              subset="2012/2013",        # ISO8601风格的字符串用于表示时间范围
              TA=NULL,                   # 默认使用"addVo()"将成交量显示在图形底部，设置为NULL增加显示范围
              theme=chartTheme("white")) # 使用名为"white"的绘图主题
  
  # 获取white绘图主题的参数
  theme.white <- chartTheme("white")
  
  # 查看绘图主题所有参数
  names(theme.white)
  
  theme.white$up.col <- "red"
  theme.white$dn.col <- "white"
  theme.white$border <- "lightgray"
  
  
  windows()
  chartSeries(GSPC,
              name="标普500走势图",
              type="candlesticks",
              subset="2013-6/",        # 2013年6月到最后一个数据
              TA=NULL,     
              theme=theme.white)       # 使用参数经过修改的绘图主题
  
  
  # 使用文字描述的取子集功能
  windows()
  chartSeries(GSPC,
              name="标普500走势图",
              show.grid = T,           # 无论是否使用此参数都看到
              type="candlesticks",
              subset="last 3 months",  # 使用文字描述，最后3个月时间序列值
              TA="addVo()",            # 加入成交量数据
              theme=theme.white)       
  
  
  
  # reChart()的大多数参数和chartSeries()相似用于对最新绘制的图形做修改
  reChart(theme=chartTheme("black"),
          subset="last 6 months")  
  
  
  # 加入多个技术指标
  windows()
  chartSeries(GSPC,
              name="标普500走势图",
              show.grid = T,             # 无论是否使用此参数都看到
              type="candlesticks",
              subset="last 2 quarters",  # 使用文字描述，最后2个季度的时间序列值
              TA="addVo();addSMA(20);addBBands(20,3)", # 加入简单移动平均线和布林线指标，不是指标函数
              theme=theme.white)         # 使用参数经过修改的绘图主题
  
  
  # chartSeries()函数绘制出的图形通过zooom()函数做缩放操作
  # n   每次调用函数时交互图形变化的倍数
  # eps 点击鼠标几次图形发生改变
  zooom(n=1,eps=2)
  
  
  # 放大2012年的历史数据
  zoomChart("2012")
  
  # 放大2012年9月的历史数据，第三轮QE启动
  zoomChart("2012-9")
  
  # 直接使用addCCI()函数在当前图形上添加新技术指标
  addCCI(20)
  
  
  windows()
  chartSeries(GSPC,
              name="标普500走势图",
              show.grid = T,             # 无论是否使用此参数都看到
              type="candlesticks",
              subset="last 2 quarters",  # 使用文字描述，最后2个季度的时间序列值
              TA="addVo();addSMA(20);addBBands(20,3)", # 加入简单移动平均线和布林线指标，不是指标函数
              theme=theme.white)         # 使用参数经过修改的绘图主题  
  
  
  # 默认使用的是"pdf"参数，图形被保存在当前目录下
  # 运行时经常出错
  saveChart(.type="jpeg",dev=dev.cur())
  
  
  
  # 使用jpeg()和dev.off()函数配合保存绘制的图形到JPEG文件中
  jpeg("GSPC.jpeg")
  chartSeries(GSPC,
              name="标普500走势图",
              show.grid = T,             # 无论是否使用此参数都看到
              type="candlesticks",
              subset="last 2 quarters",  # 使用文字描述，最后2个季度的时间序列值
              TA="addVo();addSMA(20);addBBands(20,3)", # 加入简单移动平均线和布林线指标，不是指标函数
              theme=theme.white)         # 使用参数经过修改的绘图主题  
  dev.off()

}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   09、金融数据文件读写 ##########################################################################################

# 课程内容：将获取到的金融数据写入多种不同的文件；读取大智慧导出数据文件；读取MT4导出数据文件

{
   
   getSymbols("^GSPC",src="yahoo",from="2004-1-1",to="2014-1-1")
   tail(GSPC)
   
   # 将保存历史记录的数据框直接写入本地RDATA文件
   save(GSPC,file="GSPC.RDATA")
   
   # 删除变量GSPC之后，内存中无内容无法访问
   rm(GSPC)
   tail(GSPC)
   
   # load()函数可以装载save()函数保存的历史数据
   load("GSPC.RDATA")
   tail(GSPC)   
   

   
   # 读取CSV文件
   # header=T,表示文件存在表示数据含义的数据头部
   # skip=0,表示数据中没有需要跳过的行
   TRData <- read.csv("TradeRecord.csv",header=T,skip=0)
   head(TRData)
   
   
   # 将csv数据写入文件
   # 如果这样直接写入会保留行号不利于后续分析，并增加文件大小
   write.csv(TRData,file="WriteTR.csv")   
   
   # 设置参数不写入
   write.csv(TRData,file="WriteTR.csv",row.names=F)
   
   
   
   # 读取内存数据文件
   # 打开Excel文件，拷贝相关交易数据
   # read.table()具备从内存剪贴板中读取数据
   TRData <- read.table("clipboard", header = T, sep = "\t")
   head(TRData)
   
   
   
   # 从普通文本中读取数据
   # 打开大智慧软件查找EURUSD品种，找到欧元兑美元汇率
   # 将数据拷贝到剪贴板中并保存到文本文件中
   # 使用read.table()读取文件
   EURUSD <- read.table("DZHEURUSD.txt", header = T, skip=1,sep = "\t")
   head(EURUSD);tail(EURUSD)
   
   
   # 1989-7-17以来的英镑兑美元历史数据
   GBPUSD <- read.table("DZHGBPUSD.txt", header = T, skip=1,sep = "\t")
   head(GBPUSD);tail(GBPUSD)   
   
   

   # 从网站上读取所有表格文件
   library(XML)
   url <- "http://en.wikipedia.org/wiki/world_population"
   tbls <- readHTMLTable(url)
   
   # 分析tbls的类型为列表
   print(class(tbls))
   str(tbls)
   print(length(tbls))
   
   # 输出表格列表中的第一个元素
   tbls[[1]]
   
   
   # 从MT4平台导出数据的方法
   # MT4也是外盘重要的交易平台，当我们没有特定品种数据时可以从MT4中导出文本文件
   # 注意：MT4表示时间时使用"."做分割如：2014.4.18，这种格式R语言无法直接读取
   # 要么编写一个MT4脚本读取到历史数据之后提取年月日信息然后转换成标准的2014/4/18字符串形式
   # 要么明确日期字符串格式类型，然后转换
   as.Date("2014.4.18",format="%Y.%m.%d")
   
   
   # MT4中导出HKG33数据写为文本文件然后读取
   HKG33 <- read.table("HKG33.TXT", header = T,sep = "\t")
   head(HKG33);tail(HKG33)      
  
}





# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 主讲人：加百力

###   10、加百力的R语言、金融大数据处理学习建议 #######################################################################

# 课程内容：介绍加百力总结的深入学习R语言，提升开发功力、金融大数据处理能力的方法 

# R语言的特点是：功能极其强大；各种辅助软件包千差万别（多达数千种）；
# 函数功能丰富（每个包中常有几十甚至上百个高价值函数）

# 基于R语言的特点有以下几条建议

# 1、规避陷阱：慎入群、慎入论坛、慎提问、慎写代码

# 2、学习策略：循序渐进、多读经典图书；多读系统帮助；多写测试代码；
#    站在巨人的肩膀上（不是自己慢慢写代码实现已存在功能）

# 3、初级阶段：制订长远学习计划；坚持读书、成体系、成系统的学习R语言

# 4、中高级阶段：以软件包为核心。加百力曰：不学扩展包，脑袋就长包

# 5、高级方法：扫描式学习软件包；建立自己的示例代码库；翻译扩展软件包；制作电子档（学习笔记）发布



# 提升金融大数据处理能力的几条建议

# 1、多积累交易品基础知识：外汇、股指、能源、金属、农产品、信用产品、ETF基金
#    了解的越多，交易盈利的机会越多，风险越小

# 2、多积累极端市场行情的案例：1987年股灾、1994年债券市场崩溃、1997年亚洲金融危机
#    1998年俄罗斯债务危机、2008年次级债危机

# 3、多积累前辈成功、失败的案例。前辈遇到的情况我们也会遇到，模型是否能够扛住

# 4、多思考成功案例背后的通用模式、模型：很多案例有类似的模式如公司危机模式、季节模式、
#    计划事件模式都可以总结成数学模型

# 5、多积累跨学科模型：气候模型、天体物理模型、神经网络、小波分析等都有可能应用于投资实战，
#    积累的模型越多越能发现新规律

# 6、多积累代码经验、多思考、多测试。不满足于拍脑门、想当然的下结论，而是用实际历史数据说话