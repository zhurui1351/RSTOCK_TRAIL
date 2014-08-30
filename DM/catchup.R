# 为什么选择R语言做数据处理？

# R语言是免费、开源、自由的计算平台使用成本低

# R的基础语法简单、学习速度快、上手容易

# R语言的数据可视化能力强，提供了丰富的绘图函数

# 扩展软件包发展速度快、更新快，目前已有数千个扩展包覆盖几乎所有的科学计
# 虽然运行速度比C/C++等慢，但是可以快速测试各种算法，节约项目实验、研究时间。
# 很多时候只需一行代码就可以实行一项复杂的功能

# R语言的学习、研究社区发展快、学习资料丰富

#R基本操作
# 通过赋值生成一个新变量
x  <- 1.5;print(x)
y1 <- c(1.5,2.3,8.6,7.4,9.2);print(y1)
y2 <- c("MSFT","GOOG","AAPL");print(y2)
y3 <- c(T,F,T,T,F,F);print(y3)
3.1415926 -> z;print(z)               # 数据在左，变量名在右赋值，但比较少用

assign("t",1.414);print(t)            # assign()函数给变量赋值

szSymbolName <- 'GSPC'                # assign()函数的有趣用途：将数值赋值给保存在字符串变量中的变量名
assign(szSymbolName,1860);print(GSPC)

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


# 多重分支结构
a <- 4
if( a == 1)
{
  print("a == 1")
} else if( a == 2)
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
for(i in 1:100)
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

#自定义函数
add <- function(x,y) { z=x+y;z}
add(1,2)


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

?print  
help(print)

# 常用数据结构
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
# 生成一组重复数据。使用常数初始化指定长度的向量
x <- rep(1,10);print(x)

x <- rep("USD",5);print(x)

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

#矩阵
mymatrix = matrix(c(10, 15, 3, 29), nrow = 2, byrow = TRUE)
print(mymatrix)

# 可以生成三维数组
# 使用正态分布随机数初始化整个数组
x <- array(rnorm(40,10,5),dim=c(5,4,2))
print(x)

# 数据框
# 数据框的形式和Excel比较相似
# 每一列可以看做是一个向量，类型相同；不同列数据之间可以有不同类型
# 同列表的重要区别是：数据框要求每一列数据长度相同
# 数据框的每一行可以看作是一条记录，每一列看做是记录的某一个属性

df  <- data.frame(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency=rep("USD",3),country=rep("USA",3),type=rep("STOCK",3))
df

# 列表
# 列表使用list()函数来定义，列表中的每个元素可以是单个变量或者是向量，甚至是另一个列表
# 列表中每个向量的长度可以不同，这是列表和数据框的重要区别
# 数据框是一种形式特殊的列表
lst <- list(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency="USD",country="USA",type="STOCK" )
lst
print(length(lst))  # 共5个元素

#数据的基本操作
x <- 1:10;print(x)

print(x[5])

print(x[c(1,5,8)])

print(x[1:5])

print(x[ x > 5 ])

print(mean(x))
print(x[ x > mean(x) ] ) # 逻辑表达中可以使用函数
# 追加向量元素增加向量长度
x <- 1:10;print(x)
x <- c(x,11,12,13);print(x)

# 直接在向量末尾写入数据
x <- 1:10;print(x)
x[length(x)+1] <- 11;print(x)

lst <- list(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency="USD",country="USA",type="STOCK" )
print(lst[1])
print(lst$symbols)

print(lst[[1]])        # 该列表的第一个元素是一个向量symbols

lst <- list(symbols=c("MSFT","KO","CSCO"),price=c(40.40,40.56,23.02),currency="USD",country="USA",type="STOCK" )
print(lst[1])
print(lst$symbols)

print(lst[[1]])        # 该列表的第一个元素是一个向量symbols

#基本分析函数
# 计算返回向量中最大、最小的元素的索引值
x <- 1:100
print(which.max(x))
print(which.min(x))

# 返回最大、最小值
print(max(x))
print(min(x))

# 返回均值 方差
print(mean(x))
print(sd(x))

# 对向量进行排序，默认采用升序排序方式
x <- c(3,5,2,8,6,9,7,4)
print(sort(x))
print(sort(x,decreasing=T))   # 默认的降序参数设置为F，如果设置为T则采用降序排序
# rev()函数对向量做逆序处理
x <- c(3,5,2,8,6,9,7,4)
print(x);print(rev(x))
# 获得数据集合的概述
summary(x)
# 计算组合数/排列数
print(choose(5,2))
print(factorial(3))

x <- c(1,2,NA,4,5,NA,7,6,8,9)
y <- na.omit(x)

# 去除向量中的重复数据
x <- c(1,2,NA,2,5,NA,4,3,4,9)
print(unique(x))

#R基础绘图技术
x <- 1:100
y <- 100 + x*5
plot(y)
plot(y,type="l")
# 生成100个正态分布随机数并绘图
x <- rnorm(100,10,5)
plot(x)


# 绘制1000个正态分布随机数的频数直方图
x <- rnorm(1000,30,10)
hist(x)

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

#基本文件读取
# 读取CSV文件
# header=T,表示文件存在表示数据含义的数据头部
# skip=0,表示数据中没有需要跳过的行
TRData <- read.csv("/Users/ruizhu/Desktop/R/TradeRecord.csv",header=T,skip=0)
head(TRData)

#数据挖掘
library(arules)

data(Groceries)

library(arulesViz)

rules = apriori(Groceries,parameter = list(support = 0.01,confidence = 0.2))

inspect(sort(rules,by="support")[1:6])
inspect(sort(rules,by="support")[1:6]) #按支持度查看前6条规则
inspect(sort(rules,by="confidence")[1:6]) #按置信度查看前6条规则
sub.rules=subset(rules, subset = rhs %in% "whole milk" &lift > 1.2) #也可以用subset做规则的筛选,取"右手边"含有whole milk且lift大于1.2的规则
itemFrequencyPlot(Groceries,support = 0.05,cex.names =0.8) #数据画频繁项的图

plot(rules, shading="order", control=list(main = "Two-key plot"))
plot(rules, method="grouped")
plot(rules, method="graph")
