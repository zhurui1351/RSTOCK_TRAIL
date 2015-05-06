# 加百力咨询： 《R语言与金融大数据处理》 - 《基础篇》 （v1.0)
# 最后修改时间：2014-04-23(17.3)

# 请大家自己下载安装对应的扩展包和它们的支持软件包


setwd("D:/RWD/R-FBD")

library(quantmod)    
library(ggplot2) 
library(reshape2)
library(plyr)
library(tseries)
library(timeSeries)
library(PerformanceAnalytics)
library(nlme)


# 后期介绍ggplot2软件包时将会使用的数据
data(diamonds)
data(economics)
data(msleep)

dsmall <- diamonds[sample(nrow(diamonds),100),]
dlarge <- diamonds[sample(nrow(diamonds),1000),]

pt <- function() { szCurTime <- as.character.Date(Sys.time()); options(prompt=paste(szCurTime,">",sep="")) }
pt()

print("初始化完成！",quote=F)