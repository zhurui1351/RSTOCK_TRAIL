# 加载扩展包
library(sp)
library(maptools)


# 设置当前工作目录
setwd("D:/")

# 读取省会城市经纬度数据
data <- read.csv("citylonlat.csv",header=TRUE,sep=",")

# 读取各省的边界数据等
border <- readShapePoly("package/china-province-border-data/bou2_4p.shp")

# 设置输出的图像文件
# jpeg("chinamap.jpeg")

# 画地图
plot(border,col=rainbow(925),ylim = c(18, 54), panel.first = grid());

# 增加省会城市坐标点
points(data$Jd, data$Wd, pch = 19, col = rgb(0, 0, 0, 0.5))

# 增加标注
text(data$Jd, data$Wd, data[,1], cex = 0.6, col = rgb(0,0,0,0.7),
     pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
             4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))


# 增加标签的另一种方式
# pointLabel(data$Jd, data$Wd, data[,1], offset = 0, cex = .6)

# 设置坐标轴
axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)

# 输出结果到图像文件
# dev.off()