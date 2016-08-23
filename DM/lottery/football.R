path = 'd:/lottery.csv'
data = read.csv(path)
data$时间 = paste(data$时间,':00',sep='')
data$时间 = as.POSIXct(data$时间)

x1 = seq(1,nrow(data)-1,by = 2)
x2 = seq(0,nrow(data),by = 2)

data_pre = data[x1,]
data_after = data[x2,]
data = merge(data_pre,data_after,by = c('时间','序号','比分'))

colnames(data) = c('时间','序号','比分','前胜','前平','前负','前类型','后胜','后平','后负','后类型')

length(unique(as.Date(data$时间)))
