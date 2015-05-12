#根据购买频率和近期消费情况来分析
#http://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/
#http://analyzecore.com/2015/02/19/customer-segmentation-lifecycle-grids-clv-and-cac-with-r/
#http://www.r-bloggers.com/cohort-analysis-and-lifecycle-grids-mixed-segmentation-with-r/
require('dplyr')
orders = data.frame(orderId=c(1,1,1,1,2,2),
                    clientId=c(254,254,254,254,151,151),
                   product=c('a','b','c','b','a','b'),
                   gender=c('female','female','female','female','female','female'),
                   orderdate=c('2012-04-03','2012-04-03','2012-04-03','2012-04-03','2012-01-31','2012-01-31'))
orders$orderdate = as.Date(orders$orderdate,format='%Y-%m-%d')
# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')

orders <- dcast(orders, orderId + clientId + gender + orderdate ~ 
                  product, value.var='product', fun.aggregate=length)

orders <- orders %>%
  group_by(clientId) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-orderdate)) %>%
  filter(orderdate==max(orderdate)) %>%
  filter(orderId==max(orderId))