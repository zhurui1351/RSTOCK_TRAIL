#根据购买频率和近期消费情况来分析
#http://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/
#http://analyzecore.com/2015/02/19/customer-segmentation-lifecycle-grids-clv-and-cac-with-r/
#http://www.r-bloggers.com/cohort-analysis-and-lifecycle-grids-mixed-segmentation-with-r/
#http://analyzecore.com/2015/02/19/customer-segmentation-lifecycle-grids-clv-and-cac-with-r/
#http://analyzecore.com/2014/12/04/sequence-carts-in-depth-analysis-with-r/
library(dplyr)
library(reshape2)
library(ggplot2)
# creating data sample
set.seed(10)
data <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 5000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))
order <- data.frame(orderId=c(1:1000),
                    clientId=sample(c(1:300), 1000, replace=TRUE))
gender <- data.frame(clientId=c(1:300),
                     gender=sample(c('male', 'female'), 300, replace=TRUE, prob=c(0.40, 0.60)))
date <- data.frame(orderId=c(1:1000),
                   orderdate=sample((1:100), 1000, replace=TRUE))
orders <- merge(data, order, by='orderId')
orders <- merge(orders, gender, by='clientId')
orders <- merge(orders, date, by='orderId')
orders <- orders[orders$product!='NULL', ]
orders$orderdate <- as.Date(orders$orderdate, origin="2012-01-01")
rm(data, date, order, gender)
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

# exploratory analysis
ggplot(orders, aes(x=frequency)) +
  theme_bw() +
  scale_x_continuous(breaks=c(1:10)) +
  geom_bar(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by frequency")

ggplot(orders, aes(x=recency)) +
  theme_bw() +
  geom_bar(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by recency")

#segmentation
orders.segm <- orders %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',
                         ifelse(between(recency, 7, 13), '7-13 days',
                                ifelse(between(recency, 14, 19), '14-19 days',
                                       ifelse(between(recency, 20, 45), '20-45 days',
                                              ifelse(between(recency, 46, 80), '46-80 days', '>80 days')))))) %>%
  # creating last cart feature
  mutate(cart=paste(ifelse(a!=0, 'a', ''),
                    ifelse(b!=0, 'b', ''),
                    ifelse(c!=0, 'c', ''), sep='')) %>%
  arrange(clientId)

orders.segm$segm.freq <- factor(orders.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))

lcg <- orders.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

lcg.matrix <- dcast(lcg, segm.freq ~ segm.rec, value.var='quantity', fun.aggregate=sum)


ggplot(lcg, aes(x=client, y=quantity, fill=quantity)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids")

#sub segment by gender or cart
lcg.sub <- orders.segm %>%
  group_by(gender, cart, segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

ggplot(lcg.sub, aes(x=client, y=quantity, fill=gender)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids by gender (propotion)")
ggplot(lcg.sub, aes(x=gender, y=quantity, fill=cart)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids by gender and last cart (propotion)")

# add customer acquisition cost (CAC) and customer lifetime value (CLV)
library(dplyr)
library(reshape2)
library(ggplot2)

# creating data sample
set.seed(10)
data <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 5000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))
order <- data.frame(orderId=c(1:1000),
                    clientId=sample(c(1:300), 1000, replace=TRUE))
gender <- data.frame(clientId=c(1:300),
                     gender=sample(c('male', 'female'), 300, replace=TRUE, prob=c(0.40, 0.60)))
date <- data.frame(orderId=c(1:1000),
                   orderdate=sample((1:100), 1000, replace=TRUE))
orders <- merge(data, order, by='orderId')
orders <- merge(orders, gender, by='clientId')
orders <- merge(orders, date, by='orderId')
orders <- orders[orders$product!='NULL', ]
orders$orderdate <- as.Date(orders$orderdate, origin="2012-01-01")

# creating data frames with CAC and Gross margin
cac <- data.frame(clientId=unique(orders$clientId), cac=sample(c(10:15), 289, replace=TRUE))
gr.margin <- data.frame(product=c('a', 'b', 'c'), grossmarg=c(1, 2, 3))

rm(data, date, order, gender)

#计算CLV TO DATE
# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')

# calculating customer lifetime value
orders <- merge(orders, gr.margin, by='product')

clv <- orders %>%
  group_by(clientId) %>%
  summarise(clv=sum(grossmarg))

# processing data
orders <- dcast(orders, orderId + clientId + gender + orderdate ~ product, value.var='product', fun.aggregate=length)

orders <- orders %>%
  group_by(clientId) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-orderdate)) %>%
  filter(orderdate==max(orderdate)) %>%
  filter(orderId==max(orderId))

orders.segm <- orders %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',
                         ifelse(between(recency, 7, 13), '7-13 days',
                                ifelse(between(recency, 14, 19), '14-19 days',
                                       ifelse(between(recency, 20, 45), '20-45 days',
                                              ifelse(between(recency, 46, 80), '46-80 days', '>80 days')))))) %>%
  # creating last cart feature
  mutate(cart=paste(ifelse(a!=0, 'a', ''),
                    ifelse(b!=0, 'b', ''),
                    ifelse(c!=0, 'c', ''), sep='')) %>%
  arrange(clientId)
orders.segm$segm.freq <- factor(orders.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))

#计算总的clv和cac
orders.segm <- merge(orders.segm, clv, by='clientId')

lcg.clv <- orders.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n(),
            # calculating cumulative CAC and CLV
            cac=sum(cac),
            clv=sum(clv)) %>%
  ungroup() %>%
  # calculating CAC and CLV per client
  mutate(cac1=round(cac/quantity, 2),
         clv1=round(clv/quantity, 2))

lcg.clv <- melt(lcg.clv, id.vars=c('segm.rec', 'segm.freq', 'quantity'))
#绘图
ggplot(lcg.clv[lcg.clv$variable %in% c('clv', 'cac'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=value), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids - CLV vs CAC (total)")
ggplot(lcg.clv[lcg.clv$variable %in% c('clv1', 'cac1'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=value), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids - CLV vs CAC (average)")

#Cohort Analysis 人口特征分析 增加营销 预测等分析
library(dplyr)
library(reshape2)
library(ggplot2)
library(googleVis)

set.seed(10)
# creating orders data sample
data <- data.frame(orderId=sample(c(1:5000), 25000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 25000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))
order <- data.frame(orderId=c(1:5000),
                    clientId=sample(c(1:1500), 5000, replace=TRUE))
date <- data.frame(orderId=c(1:5000),
                   orderdate=sample((1:500), 5000, replace=TRUE))
orders <- merge(data, order, by='orderId')
orders <- merge(orders, date, by='orderId')
orders <- orders[orders$product!='NULL', ]
orders$orderdate <- as.Date(orders$orderdate, origin="2012-01-01")
rm(data, date, order)
# creating data frames with CAC, Gross margin, Campaigns and Potential CLV
gr.margin <- data.frame(product=c('a', 'b', 'c'), grossmarg=c(1, 2, 3))
campaign <- data.frame(clientId=c(1:1500),
                       campaign=paste('campaign', sample(c(1:7), 1500, replace=TRUE), sep=' '))
cac <- data.frame(campaign=unique(campaign$campaign), cac=sample(c(10:15), 7, replace=TRUE))
campaign <- merge(campaign, cac, by='campaign')
potential <- data.frame(clientId=c(1:1500),
                        clv.p=sample(c(0:50), 1500, replace=TRUE))
rm(cac)

# reporting date
today <- as.Date('2013-05-16', format='%Y-%m-%d')

orders <- merge(orders, gr.margin, by='product')

customers <- orders %>%
  # combining products and summarising gross margin
  group_by(orderId, clientId, orderdate) %>%
  summarise(grossmarg=sum(grossmarg)) %>%
  ungroup() %>%
  # calculating frequency, recency, average time lapses between purchases and defining cohorts
  group_by(clientId) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-max(orderdate)),
         av.gap=round(as.numeric(max(orderdate)-min(orderdate))/frequency, 0),
         cohort=format(min(orderdate), format='%Y-%m')) %>%
  ungroup() %>%
  # calculating CLV to date
  group_by(clientId, cohort, frequency, recency, av.gap) %>%
  summarise(clv=sum(grossmarg)) %>%
  arrange(clientId)
# calculating potential CLV and CAC
customers <- merge(customers, campaign, by='clientId')
customers <- merge(customers, potential, by='clientId')
# leading the potential value to more or less real value
customers$clv.p <- round(customers$clv.p / sqrt(customers$recency) * customers$frequency, 2)

rm(potential, gr.margin, today)

customers <- customers %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 30), '0-30 days',
                         ifelse(between(recency, 31, 60), '31-60 days',
                                ifelse(between(recency, 61, 90), '61-90 days',
                                       ifelse(between(recency, 91, 120), '91-120 days',
                                              ifelse(between(recency, 121, 180), '121-180 days', '>180 days'))))))

# defining order of boundaries
customers$segm.freq <- factor(customers$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
customers$segm.rec <- factor(customers$segm.rec, levels=c('>180 days', '121-180 days', '91-120 days', '61-90 days', '31-60 days', '0-30 days'))
#首次购买行为分析
lcg.coh <- customers %>%
  group_by(cohort, segm.rec, segm.freq) %>%
  # calculating cumulative values
  summarise(quantity=n(),
            cac=sum(cac),
            clv=sum(clv),
            clv.p=sum(clv.p),
            av.gap=sum(av.gap)) %>%
  ungroup() %>%
  # calculating average values
  mutate(av.cac=round(cac/quantity, 2),
         av.clv=round(clv/quantity, 2),
         av.clv.p=round(clv.p/quantity, 2),
         av.clv.tot=av.clv+av.clv.p,
         av.gap=round(av.gap/quantity, 2),
         diff=av.clv-av.cac)

ggplot(lcg.coh, aes(x=cohort, fill=cohort)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=diff), stat='identity', alpha=0.5) +
  geom_text(aes(y=diff, label=round(diff,0)), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Cohorts in LifeCycle Grids - difference between av.CLV to date and av.CAC")

ggplot(lcg.coh, aes(x=cohort, fill=cohort)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=av.clv.tot), stat='identity', alpha=0.2) +
  geom_text(aes(y=av.clv.tot+10, label=round(av.clv.tot,0), color=cohort), size=4) +
  geom_bar(aes(y=av.clv), stat='identity', alpha=0.7) +
  geom_errorbar(aes(y=av.cac, ymax=av.cac, ymin=av.cac), color='red', size=1.2) +
  geom_text(aes(y=av.cac, label=round(av.cac,0)), size=4, color='darkred', vjust=-.5) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Cohorts in LifeCycle Grids - total av.CLV and av.CAC")
#custom flow
coh <- '2012-09'
report.dates <- c('2012-10-01', '2013-01-01', '2013-04-01')
report.dates <- as.Date(report.dates, format='%Y-%m-%d')

# defining segments for each cohort's customer for reporting dates
df.sankey <- data.frame()

for (i in 1:length(report.dates)) {
  
  orders.cache <- orders %>%
    filter(orderdate < report.dates[i])
  
  customers.cache <- orders.cache %>%
    select(-product, -grossmarg) %>%
    unique() %>%
    group_by(clientId) %>%
    mutate(frequency=n(),
           recency=as.numeric(report.dates[i] - max(orderdate)),
           cohort=format(min(orderdate), format='%Y-%m')) %>%
    ungroup() %>%
    select(clientId, frequency, recency, cohort) %>%
    unique() %>%
    filter(cohort==coh) %>%
    mutate(segm.freq=ifelse(between(frequency, 1, 1), '1 purch',
                            ifelse(between(frequency, 2, 2), '2 purch',
                                   ifelse(between(frequency, 3, 3), '3 purch',
                                          ifelse(between(frequency, 4, 4), '4 purch',
                                                 ifelse(between(frequency, 5, 5), '5 purch', '>5 purch')))))) %>%
    mutate(segm.rec=ifelse(between(recency, 0, 30), '0-30 days',
                           ifelse(between(recency, 31, 60), '31-60 days',
                                  ifelse(between(recency, 61, 90), '61-90 days',
                                         ifelse(between(recency, 91, 120), '91-120 days',
                                                ifelse(between(recency, 121, 180), '121-180 days', '>180 days')))))) %>%
    mutate(cohort.segm=paste(cohort, segm.rec, segm.freq, sep=' : '),
           report.date=report.dates[i]) %>%
    select(clientId, cohort.segm, report.date)
  
  df.sankey <- rbind(df.sankey, customers.cache)
}

# processing data for Sankey diagram format
df.sankey <- dcast(df.sankey, clientId ~ report.date, value.var='cohort.segm', fun.aggregate = NULL)
write.csv(df.sankey, 'customers_path.csv', row.names=FALSE)
df.sankey <- df.sankey %>% select(-clientId)

df.sankey.plot <- data.frame()
for (i in 2:ncol(df.sankey)) {
  
  df.sankey.cache <- df.sankey %>%
    group_by(df.sankey[ , i-1], df.sankey[ , i]) %>%
    summarise(n=n())
  
  colnames(df.sankey.cache)[1:2] <- c('from', 'to')
  
  df.sankey.cache$from <- paste(df.sankey.cache$from, ' (', report.dates[i-1], ')', sep='')
  df.sankey.cache$to <- paste(df.sankey.cache$to, ' (', report.dates[i], ')', sep='')
  
  df.sankey.plot <- rbind(df.sankey.plot, df.sankey.cache)
}

# plotting
plot(gvisSankey(df.sankey.plot, from='from', to='to', weight='n',
                options=list(height=900, width=1800, sankey="{link:{color:{fill:'lightblue'}}}")))

ggplot(lcg.coh, aes(x=cohort, fill=cohort)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=av.gap), stat='identity', alpha=0.6) +
  geom_text(aes(y=av.gap, label=round(av.gap,0)), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Cohorts in LifeCycle Grids - average time lapses between purchases")

# campaign cohorts
lcg.camp <- customers %>%
  group_by(campaign, segm.rec, segm.freq) %>%
  # calculating cumulative values
  summarise(quantity=n(),
            cac=sum(cac),
            clv=sum(clv),
            clv.p=sum(clv.p),
            av.gap=sum(av.gap)) %>%
  ungroup() %>%
  # calculating average values
  mutate(av.cac=round(cac/quantity, 2),
         av.clv=round(clv/quantity, 2),
         av.clv.p=round(clv.p/quantity, 2),
         av.clv.tot=av.clv+av.clv.p,
         av.gap=round(av.gap/quantity, 2),
         diff=av.clv-av.cac)

ggplot(lcg.camp, aes(x=campaign, fill=campaign)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=diff), stat='identity', alpha=0.5) +
  geom_text(aes(y=diff, label=round(diff,0)), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Campaigns in LifeCycle Grids - difference between av.CLV to date and av.CAC")

lcg.camp <- customers %>%
  group_by(campaign, segm.rec, segm.freq) %>%
  # calculating cumulative values
  summarise(quantity=n(),
            cac=sum(cac),
            clv=sum(clv),
            clv.p=sum(clv.p),
            av.gap=sum(av.gap)) %>%
  ungroup() %>%
  # calculating average values
  mutate(av.cac=round(cac/quantity, 2),
         av.clv=round(clv/quantity, 2),
         av.clv.p=round(clv.p/quantity, 2),
         av.clv.tot=av.clv+av.clv.p,
         av.gap=round(av.gap/quantity, 2),
         diff=av.clv-av.cac)

ggplot(lcg.camp, aes(x=campaign, fill=campaign)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=diff), stat='identity', alpha=0.5) +
  geom_text(aes(y=diff, label=round(diff,0)), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Campaigns in LifeCycle Grids - difference between av.CLV to date and av.CAC")

ggplot(lcg.camp, aes(x=campaign, fill=campaign)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=av.clv.tot), stat='identity', alpha=0.2) +
  geom_text(aes(y=av.clv.tot+10, label=round(av.clv.tot,0), color=campaign), size=4) +
  geom_bar(aes(y=av.clv), stat='identity', alpha=0.7) +
  geom_errorbar(aes(y=av.cac, ymax=av.cac, ymin=av.cac), color='red', size=1.2) +
  geom_text(aes(y=av.cac, label=round(av.cac,0)), size=4, color='darkred', vjust=-.5) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Campaigns in LifeCycle Grids - total av.CLV and av.CAC")

ggplot(lcg.camp, aes(x=campaign, fill=campaign)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=av.gap), stat='identity', alpha=0.6) +
  geom_text(aes(y=av.gap, label=round(av.gap,0)), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Campaigns in LifeCycle Grids - average time lapses between purchases")

#clv变动路径分析
library(TraMineR)

min.date <- min(orders$orderdate)
max.date <- max(orders$orderdate)

l <- c(seq(0,as.numeric(max.date-min.date), 10), as.numeric(max.date-min.date))
df <- data.frame()
for (i in l) {
  
  cur.date <- min.date + i
  print(cur.date)
  
  orders.cache <- orders %>%
    filter(orderdate <= cur.date)
  
  customers.cache <- orders.cache %>%
    select(-product, -grossmarg) %>%
    unique() %>%
    group_by(clientId) %>%
    mutate(frequency=n(),
           recency=as.numeric(cur.date - max(orderdate))) %>%
    ungroup() %>%
    select(clientId, frequency, recency) %>%
    unique() %>%
    
    mutate(segm=
             ifelse(between(frequency, 1, 2) & between(recency, 0, 60), 'new customer',
                    ifelse(between(frequency, 1, 2) & between(recency, 61, 180), 'under risk new customer',
                           ifelse(between(frequency, 1, 2) & recency > 180, '1x buyer',
                                  
                                  ifelse(between(frequency, 3, 4) & between(recency, 0, 60), 'engaged customer',
                                         ifelse(between(frequency, 3, 4) & between(recency, 61, 180), 'under risk engaged customer',
                                                ifelse(between(frequency, 3, 4) & recency > 180, 'former engaged customer',
                                                       
                                                       ifelse(frequency > 4 & between(recency, 0, 60), 'best customer',
                                                              ifelse(frequency > 4 & between(recency, 61, 180), 'under risk best customer',
                                                                     ifelse(frequency > 4 & recency > 180, 'former best customer', NA)))))))))) %>%
    
    mutate(report.date=i) %>%
    select(clientId, segm, report.date)
  
  df <- rbind(df, customers.cache)
}

df <- df %>%
  mutate(grid=paste(segm.rec, segm.freq, sep=' : ')) %>%
  select(clientId, grid, report.date)

df <- dcast(df, clientId ~ report.date, value.var='grid', fun.aggregate = NULL)
df.seq <- seqdef(df, 2:ncol(df), left='DEL', right='DEL', xtstep=10)

# creating df with first purch.date and campaign cohort features
feat <- df %>% select(clientId)
feat <- merge(feat, campaign[, 1:2], by='clientId')
feat <- merge(feat, customers[, 1:2], by='clientId')

# plotting the 10 most frequent sequences based on campaign
seqfplot(df.seq, border=NA, group=feat$campaign)

# plotting the 10 most frequent sequences based on campaign
seqfplot(df.seq, border=NA, group=feat$campaign, cex.legend=0.9)

# plotting the 10 most frequent sequences based on first purch.date cohort
coh.list <- sort(unique(feat$cohort))
# defining cohorts for plotting
feat.coh.list <- feat[feat$cohort %in% coh.list[1:6] , ]
df.coh <- df %>% filter(clientId %in% c(feat.coh.list$clientId))
df.seq.coh <- seqdef(df.coh, 2:ncol(df.coh), left='DEL', right='DEL', xtstep=10)
seqfplot(df.seq.coh, border=NA, group=feat.coh.list$cohort, cex.legend=0.9)