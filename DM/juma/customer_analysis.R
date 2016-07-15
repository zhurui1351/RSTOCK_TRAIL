enddate = as.Date('2016-07-08')
cusflag = cus_flag_func(cusdt,orderdt,enddate)
actcus = subset(cusflag,flag %in% c('留存用户'))
flowcus = subset(cusflag,flag %in% c('流失用户'))

actcus_info = getCusinfobytype(actcus$cusno,orderdt)
flowcus_info = getCusinfobytype(flowcus$cusno,orderdt)

result = data.frame()

feature = colnames(actcus_info)[c(2:8)]
for(fe in feature)
{
  r = data.frame(留存用户=mean(actcus_info[,fe]),流失用户=mean(flowcus_info[,fe]))
  result = rbind(result,r)
}
rownames(result) = feature
result_t = t(as.matrix(result))
barplot(result_t,beside = T,col = c("lightblue", "mistyrose"),legend=rownames(result_t))

