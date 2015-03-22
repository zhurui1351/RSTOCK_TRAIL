#analyze the log in the rbreaker
#rbreaker save the record in the logger env
require(quantmod)
record= logger$record

if(nrow(record)%%2 != 0) stop('error trading nums')

index = 1:nrow(record)

enter = record[which(index%%2==1),]
exit = record[which(index%%2==0),]

records = cbind(enter,exit)
colnames(records) = c('enterdate','enterprice','entertype','exitdate','exitprice','exittype')
net = records$exitprice - records$enterprice
#when short exitprice<enterprice means net profit
index=c(which(records$exittype == 'stopshort'),which(records$exittype == 'exitshort'))
net[index] = -net[index]
records$net = net

records=read.zoo(records,header=T,index.column = 1,format="%Y-%m-%d %H:%M:%S", tz="")
records = as.xts(records)

years =as.character(2001:2014)
year_net = sapply(years,function(x){ sum(as.numeric(records[x]$net))})
year_num = sapply(years,function(x){ length(as.numeric(records[x]$net))})

lossAndProfit = function(records)
{
  performance = list()
  #loss
  performance$gross_loss = sum(as.numeric(records[as.numeric(records$net) < 0,'net']))
  performance$gross_loss_nums = length(as.numeric(records[as.numeric(records$net) < 0,'net']))
  #long loss
  #long stop loss and long exit loss
  performance$enterlong_stop_loss = sum(as.numeric(records[records$entertype=='enterlong' & records$exittype=='stoplong' ,'net']))
  performance$enterlong_exit_loss = sum(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='enterlong' & records$exittype=='exitlong' ,'net']))
  performance$openlong_stop_loss = sum(as.numeric(records[records$entertype=='openlong' & records$exittype=='stoplong' ,'net']))
  performance$openlong_exit_loss = sum(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='openlong' & records$exittype=='exitlong' ,'net']))
  
  performance$enterlong_stop_loss_num = length(as.numeric(records[records$entertype=='enterlong' & records$exittype=='stoplong' ,'net']))
  performance$enterlong_exit_loss_num = length(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='enterlong' & records$exittype=='exitlong' ,'net']))
  performance$openlong_stop_loss_num = length(as.numeric(records[records$entertype=='openlong' & records$exittype=='stoplong' ,'net']))
  performance$openlong_exit_loss_num = length(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='openlong' & records$exittype=='exitlong' ,'net']))
  
  # short loss
  performance$entershort_stop_loss = sum(as.numeric(records[records$entertype=='entershort' & records$exittype=='stopshort' ,'net']))
  performance$entershort_exit_loss = sum(as.numeric(records[as.numeric(records$net) < 0 & records$entertype=='entershort' & records$exittype=='exitshort' ,'net']))
  performance$openshort_stop_loss = sum(as.numeric(records[records$entertype=='openshort' & records$exittype=='stopshort' ,'net']))
  performance$openshort_exit_loss = sum(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='openshort' & records$exittype=='exitshort' ,'net']))
  
  performance$entershort_stop_loss_num = length(as.numeric(records[records$entertype=='entershort' & records$exittype=='stopshort' ,'net']))
  performance$entershort_exit_loss_num = length(as.numeric(records[as.numeric(records$net) < 0 & records$entertype=='entershort' & records$exittype=='exitshort' ,'net']))
  performance$openshort_stop_loss_num = length(as.numeric(records[records$entertype=='openshort' & records$exittype=='stopshort' ,'net']))
  performance$openshort_exit_loss_num = length(as.numeric(records[as.numeric(records$net) < 0 &records$entertype=='openshort' & records$exittype=='exitshort' ,'net']))
  
  performance$entershort_loss = performance$entershort_exit_loss + performance$entershort_stop_loss
  performance$enterlong_loss = performance$enterlong_exit_loss + performance$enterlong_stop_loss
  #profit
  performance$gross_profit = sum(as.numeric(records[as.numeric(records$net) > 0,'net']))
  performance$gross_profit_num = length(as.numeric(records[as.numeric(records$net) > 0,'net']))
  
  #longprofit
  #enterlong profit open long profit
  
  performance$enterlong_profit = sum(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='enterlong','net']))
  performance$openlong_profit = sum(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='openlong','net']))
  
  performance$enterlong_profit_num = length(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='enterlong','net']))
  performance$openlong_profit_num = length(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='openlong','net']))
  
  #entershort profit openshort profit
  performance$entershort_profit = sum(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='entershort','net']))
  performance$openshort_profit = sum(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='openshort','net']))
  
  performance$entershort_profit_num = length(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='entershort','net']))
  performance$openshort_profit_num = length(as.numeric(records[as.numeric(records$net)>0 & records$entertype=='openshort','net']))
  return(performance)
}
p = lossAndProfit(records)
#分析gross profit 和 gross loss

report = function(p)
{
  d = c(p$gross_profit,p$gross_profit_num,p$enterlong_profit,p$enterlong_profit_num,
        p$openlong_profit,p$openlong_profit_num,p$entershort_profit,p$entershort_profit_num,
        p$openshort_profit,p$openshort_profit_num,p$gross_loss,p$gross_loss_nums,
        p$enterlong_exit_loss,p$enterlong_exit_loss_num,p$enterlong_stop_loss,p$enterlong_stop_loss_num,
        p$openlong_exit_loss,p$openlong_exit_loss_num,p$openlong_stop_loss,p$openlong_stop_loss_num,
        p$entershort_exit_loss,p$entershort_exit_loss_num,p$entershort_stop_loss,p$entershort_stop_loss_num,
        p$openshort_exit_loss,p$openshort_exit_loss_num,p$openshort_stop_loss,p$openshort_stop_loss_num)
  name = c('gross_profit','enterlong_profit','openlong_profit','entershort_profit',
           'openshort_profit','gross_loss', 'enterlong_exit_loss','enterlong_stop_loss',
           'openlong_exit_loss','openlong_stop_loss','entershort_exit_loss','entershort_stop_loss',
           'openshort_exit_loss','openshort_stop_loss')
  m = matrix(d,ncol=2,byrow=T)
  rownames(m) = name
  colnames(m) = c('points','nums')
  return(m)
}

m = report(p)

records_2001 = records['2001']
p_2001 = lossAndProfit(records_2001)
m_2001 = report(p_2001)

records_2002 = records['2002']
p_2002 = lossAndProfit(records_2002)
m_2002 = report(p_2002)

records_2003 = records['2003']
p_2003 = lossAndProfit(records_2003)
m_2003 = report(p_2003)
