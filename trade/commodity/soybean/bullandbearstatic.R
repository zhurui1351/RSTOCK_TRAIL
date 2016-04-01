rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/bullandbear/help',encoding = 'utf8')
dou1 = read_dou1_d_wind()
bulllist  = find_bull(ispoint=T,uppoint = 400,downpoint = -100,shindex = na.omit(dou1[,1:4]))
list_l  = bulllist
myindex = na.omit(dou1[,1:4])
for(i in 1:(length(list_l )-1))
{
  bullfrom = as.character(list_l [[i]]['from'])
  bullto = as.character(list_l [[i]]['to'])
  bullend = as.character(list_l [[i]]['end'])
  statdate = paste(bullfrom,bullto,sep='/')
  
  str = paste('从',bullfrom ,'的最低',myindex[bullfrom]$Low,'到',bullto,"的最高",myindex[bullto]$High
              ,'到',bullend,'的低点',myindex[bullend]$Low,'结束',sep='')
  print(str)
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)

}

bearlist = find_bear(uppoint = 100,downpoint = -400,shindex = na.omit(dou1[,1:4]),ispoint = T)
list_l  = bearlist
myindex = na.omit(dou1[,1:4])
for(i in 1:(length(list_l )-1))
{
  bullfrom = as.character(list_l [[i]]['from'])
  bullto = as.character(list_l [[i]]['to'])
  bullend = as.character(list_l [[i]]['end'])
  statdate = paste(bullfrom,bullto,sep='/')
  
  str = paste('从',bullfrom ,'的最高',myindex[bullfrom]$High,'到',bullto,"的最低",myindex[bullto]$Low
              ,'到',bullend,'的高点',myindex[bullend]$High,'结束',sep='')
  print(str)
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)
  
}
