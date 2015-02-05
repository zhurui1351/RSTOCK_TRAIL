#函数初始化xts对象，加入三列，第一列对应期限的涨跌，以-1,0-1表示，-1表示跌，0表示不变，1表示涨
#第二列参照维度第几周等信息，第三列为比较维度，比较周几
#type参数表示初始化数据的类型
#iw 表示 in week 添加周几、第几周的信息
#bw 表示 between week 添加第几周 第几个月的信息
#bm 表示 between month 添加第几月，第几年的信息
initialData <- function(data,type='iw')
{
  if(!is.xts(data))
  {
    stop("data should be a xts object")
  }
  mydata = data
  if(type == 'iw')
  {
    sec = xts(strftime(index(mydata), "%u"),index(mydata))
    names(sec) = 'sec'
    #获取第几周的信息
    fst = xts( strftime(index(mydata), "%U"),index(mydata))
    names(fst) = 'fst'
    
  }
  if(type == 'bw')
  {
    mydata = to.weekly(mydata)
   # colnames(mydata) <- c("Open","High","Low","Close")
   tempm = '0'
   tempindex = 1
   r = c()
   xx = index(mydata)
   for(i in 1 : length(xx))
   {
     m = strftime(xx[i], "%m")
     if(m != tempm)
     {
       tempm = m
       tempindex = 1
     }
     else
     {
       tempindex = tempindex+1
     }
     
     r=c(r,tempindex)
   }
   
    sec = xts(r,index(mydata))
    names(sec) = 'sec'
    #获取第几月的信息
    fst = xts( strftime(index(mydata), "%m"),index(mydata))
    names(fst) = 'fst'
    
  }
  
  if(type == 'bm')
  {
    mydata = to.period(mydata,period = 'months')
    #colnames(mydata) <- c("Open","High","Low","Close")
    sec = xts(strftime(index(mydata), "%m"),index(mydata))
    names(sec) = 'sec'
    #获取第几月的信息
    fst = xts( strftime(index(mydata), "%Y"),index(mydata))
    names(fst) = 'fst'
    
  }
  mydata$volatility = sign(Cl(mydata)-Op(mydata))
  #使用merge.xts报错，未知原因
  #mydata <- data.frame(mydata[,c(1,2,3,4,7)],wdays[,1],weekth[,1])
  mydata = data.frame(Op(mydata),Hi(mydata),Lo(mydata),Cl(mydata),mydata[,'volatility'],sec[,1],fst[,1])
  mydata = as.xts(mydata)
  
  #获取礼拜几信息
  #初始化数据处理结束
  return(mydata)
}



