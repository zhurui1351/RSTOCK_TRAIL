require(quantmod)
require(blotter)
require(compiler)
path = "D:/minutedata"
files <- dir(path)
files = 'USDJPY.txt'
RBreaker_cmp <- cmpfun(RBreaker)
dualthrust_cmp <-cmpfun(dualthrust)
f = files[1]
results =list()
i = 1
for(f in files)
{
  fname = file.path(path,f)
  priceData <- read.table(fname,sep=',',header=T,colClasses = rep(c("NULL", "character", "numeric"), c(1, 2, 5)))  
  priceData <- read.zoo(priceData, sep=",", header=TRUE, 
                        index.column=1:2, format="%Y%m%d %H%M%S", tz="GMT")
  priceData <- as.xts(priceData)  
  colnames(priceData) <- c("Open","High","Low","Close","Volume")
  RBreaker_cmp(priceData,minute = 60,verbose = F)
  dualthrust_cmp(priceData,minute= 5,verbose = F,ks=0.35,kx=0.3)
  result = logger$record
  results[[i]] = list(file=f,record=logger$record)
  i = i+1
  
}

RBreaker_cmp(priceData,minute = 1,verbose = F)
r_stat = tradeStats('m_data','m_data')
dualthrust_cmp(priceData,verbose=F,minute = 60)
d_stat = tradeStats('m_data','m_data')
