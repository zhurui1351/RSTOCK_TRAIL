#https://github.com/jincheng9/stock_crawler/blob/master/R/get.stock.index.info.R
#thanks
# get the real time China's A-share stock and index data from sina finance
# author: jincheng9

# input: stock.code: sh600000 (浦发银行) sz000001 (平安银行)
# output: a data.frame containing the stock information
require('RCurl')
get.stock.info <- function (stock.code) {
  
  # get the stock information using http/js interface
  url <- paste0('http://hq.sinajs.cn/list=', stock.code)
  stock.info <- strsplit(getURL(url, .encoding='GBK'), ',')[[1]]
  
  # get the stock symbol
  stock.symbol <- stock.info[1]
  loc <- gregexpr(pattern='\"', stock.symbol)[[1]][1]
  stock.info[1] <- substr(stock.symbol, loc+1, nchar(stock.symbol))
  stock.info <- stock.info[1:(length(stock.info)-1)]
  
  # convert a character string to a data.frame
  stock.data <- data.frame(V1=0)
  for (i in 1:length(stock.info)) {
    stock.data[[i]] <- stock.info[i]
  }
  
  # convert string to float, except for the symbol, date, and time
  stock.data[2:(length(stock.info)-2)] <- as.numeric(stock.data[2:(length(stock.info)-2)])
  colnames(stock.data) = c('name','open','preclose','curprice','high','low','buyprice','sellprice',
                           'volumn','total','b1amount','b1price','b2amount','b2price',
                           'b3amount','b3price','b4amount','b4price','b5amount','b5price',
                           's1amount','s1price','s2amount','s2price','s3amount','s3price',
                           's4amount','s4price','s5amount','s5price','date','time')
  # return the stock data 
  stock.data
}

# input: index.code: sh000001 (上证指数) sz399001 (深圳成指)
# output: a data.frame containing the index information
get.index.info <- function(index.code) {
  # get the index information using the http/js interface
  url <- paste0('http://hq.sinajs.cn/list=s_', index.code)
  index.info <- strsplit(getURL(url, .encoding='GBK'), ',')[[1]]
  
  # get the index symbol
  index.symbol <- index.info[1]
  loc <- gregexpr(pattern='\"', index.symbol)[[1]][1]
  index.info[1] <- substr(index.symbol, loc+1, nchar(index.symbol))
  
  # get the index volume of transaction
  index.volume <- index.info[length(index.info)]
  loc <- gregexpr(pattern='\"', index.volume)[[1]][1]
  index.info[length(index.info)] <- substr(index.volume, 1, loc-1)
  
  # convert a character string to a data.frame
  index.data <- data.frame(V1=0)
  for (i in 1:length(index.info)) {
    index.data[[i]] <- index.info[i]
  }
  
  # convert string to float, except for the symbol
  index.data[2:length(index.info)] <- as.numeric(index.data[2:length(index.info)])
  
  # return the index data
  
  index.data
}

test_cabage = function()
{
  
  stock.data <- get.stock.info('sh600000')
  cat('stock symbol is: ', stock.data$V1, '\n')
  cat('open price is: ', stock.data$V2, '\n')
  cat('previous close price is: ', stock.data$V3, '\n')              
  cat('current price is: ', stock.data$V4, '\n')     
  
  index.data <- get.index.info('sz399905')
  cat('index symbol is: ', index.data$V1, '\n')          
  cat('current price is: ', index.data$V3, '\n')  
}
