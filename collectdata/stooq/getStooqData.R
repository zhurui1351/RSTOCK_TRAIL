# http://www.rhinocerus.net/forum/lang-php/662887-how-read-download-attachment-uri.html - inspiration :)
#http://snipplr.com/view/57749/downloading-market-data-from-stooq-to-r/

library(RCurl)


getStooqData <- function(asset_code,static_cookie=TRUE) {
  
  data_tmp <- tempfile() # "data.csv"
  cookie_tmp <- "cookie.txt"
  
  u1 <- paste("http://stooq.com/q/d/?s=",asset_code,sep="")
  u2 <- paste("http://stooq.com/q/d/l/?s=",asset_code,"&i=d",sep="")
  
  if (!static_cookie) { 
    
    
    
    h <- c(paste("GET ",u1," HTTP/1.0",sep=""),
           Accept="image/gif",Accept="image/x-xbitmap",Accept="image/jpeg",Accept="mage/pjpeg",Accept="application/x-shockwave-flash",Accept="application/vnd.ms-excel",Accept="application/msword",Accept="*/*",
           'Accept-Language'="pl, en-us;q=0.7",'User-Agent'="Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)",'Proxy-Connection'="Keep-Alive")
    
    
    u1Opts <- curlOptions(header=TRUE,httpheader=h,cookiejar=cookie_tmp)
    curlPerform(url=u1,.opts=u1Opts,verbose=TRUE)
    
    h <- c(paste("GET",u2,"HTTP/1.0"),
           Accept="image/gif",Accept="image/x-xbitmap",Accept="image/jpeg",Accept="mage/pjpeg",Accept="application/x-shockwave-flash",Accept="application/vnd.ms-excel",Accept="application/msword",Accept="*/*",
           'Accept-Language'="pl, en-us;q=0.7",'User-Agent'="Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)",'Proxy-Connection'="Keep-Alive")   
    
    u2Opts <- curlOptions(header=TRUE,httpheader=h,cookiefile=cookie_tmp)
    
  }
  
  else {
    
    
    h <- c(paste("GET",u2,"HTTP/1.0"),
           Accept="image/gif",Accept="image/x-xbitmap",Accept="image/jpeg",Accept="mage/pjpeg",Accept="application/x-shockwave-flash",Accept="application/vnd.ms-excel",Accept="application/msword",Accept="*/*",
           'Accept-Language'="pl, en-us;q=0.7",'User-Agent'="Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)",'Proxy-Connection'="Keep-Alive",Cookie=paste("cookie_uu=p;cookie_user=%3F0001dllg000011500d1300%7C",asset_code,sep=""))   
    
    u2Opts <- curlOptions(header=TRUE,httpheader=h)
    
  } 
  
  
  reader <- basicTextGatherer() 
  
  w <- getURLContent(url=u2,.opts=u2Opts)
  
  
  write(w,file=data_tmp)
  
  stooq_data <- read.csv(data_tmp)
  
  stooq_data
}


stooq_data <- getStooqData("usd_i",static_cookie=TRUE)
