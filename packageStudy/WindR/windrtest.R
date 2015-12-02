#http://www.dajiangzhang.com/document#pointa_3f219be0-23e2-4f16-8579-93e954bc4967
library(WindR)
w.start()
x = w.wsd("600000.SH","open,high,low,close,volume","1990-01-01")
x = x$Data
x = na.omit(x)
head(x)


codes='USDX'
fields='open,high,low,close'
begintime= '1990-01-01'
endtime= Sys.time()
wdata= w.wsi(codes,fields,begintime,endtime,'BarSize=3')
