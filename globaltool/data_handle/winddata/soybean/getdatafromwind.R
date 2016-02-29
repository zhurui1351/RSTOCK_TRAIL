require(WindR)
w.start()
w_wsd_data<-w.wsd("A.DCE","open,high,low,close","1999-01-01","2016-02-29","Currency=CNY;PriceAdj=F")
w_wsd_code<-w.wsd("A.DCE","trade_hiscode","2014-06-15","2015-12-15")
