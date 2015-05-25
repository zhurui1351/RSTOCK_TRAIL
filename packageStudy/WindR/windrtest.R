x = w.wsd("600000.SH","open,high,low,close,volume","1990-01-01")
x = x$Data
x = na.omit(x)
head(x)
