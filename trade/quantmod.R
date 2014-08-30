### R code from vignette source 'quantmod.Rnw'

###################################################
### code chunk number 1: "House keeping"
###################################################
options(width=81,continue=" ")


###################################################
### code chunk number 2: quantmod.Rnw:123-137 (eval = FALSE)
###################################################
## # install quantmod, takes care of Defaults, xts, zoo, TTR
## #
## install.packages("quantmod", repos = "http://cran.fhcrc.org",lib=.Library, depend=T)
## #
## # install PerformanceAnalytics
## #
## install.packages("PerformanceAnalytics", repos = "http://cran.fhcrc.org",lib=.Library, depend=T)
## #
## # quantstrat and blotter stuff
## #
## install.packages("FinancialInstrument", repos = "http://R-Forge.R-project.org", lib=.Library)
## install.packages("blotter", repos = "http://R-Forge.R-project.org", lib=.Library)
## install.packages("quantstrat", repos = "http://R-Forge.R-project.org", lib=.Library)
## install.packages("TTR", repos = "http://R-Forge.R-project.org", lib=.Library)


###################################################
### code chunk number 3: quantmod.Rnw:176-178
###################################################
library(quantmod)
args(getSymbols)


###################################################
### code chunk number 4: quantmod.Rnw:195-201
###################################################
getSymbols("^GSPC")
ls(all=T)
.getSymbols
showSymbols()
class(GSPC)
dim(GSPC)


###################################################
### code chunk number 5: quantmod.Rnw:212-215
###################################################
tail(GSPC,4)
tail(Cl(GSPC),4)
tail(Ad(GSPC),4)


###################################################
### code chunk number 6: quantmod.Rnw:227-228
###################################################
args(chartSeries)


###################################################
### code chunk number 7: GSPC1
###################################################
chartSeries(GSPC,subset="2013",theme="white")


###################################################
### code chunk number 8: GSPC0
###################################################
whiteTheme <- chartTheme("white")
names(whiteTheme)
whiteTheme$bg.col <- "white"
whiteTheme$dn.col <- "pink"
whiteTheme$up.col <- "lightgreen"
whiteTheme$border <- "lightgray"
x <- chartSeries(GSPC,subset="last 3 months",theme=whiteTheme,TA=NULL)
class(x)


###################################################
### code chunk number 9: quantmod.Rnw:400-407
###################################################
myStr <- "2013-07-04"
class(myStr)
myDate <- as.Date(myStr)
class(myDate)
as.numeric(myDate)
format(myDate,"%m/%d/%y")
as.Date("110704",format="%y%m%d")


###################################################
### code chunk number 10: quantmod.Rnw:419-423
###################################################
d <- Sys.time()
class(d)
unclass(d)
sapply(unclass(as.POSIXlt(d)), function(x) x)


###################################################
### code chunk number 11: GSPC2
###################################################
chartSeries(GSPC["2013"],theme=whiteTheme,name="S&P 500")


###################################################
### code chunk number 12: GSPC3
###################################################
chartSeries(GSPC["2011/2013"],theme=whiteTheme,name="S&P 500")


###################################################
### code chunk number 13: GSPC4
###################################################
chartSeries(GSPC["201307"],theme=whiteTheme,name="S&P 500")


###################################################
### code chunk number 14: GSPC5
###################################################
chartSeries(GSPC["2013-06::2013-07"],theme=whiteTheme,name="S&P 500")


###################################################
### code chunk number 15: quantmod.Rnw:506-507
###################################################
args(getSymbols.yahoo)


###################################################
### code chunk number 16: quantmod.Rnw:523-528
###################################################
getSymbols("SPY",from="2000-01-01")
class(SPY)
head(SPY)
head(index(SPY))
class(index(SPY))


###################################################
### code chunk number 17: SBUX1
###################################################
getSymbols("SBUX",index.class="POSIXct",from="2000-01-01")
class(SPY)
head(SBUX)
head(index(SBUX))
class(index(SBUX))
chartSeries(SBUX,theme=whiteTheme)


###################################################
### code chunk number 18: quantmod.Rnw:557-559
###################################################
(spl <- getSplits("SBUX"))
class(spl)


###################################################
### code chunk number 19: quantmod.Rnw:567-569
###################################################
(div <- getDividends("SBUX"))
class(div)


###################################################
### code chunk number 20: quantmod.Rnw:578-579
###################################################
args(adjustOHLC)


###################################################
### code chunk number 21: quantmod.Rnw:598-601
###################################################
head(SBUX)
adj.exact <- adjustOHLC(SBUX)
head(adj.exact)


###################################################
### code chunk number 22: SBUX2
###################################################
head(adj.exact)
adj.approx <- adjustOHLC(SBUX, use.Adjusted=TRUE)
head(adj.approx)
chartSeries(adj.exact,theme=whiteTheme,name="SBUX")


###################################################
### code chunk number 23: quantmod.Rnw:632-635
###################################################
getSymbols("SBUX",index.class="POSIXct",from="2000-01-01",adjust=T)
head(SBUX)
head(adj.exact)


###################################################
### code chunk number 24: RRF
###################################################
getSymbols('DTB3',src='FRED')
first(DTB3,'1 week')
last(DTB3,'1 week')
chartSeries(DTB3,theme="white")


###################################################
### code chunk number 25: SBUX3
###################################################
b <- BBands(HLC=HLC(SBUX["2013"]), n=20, sd=2)
tail(b,10)
chartSeries(SBUX,TA='addBBands();addBBands(draw="p");addVo()',
  subset='2013',theme="white")


###################################################
### code chunk number 26: SBUX5
###################################################
macd  <- MACD( Cl(SBUX), 12, 26, 9, maType="EMA" )
tail(macd)
chartSeries(SBUX, TA = "addMACD()",subset="2013",theme=whiteTheme)


###################################################
### code chunk number 27: SBUX6
###################################################
rsi  <- RSI( Cl(SBUX), n = 14 )
tail(rsi)
chartSeries(SBUX, TA = "addRSI()",subset="2013",theme=whiteTheme)


###################################################
### code chunk number 28: SBUX4
###################################################
myTheme<-chart_theme()
myTheme$col$up.col<-'lightgreen'
myTheme$col$dn.col<-'pink'
#
chart_Series(SBUX["2013"],TA='add_BBands(lwd=2)',theme=myTheme,name="SBUX")


###################################################
### code chunk number 29: quantmod.Rnw:852-853
###################################################
args(strptime)


###################################################
### code chunk number 30: quantmod.Rnw:872-874
###################################################
library(xts)
args(xts)


###################################################
### code chunk number 31: quantmod.Rnw:891-899
###################################################
fn1 <- "GBPUSD.txt"
dat <- read.table(file=fn1,sep=",",header=T,as.is=T)
head(dat)
tm <- strptime(
  paste(dat[,"Date"], sprintf("%04d",dat[,"Time"])),
  format="%m/%d/%Y %H%M")
class(tm)
head(tm)


###################################################
### code chunk number 32: GBPUSD1
###################################################
GBP <- xts(x=dat[,c("Open","High","Low","Close")],order.by=tm)
GBP <- GBP['2007']
first(GBP,'4 hours')
barChart(GBP,TA='addSMA(n = 7, col = "red");addSMA(n = 44, col = "blue")',
  subset='2007-12-24/2007-12-26',theme="white",name="GBPUSD")


###################################################
### code chunk number 33: quantmod.Rnw:931-949 (eval = FALSE)
###################################################
## # make candle stick plot with moving averages
## #
## chart_Series(GBP,subset='2007-12-24/2007-12-26',theme=myTheme,name="GBPUSD",
##   TA='add_SMA(n=7,col="red",lwd=2);add_SMA(n=44,col="blue",lwd=2)')
## #
## # find cross-over bar
## #
## fastMA <- SMA(Cl(GBP),n=7)
## slowMA <- SMA(Cl(GBP),n=44)
## co <- fastMA > slowMA
## x <- which(co['2007-12-24/2007-12-26'])[1]
## #
## # identify cross-over bar
## #
## ss <- GBP['2007-12-24/2007-12-26']
## add_TA(ss[x,"Low"]-0.0005,pch=17,type="p",col="red", on=1,cex=2)
## #
## text(x=x,y=ss[x,"Low"]-0.0005,"Crossover\nbar",pos=1)


