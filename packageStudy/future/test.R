#http://www.aroma-project.org/share/presentations/BengtssonH_20160628-useR2016/BengtssonH_20160628-A_Future_for_R,useR2016,flat.html#15
library("future")
plan(multiprocess)       ## Parallel processing
a %<-% sum(1:50)    ## These two assignments are
b %<-% sum(51:100)  ## non-blocking and in parallel
y <- a + b               ## Waits for a and b to be resolved
y
