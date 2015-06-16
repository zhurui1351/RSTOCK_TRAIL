#寻找两个波谷之间的波峰，首先判断是否满足增长率的要求
#1.满足，则波谷起点，波峰就是最高点，寻找波峰到后续满足回撤的点
#2.不满足，如果波峰后面一个波谷小于第一个波谷，则后面的波谷作为起点，重新寻找
#如果后面一个波谷大于第一个波谷，则忽略该波谷寻找下一个波峰和布谷，继续计算，直到满足条件
#寻找下降和回撤满足的点用相似的算法，只是将波峰和波谷的角色交换


require(quantmod)
dataseries=c(100,98,95,96,93,95,94,98,102,106,110,107,105,107,111,115,113,114,115,118,115,117,
             120,117,116,110,112,108,105,100)
plot(dataseries)

upraotio = 200
downratio = 100

findTheFirstTimeSpan <- function(dataseries,upraotio=6,downratio=2)
{
  peakpos = findPeaks(dataseries,thresh=0.000001) - 1
  valleypos = findValleys(dataseries,thresh=0.000001) - 1
  if(length(valleypos) < 2 )
    return(list(start=1,peak=0,end=0))
  if(peakpos[1] < valleypos[1])
  {
    peakpos = peakpos[2:length(peakpos)]
  }
  
  start = 1
  startpos = 0
  highpos = 0
  endpos = 0
  i = 1
  nextvalleypos = i + 1;
  preppeakpos = nextvalleypos - 1
  while(i < length(valleypos))
  {
    #满足增长
    if((dataseries[peakpos[preppeakpos]] - dataseries[valleypos[start]]) >= upraotio)
    {
      startpos = valleypos[start]
      highpos = peakpos[preppeakpos]
      #寻找回撤结束点
      for(j in (highpos+1):length(dataseries))
      {
        if(dataseries[highpos] - dataseries[j] > downratio)
        {
          break
        }
      }
      
      endpos = j
      return(list(start=startpos,peak=highpos,end=endpos))
      # break;
    }
    else{ #计算下一个峰谷
      if(dataseries[valleypos[nextvalleypos]] < dataseries[valleypos[start]])
      {
        start = nextvalleypos
      }
      i = i + 1
      nextvalleypos = i + 1;
      preppeakpos = nextvalleypos - 1
    }
    
  }
  return(list(start=startpos,peak=highpos,end=endpos))
}

findallSapn <- function(dataseries,upraotio=6,downratio=2)
{
  i = 1;
  result = list()
  result_index = 1
  while(i<length(dataseries))
  {
    r = findTheFirstTimeSpan(dataseries[i:length(dataseries)],upraotio,downratio)
    if(r$end == 0 )
      break
    r=lapply(r,function(x){x+i-1})
    result[[result_index]] = r
    result_index = result_index + 1
    
    
    i = r$end
  }
  
  return(result)
}

addlines <- function(dataseries,spanList)
{
  plot(dataseries)
  lines(c(spanList[[1]]$start,spanList[[1]]$peak),c(dataseries[c(spanList[[1]]$start,spanList[[1]]$peak)]))
  lines(c(spanList[[1]]$peak,spanList[[1]]$end),c(dataseries[c(spanList[[1]]$peak,spanList[[1]]$end)]))
  
  lapply(spanList,function(x){lines(c(x$start,x$peak),c(dataseries[c(x$start,x$peak)]))
                              lines(c(x$peak,x$end),c(dataseries[c(x$peak,x$end)]))
  })
}

##选择满足回撤的最长增长
#起点是第一对波谷和波峰作为起点和临时高点，判断波峰后的波谷与临时高点是否满足回撤条件
#1.如果满足，那么起点就是波谷，高点就是波峰，终点就是后一个波谷
#2.不满足，如果后一个波谷小于起点，那么该波谷作为起点，重新寻找。如果不小于起点，那么
#检查下一个波峰，如果波峰大于临时高点，则该波峰作为临时高点，继续检查下一个波谷

##满足增长的最大回撤
