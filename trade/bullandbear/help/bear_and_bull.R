#find bull 
find_bull_first = function(upratio = 1,downratio = -0.3,shindex,ispoint=F,uppoint = 100,downpoint = -50)
{
  templow =as.numeric(Lo(shindex[1,]))
  temphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  end = index(shindex[1,])
  for(i in 2:nrow(shindex))
  {
    
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    templow_1 = templow
    
    if(curLo < templow)
    {
      templow = curLo
      from = curdate
      to = curdate
    }
    
    if(ispoint == F)
    {
      flag = ((curHi-templow_1) / templow_1) > upratio 
    }
    else
    {
      flag = (curHi-templow_1) > uppoint
    }
    
    if( flag)
    {
      to = curdate
      end = to
      temphi = curHi
      break;
    }
  }
  
  if(i == nrow(shindex))
  {
    return(c(from=from,to=curdate,end = end))
  }
  
  for(j in (i+1) : nrow(shindex))
  {
    cur = shindex[j,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    temphi_1 = temphi
    
    if(curHi > temphi)
    {
      to = curdate
      temphi = curHi
      end = to
    }
    
    if(ispoint == F)
    {
      flag = ((curLo - temphi_1) / temphi_1) < downratio
    }
    else
    {
      flag = (curLo - temphi_1) < downpoint
    }
    
    if(flag)
    {
      end = curdate
      break
    }
  }
  
  return(c(from=from,to=to,end = end))
  #dygraph(Cl(shindex[paste(from,end,sep='/')]))
}

find_bull = function(upratio = 1,downratio = -0.3,shindex,ispoint = F,uppoint = 100,downpoint = -50)
{
  i = 1
  l = list()
  li = 1
  while(i < nrow(shindex))
  {
    temp = find_bull_first(upratio,downratio,shindex[i:nrow(shindex),],ispoint,uppoint,downpoint)
    l[[li]] =  temp
    li = li + 1
    
    to = temp['to']
    pos = which(index(shindex) == to)
    
    i = pos+1
  }
  return(l)
}

#find bear market

#find bear
find_bear_first = function(upratio = 0.2,downratio = -0.4,shindex,ispoint = F,uppoint = 40,downpoint = -100)
{
  templow =as.numeric(Lo(shindex[1,]))
  temphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  end =  index(shindex[1,])
  for(i in 2:nrow(shindex))
  {
    
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    temphigh_1 = temphi
    
    if(curHi > temphi)
    {
      temphi = curHi
      from = curdate
      to = curdate
    }
    
    if(ispoint == F)
    {
      flag =  ((curLo-temphigh_1) / temphigh_1) < downratio 
    }
    else
    {
      flag = (curLo-temphigh_1) < downpoint
    }
    
    if(flag)
    {
      to = curdate
      end = to
      templow = curLo
      break;
    }
  }
  
  if(i == nrow(shindex))
  {
    return(c(from=from,to=curdate,end = end))
  }
  
  for(j in (i+1) : nrow(shindex))
  {
    cur = shindex[j,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    templow_1 = templow
    
    if(curLo < templow)
    {
      to = curdate
      templow = curLo
      end = to
    }
    
    
    if(ispoint == F)
    {
      flag =  ((curHi - templow_1) / templow_1) > upratio 
    }
    else
    {
      flag = (curHi - templow_1) > uppoint
    }
    
    if(flag)
    {
      end = curdate
      break
    }
  }
  
  return(c(from=from,to=to,end = end))
  #dygraph(Cl(shindex[paste(from,end,sep='/')]))
}


find_bear = function(upratio = 0.2,downratio = -0.4,shindex,ispoint = F,uppoint = 40,downpoint = -100)
{
  i = 1
  l = list()
  li = 1
  while(i < nrow(shindex))
  {
    temp = find_bear_first(upratio,downratio,shindex[i:nrow(shindex),],ispoint,uppoint,downpoint)
    l[[li]] =  temp
    li = li + 1
    
    to = temp['to']
    pos = which(index(shindex) == to)
    
    i = pos+1
  }
  return(l)
}

##  阶段划分：距离最近低点 上涨 30%以前为一阶段
## 开上上涨30%到上涨最高幅度之间为二阶段(中间回撤20%  反弹结束 继续以前期低点搜寻)
## 最近高点回撤 30% 为3阶段
## 30%回撤到最低点为4阶段 （中间反弹20%  下跌结束 继续以前期高点为参考搜寻）
## 
find_upstage_enter = function(shindex,startupratio = 0.3, upratio = 1,breakdownratio = -0.3, downratio = -0.2)
{
 
  tempuplow =as.numeric(Lo(shindex[1,]))
  tempuphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  end =  index(shindex[1,])
  
  #记录位置
  records = data.frame()
  recordsindex = 1
  
  i = 2
 # for(i in 2:nrow(shindex))
  while(i < nrow(shindex))
  {
  #  print(i)
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    tmp_up_low = tempuplow

    #遇到更低点
    if(curLo < tempuplow)
    {
      #重新定位当前最低点
      tempuplow = curLo
      from = curdate
      to = curdate
      end = curdate
    }
    
   # startupratio = 0.3
    #向上 找到满足符合要求的点
    if( ((curHi-tmp_up_low) / tmp_up_low) > startupratio )
    {
     # print(i)
     # return('')
      to = curdate
      end = to
      tempuphi = curHi
      startenteruppoint = curdate
      starthi = curHi
      #记录相关值
      #在到达牛市(100%前)顶点前回撤是否达到20%
      for(j  in (i+1) : nrow(shindex))
      {
        if(j >= nrow(shindex))
        {
         # return('')
          r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate ,flag='notend')
          records = rbind(records,r)
          recordsindex = recordsindex + 1
          print('1')
          return(list(records,list(from=from,to=to,end=end)))
        }
        
        cur = shindex[j,]
        curHi = as.numeric(Hi(cur))
        curLo = as.numeric(Lo(cur))
        curdate = index(cur)
        
        #继续创新高
        if(curHi > tempuphi)
        {
          to = curdate
          tempuphi = curHi
          end = curdate
        }
       # upratio = 1
        #成功达到牛市
        if( ((curHi-tmp_up_low) / tmp_up_low) > upratio )
        {
          # print(j)
          # return('')
          #寻找牛市终点
          i = j + 1
          if(i >= nrow(shindex))
          {
            r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate,flag='succ')
            records  = rbind(records,r)
            return(list(records,list(from=from,to=to,end=end)))
          }
          #寻找牛市终点
          for(j in i : nrow(shindex))
          {
           
            cur = shindex[j,]
            curHi = as.numeric(Hi(cur))
            curLo = as.numeric(Lo(cur))
            curdate = index(cur)
            
            if(j >= nrow(shindex))
            {
            #  print('end')
             # return('')
              r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate,flag='succ')
              print(r)
              records  = rbind(records,r)
              recordsindex = recordsindex + 1
              return(list(records,list(from=from,to=to,end=end)))
            }
            
            temphi_1 = tempuphi
            
            if(curHi > tempuphi)
            {
              to = curdate
              tempuphi = curHi
              end = to
            }
          #  breakdownratio = -0.3
            if(((curLo - temphi_1) / temphi_1) < breakdownratio)
            {
              r = data.frame(from=from,start = startenteruppoint,to=to,end = curdate,flag='succ' )
              records  = rbind(records,r)
              pos = which(index(shindex) == to)
              i = pos+1
              tempuplow =as.numeric(Lo(shindex[i,]))
              tempuphi = as.numeric(Hi(shindex[i,]))
              from = index(shindex[i,])
              to = index(shindex[i,])
              end =  index(shindex[i,])
              break
            }
        #  break
          }
          break
        }
        
        #回撤20%以上，结束
       # downratio = -0.2
        if(((curLo - starthi) / starthi) < downratio)
        {
          r = data.frame(from=from,start = startenteruppoint,to=to,end=curdate,flag='failup' )
          i = j + 1
          records  = rbind(records,r)
          recordsindex = recordsindex + 1
          
          break
        }
      }
      
    }
    i = i + 1
  }
  return(list(records,list(from=from,to=to,end=end)))
}



#根据点位进行牛熊波段分析
find_upstage_enter_points = function(shindex,startuppoint = 30, uppoint = 100,breakdownpoint = -30, downpoint = -20)
{
  
  tempuplow =as.numeric(Lo(shindex[1,]))
  tempuphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  end =  index(shindex[1,])
  
  #记录位置
  records = data.frame()
  recordsindex = 1
  
  i = 2
  # for(i in 2:nrow(shindex))
  while(i < nrow(shindex))
  {
    #  print(i)
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    tmp_up_low = tempuplow
    
    #遇到更低点
    if(curLo < tempuplow)
    {
      #重新定位当前最低点
      tempuplow = curLo
      from = curdate
      to = curdate
      end = curdate
    }
    
    # startupratio = 30
    #向上 找到满足符合要求的点
    if( (curHi-tmp_up_low)  > startuppoint )
    {
      # print(i)
      # return('')
      to = curdate
      end = to
      tempuphi = curHi
      startenteruppoint = curdate
      starthi = curHi
      #记录相关值
      #在到达牛市(100%前)顶点前回撤是否达到20%
      for(j  in (i+1) : nrow(shindex))
      {
        if(j >= nrow(shindex))
        {
          # return('')
          r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate ,flag='notend')
          records = rbind(records,r)
          recordsindex = recordsindex + 1
          print('1')
          return(list(records,list(from=from,to=to,end=end)))
        }
        
        cur = shindex[j,]
        curHi = as.numeric(Hi(cur))
        curLo = as.numeric(Lo(cur))
        curdate = index(cur)
        
        #继续创新高
        if(curHi > tempuphi)
        {
          to = curdate
          tempuphi = curHi
          end = curdate
        }
        # upratio = 1
        #成功达到牛市
        if( (curHi-tmp_up_low) > uppoint )
        {
          # print(j)
          # return('')
          #寻找牛市终点
          i = j + 1
          if(i >= nrow(shindex))
          {
            r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate,flag='succ')
            records  = rbind(records,r)
            return(list(records,list(from=from,to=to,end=end)))
          }
          #寻找牛市终点
          for(j in i : nrow(shindex))
          {
            
            cur = shindex[j,]
            curHi = as.numeric(Hi(cur))
            curLo = as.numeric(Lo(cur))
            curdate = index(cur)
            
            if(j >= nrow(shindex))
            {
              #  print('end')
              # return('')
              r = data.frame(from=from,start = startenteruppoint,to=curdate,end = curdate,flag='succ')
              print(r)
              records  = rbind(records,r)
              recordsindex = recordsindex + 1
              return(list(records,list(from=from,to=to,end=end)))
            }
            
            temphi_1 = tempuphi
            
            if(curHi > tempuphi)
            {
              to = curdate
              tempuphi = curHi
              end = to
            }
            #  breakdownratio = -0.3
            if((curLo - temphi_1) < breakdownpoint)
            {
              r = data.frame(from=from,start = startenteruppoint,to=to,end = curdate,flag='succ' )
              records  = rbind(records,r)
              pos = which(index(shindex) == to)
              i = pos+1
              tempuplow =as.numeric(Lo(shindex[i,]))
              tempuphi = as.numeric(Hi(shindex[i,]))
              from = index(shindex[i,])
              to = index(shindex[i,])
              end =  index(shindex[i,])
              break
            }
            #  break
          }
          break
        }
        
        #回撤20%以上，结束
        # downratio = -0.2
        if((curLo - starthi) < downpoint)
        {
          r = data.frame(from=from,start = startenteruppoint,to=to,end=curdate,flag='failup' )
          i = j + 1
          records  = rbind(records,r)
          recordsindex = recordsindex + 1
          
          break
        }
      }
      
    }
    i = i + 1
  }
  return(list(records,list(from=from,to=to,end=end)))
}


# manul flag bear and bull
flag_for_shindex = function(shindex)
{
  #上涨100%，跌幅 30%
  bear = list(c(from = '1991-01-02',to='1992-05-26'),c(from = '1992-11-17',to='1993-02-16'),c(from = '1994-07-29',to='1994-09-13')
              ,c(from = '1996-01-19',to='1997-05-12'),c(from = '1999-05-19',to='2001-06-14')
              ,c(from = '2005-06-06',to='2007-10-16'),c(from = '2008-10-28',to='2009-08-04'),c(from = '2014-07-22',to='2015-06-12'))
  
  bull = list(c(from = '1992-05-26',to='1992-11-17'),c(from = '1993-02-16',to='1994-07-29'),c(from = '1994-09-13',to='1996-01-19')
              ,c(from = '1997-05-12',to='1999-05-18'),c(from = '2001-06-14',to='2005-06-06'),c(from = '2007-10-16',to='2008-10-28')
              ,c(from = '2009-08-04',to='2010-07-02'))
  
}

