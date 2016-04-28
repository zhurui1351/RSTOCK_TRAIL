regressionTest = function(stats_frame,fm,pricedata,ratio=2,num = 300)
{
  records = data.frame()
  first = F
  ishold = F
  long = F
  short = F
  type = ''
  x = 0
  
  for(i in (num+1):nrow(stats_frame))
  {
    f = lm(fm,data = stats_frame[(i - num):(i-1),])
    sf = summary(f)
    se = sf$sigma
    prv = predict(f,stats_frame[i,c(-1)],level = 0.95,interval="prediction")
    pv = prv[1]
    plwr = prv[2]
    pupr = prv[3]
    
    tv = stats_frame[i,1]
    it = rownames(stats_frame[i,])
    price = pricedata[it]
    gap = pv - tv
    r2 = sf$adj.r.squared
    
    condition  = abs(gap) > ratio * se
    #condition = tv > pupr || tv < plwr
    
    condition1 = gap > 0
    #condition1 = tv < plwr
    
    if(short || long)
    {
      open = as.numeric(Op(price))
      type = ifelse(short==T,'short','long')
      short = F
      long = F
      ishold = T
      entertime = it
      x = i
      
    }
    
    if(condition && ishold == F && r2 > 0.3)
    {
      gap_s = pv - tv
      r2_s = sf$adj.r.squared
      se_s = se
      if(condition1 )
      {
        long = T
      }else
      {
        short = T
      }
    }
    
    if(ishold == T)
    {
      if(condition)
      {
        next
      }
      outit = rownames(stats_frame[i+1,])
      outprice = pricedata[outit]   
      out = as.numeric(Op(outprice))
      outtime = outit
      r = data.frame(entertime = entertime,open=open,out=out,outtime=outtime,type = type,profit = ifelse(type=='long',out-open,open-out),i=x,gap=gap_s,r2=r2_s,se=se_s)
      records = rbind(records,r)
      ishold = F
    }
  }
  return(records)
}


cl_dou1 = dou1_day$Close
cl_douyou = douyou_day$Close
cl_doubo = doubo_day$Close
cl_corp = corp_day$Close

cl = merge(cl_dou1,cl_douyou,cl_doubo,cl_corp)
cl = na.omit(cl)
names(cl)= c('dou','douyou','doubo','corp')
cl = as.data.frame(cl)
fm = dou ~ .
f = lm(dou~.,data = cl[1:300,])

idx = data.frame()
for(i in 301:nrow(cl))
{
  f = lm(dou~.,data = cl[(i-300):(i-1),])
  sf = summary(f)
  se = sf$sigma
  pv = predict(f,cl[i,c(2,3,4)])
  tv = cl[i,1]
  if(abs(tv - pv) > 2 * se)
  {
    r = data.frame(i=i,sp = pv - tv)
    idx = rbind(idx,r)
  }
}

records = regressionTest(cl,fm,dou1_day)


# 分钟数据
cl_dou1_m = Cl(to.hourly(dou1_m))
cl_douyou_m = lag(Cl(to.hourly(douyou_m)),0)
cl_doubo_m = lag(Cl(to.hourly(doubo_m)),0)
cl_corp_m = Cl(to.hourly(corp_m))

cl_m = merge(cl_dou1_m,cl_douyou_m,cl_doubo_m)
cl_m = na.omit(cl_m)
names(cl_m)= c('dou','douyou','doubo')
cl_m = as.data.frame(cl_m)
dou1 = to.hourly(dou1_m)
records = regressionTest(cl_m,fm,dou1)
