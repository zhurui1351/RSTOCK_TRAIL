rm(list=ls(all=TRUE))
require(quantmod)
require(TTR)

#初始数据处理，加入周、周几、每日涨跌信息
#获取每日涨跌信息，1为涨,-1为跌，0为持平
initialData <- function(data)
{
  mydata = data
  mydata$volatility <- sign(Cl(mydata)-Op(mydata))
  #获取礼拜几信息
  wdays <- xts(strftime(index(mydata), "%u"),index(mydata))
  names(wdays) <- 'weekday'
  #获取第几周的信息
  weekth <- xts( strftime(index(mydata), "%U"),index(mydata))
  names(weekth) <- 'weekth'
  #使用merge.xts报错，未知原因
  mydata <- data.frame(mydata[,c(1,2,3,4,7)],wdays[,1],weekth[,1])
  mydata <- as.xts(mydata)
  #初始化数据处理结束
  return(mydata)
}



#定位周内数据序列，输入初始化处理后的对象，开始的索引值
#返回从index开始属于同一周的数据的张跌值以及真实数据的个数
getVolatilityInOneWeek <- function(priceData,start_index)
{
  #初始化返回值，位置代表周几，值： 0表示没有涨跌（如当天没有数据也可理解为涨跌为0）
  #-1代表跌 1代表涨
  #return_data = c(0,0,0,0,0)
  return_data = rep(0,tradeDays)

  #真实数据个数 当天有可能休假，在数据中可能不存在当天的数据
  length = 0
  #获取当前数据是第几周
  week = as.integer(priceData[start_index,'weekth'])
  #当前数据是周内第几天
  day = as.integer(priceData[start_index,'weekday'])
  
  #一周按tradeDays天计算，不足tradeDays天的进行补齐
  weekLength = tradeDays
  if(( nrow(priceData) - start_index + day) < tradeDays)
  {
    weekLength = nrow(priceData) - start_index + day
  }
  
  for(i in day:weekLength)
  {
    currentWeek = as.integer(priceData[start_index+i-day,'weekth'])
    #周数已经变化，中断循环
    if(currentWeek != week)
    {
      break
    }
    return_data[i] = priceData[start_index+i-day,'volatility']
    length = length + 1
  }
  #返回值，列表 第一项是当周的涨跌，第二项是实际数据个数
  return(list(return_data,length))
}

#rules表示规则 1为涨，-1为跌，0表示不考虑，位置索引值代表周几
#rules <- c(-1,1,0,0,0) 表示周一跌，周二涨的情况
#产生rules，计算基础概率
#本质上是求幂集
#该函数产生所有关心涨跌的位置指数比如对于c(1,2)产生c(1),c(2),c(1,2)三个组合
#代表关心周一涨跌或者周二涨跌，或者周一周二涨跌都关心
generateRulespos <- function(data){
  if(length(data) == 1)
  {
    return(list(data[1]))
  }
  else
  {
    #递归产生所有子集
    rules = generateRulespos(data[2:length(data)])
    extend_rules = lapply(rules,function(x,n){c(n,x)},n=data[1])
    rules = c(list(data[1]),rules,extend_rules)
    return(rules)
  }
}

#根据位置参数设置一条rule，每个位置有涨（1）、跌（-1）两种选择
#比如输入参数c(1,2),返回c(1,1,0,0,0) c(1,-1,0,0,0) c(-1,1,0,0,0) c(-1,-1,0,0,0)
getRuleByPos<-function(posRule)
{
  if(length(posRule) == 1)
  {
    #只有一个位置，那么将该位置设为1或-1
    rule = rep(0,tradeDays)
    rule1 = rep(0,tradeDays)
    rule[posRule[1]] = 1
    rule1[posRule[1]] = -1
    return(list(rule,rule1))
  }
  else
  {
    #递归求解，先获得其他位置的rules，然后将对应位置设置为1或-1
    otherRule = getRuleByPos(posRule[2:length(posRule)])
    positiveRule = lapply(otherRule,function(x,n){x[n]=1;x},n=posRule[1])
    negRule = lapply(otherRule,function(x,n){x[n]=-1;x},n=posRule[1])
    #合并rule，形成行的rule列表
    rules = c(positiveRule,negRule)
    return(rules)
  }
}

#产生所有的规则，对于每个关心的天，可以设置为关心涨（1）、跌（-1）、其余为0 表示不关心
#只对rules小于等于prune的rule感兴趣
generateRules<-function(prune=5)
{
  rules = list()
  #产生所有的位置组合
  pos = generateRulespos(c(1:tradeDays))
  #剪枝，待实现
  for(i in 1:length(pos))
  {
    if(length(pos[[i]]) > prune)
    {
      next
    }
    rule = getRuleByPos(pos[[i]])
    rules = c(rule,rules)
  }
  return(rules)
}


#判断涨跌是否符合规则
isSatisfiedRule<- function(data,rule)
{
  for(i in 1 : length(rule))
  {
    #规则为0，代表不考虑当天的涨跌
    if(rule[i] == 0)
    {
      next
    }
    #不满足规则
    if(rule[i] != data[i])
    {
      return(FALSE)
    }
  }
  return(TRUE)
}

#依照规则进行模式匹配
compareToRule <- function(rule,mydata)
{
  total = nrow(mydata)
  i = 1
  #数据中总共含有多少周
  numweek = 0
  #符合规则的周数
  num_satisfied = 0
  while(i < total)
  {
    #周内循环
    infoInOneWeek = getVolatilityInOneWeek(mydata,i)
    vol = infoInOneWeek[[1]]
    num = infoInOneWeek[[2]]
    i = i + num
    #如果符合规则，那么结果加1
    if(isSatisfiedRule(data=as.integer(vol),rule=rule))
    {
      num_satisfied = num_satisfied + 1    
    }
    numweek = numweek + 1
  }
  ratio = num_satisfied / numweek
  print(ratio)
  return(c(ratio,num_satisfied,numweek))
}

#根据rules集合，求所有的基础概率

getBasePropByRules <- function(rules,mydata)
{
  base_prob = list()
  for(i in 1 : length(rules))
  {
    result = compareToRule(rules[[i]],mydata)
    base_prob[[i]] = list(rule=rules[[i]],ratio=result[1],num_satisfied=result[2],numweek=result[3])
  }
  return(base_prob)  
}

#合并rules，计算条件概率
#对于两条rule 前序相同，P(A)后一位为0,P(B)后一位不为0，再后面的位数都为0，可以计算条件概率
#对于c(1,0,0,0,0) 和 c(1,1,0,0,0) 可以计算出 周一涨后周二也涨的条件概率

#是否可以形成条件概率
isConditionProb <- function(rule,rule1)
{
  r = rule$rule
  r1 = rule1$rule
  match = (r == r1)
  #P(A) P(B) 只会有一项规则不同
  if(length(match[match==FALSE]) != 1)
    return(FALSE)
  index = which(match==FALSE)
  #为0时才能计算条件概率
  if(r[index] !=0 && r1[index] !=0)
  {
    return(FALSE)
  }
  #如果不是最后一位，那么index后面的值都为0
  if(index != length(r))
  {
    if(!all(r[(index+1) : length(r)] == 0))
      return(FALSE)
    if(!all(r1[(index+1) : length(r1)] == 0))
      return(FALSE)
  }
  return(TRUE)
}

#计算条件概率
conditionProb <- function(baseProb1,baseProb2)
{
  #检验哪个条件作为前验条件
  if(baseProb1$num_satisfied <= baseProb2$num_satisfied)
  {
    prob = baseProb1$num_satisfied / baseProb2$num_satisfied
    rule = list(pre=baseProb2$rule,cons=baseProb1$rule,prob=prob,numpre=baseProb2$num_satisfied,numcons=baseProb1$num_satisfied)
  }
  else
  {
    prob = baseProb2$num_satisfied / baseProb1$num_satisfied
    rule = list(pre=baseProb1$rule,cons=baseProb2$rule,prob=prob,numpre=baseProb1$num_satisfied,numcons=baseProb2$num_satisfied) 
  }
  return(rule)
}

#根据基础概率 获得所有的条件概率
getConditionRules <- function(mybase_prob)
{
  condition_rules <- list()
  index_condition_rule <- 1
  #对基础概率两两匹配
  for(i in 1 : length(mybase_prob))
  {
    
    rule = mybase_prob[[i]]
    j = i + 1
    while(j<=length(mybase_prob))
    {
      rule1 = mybase_prob[[j]]
      #如果两个规则可以合并
      if(isConditionProb(rule,rule1))
      {
        newRule = conditionProb(rule,rule1)
        #保存得到的条件概率
        condition_rules[[index_condition_rule]] = newRule
        index_condition_rule = index_condition_rule + 1
      }
      j = j + 1
    }
    
  }
  
  return(condition_rules)
}

#对基础概率以及条件概率进行过滤，使用prop.test进行测试
#大于一定概率 ，p-value在0.05以上的规则被认为是有显著统计特性的规则
#假设检验，原假设为比率大于confProp
getInterestingRules <- function(confProp,conf,base_prob,condition_rules)
{
  interestingRules = list()
  rule_index = 1
  #寻找基本概率满足条件的rule
  for(i in 1:length(base_prob))
  {
    #符合比例的个数
    satisfied = base_prob[[i]]$num_satisfied
    #总周数
    numweek = base_prob[[i]]$numweek
    #比例
    ratio = base_prob[[i]]$prob
    #进行假设检验
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #如果p-value 满足大于conf，则认为有显著性
    if(p$p.value > conf && ratio >=confProp)
    {
      rule = base_prob[[i]]
      rule$type = 'base'
      rule$pvalue = p$p.value
      interestingRules[[rule_index]] = rule
      rule_index = rule_index + 1
    }
  }
  #寻找条件概率满足条件的rule
  for(i in 1:length(condition_rules))
  {
    #符合比例的个数
    satisfied = condition_rules[[i]]$numcons
    #总周数
    numweek = condition_rules[[i]]$numpre
    #比例
    ratio = condition_rules[[i]]$prob
    #进行假设检验
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #如果p-value 满足大于conf，则认为有显著性
    if(p$p.value > conf  && ratio >=confProp)
    {
      rule = condition_rules[[i]]
      rule$type = 'condition'
      rule$pvalue = p$p.value
      interestingRules[[rule_index]] = rule
      rule_index = rule_index + 1
    }
  }
  return(interestingRules)
}
#周内SN算法
#大于一定概率 ，p-value在0.05以上的规则被认为是有显著统计特性的规则
#假设检验，原假设为比率大于confProp,pvalue大于conf
#p-vaule 大于conf
SN_weekly <- function(stockdata,confProp=0.6,conf=0.5)
{
  #初始化数据
  stockdata = initialData(stockdata)
  
  #过滤条件，只对长度小于prune的规则感兴趣
  prune = 5
  
  #每条规则 1为涨，-1为跌，0表示不考虑，位置索引值代表周几 
  #比如周一涨，周二跌,其余不关心 表示为 ,c(1,-1,0,0,0)
  #产生所有的周内规则
  rules = generateRules(prune=prune)
  #保存基础的rule，以及出现的频数、比例
  base_prob = getBasePropByRules(rules,stockdata)
  
  #计算条件概率
  condition_rules = getConditionRules(base_prob)
  #得到有意思的条件集合
  interestingRules = getInterestingRules(confProp,conf,base_prob,condition_rules)
  
  return(interestingRules)
}

############################################################################
#主程序开始
############################################################################
#设置系统语言
Sys.setlocale(locale = 'C')
#获取IBM数据
#IBM <- getSymbols("IBM",auto.assign=FALSE,from='2010-01-01')
load('ibm.data')
#除权除息
#IBM=adjustOHLC(IBM)
#获取数据,以数据为文件为例
path = "D:/stock/dest"
files <- dir(path)
files = c('SH600000.TXT')
f = files[1]
fname <- file.path(path,f)
#读入数据
try(stockdata <- read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
#stockdata<-IBM
colnames(stockdata) <- c("Open","High","Low","Close","Volume","Amount")
#设置时间格式
index(stockdata) <- as.POSIXct(index(stockdata))
#转化为xts对象
stockdata <- as.xts(stockdata)

#每周交易天数
tradeDays = 5
#概率门槛值
confProp=0.6
#pvalue门槛值
conf=0.05

#调用周内SN算法
myrules <- SN_weekly(stockdata,confProp,conf)

print(myrules)