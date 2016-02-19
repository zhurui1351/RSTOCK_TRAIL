rm(list=ls(all=TRUE))
require(quantmod)
require(TTR)

#��ʼ���ݴ����������ܡ��ܼ���ÿ���ǵ���Ϣ
#��ȡÿ���ǵ���Ϣ��1Ϊ��,-1Ϊ����0Ϊ��ƽ
initialData <- function(data)
{
  mydata = data
  mydata$volatility <- sign(Cl(mydata)-Op(mydata))
  #��ȡ��ݼ���Ϣ
  wdays <- xts(strftime(index(mydata), "%u"),index(mydata))
  names(wdays) <- 'weekday'
  #��ȡ�ڼ��ܵ���Ϣ
  weekth <- xts( strftime(index(mydata), "%U"),index(mydata))
  names(weekth) <- 'weekth'
  #ʹ��merge.xts������δ֪ԭ��
  mydata <- data.frame(mydata[,c(1,2,3,4,7)],wdays[,1],weekth[,1])
  mydata <- as.xts(mydata)
  #��ʼ�����ݴ�������
  return(mydata)
}



#��λ�����������У������ʼ��������Ķ��󣬿�ʼ������ֵ
#���ش�index��ʼ����ͬһ�ܵ����ݵ��ŵ�ֵ�Լ���ʵ���ݵĸ���
getVolatilityInOneWeek <- function(priceData,start_index,tradeDays=5)
{
  #��ʼ������ֵ��λ�ô����ܼ���ֵ�� 0��ʾû���ǵ����統��û������Ҳ������Ϊ�ǵ�Ϊ0��
  #-1������ 1������
  #return_data = c(0,0,0,0,0)
  return_data = rep(0,tradeDays)

  #��ʵ���ݸ��� �����п����ݼ٣��������п��ܲ����ڵ��������
  length = 0
  #��ȡ��ǰ�����ǵڼ���
  week = as.integer(priceData[start_index,'weekth'])
  #��ǰ���������ڵڼ���
  day = as.integer(priceData[start_index,'weekday'])
  
  #һ�ܰ�tradeDays����㣬����tradeDays��Ľ��в���
  weekLength = tradeDays
  if(( nrow(priceData) - start_index + day) < tradeDays)
  {
    weekLength = nrow(priceData) - start_index + day
  }
  
  for(i in day:weekLength)
  {
    currentWeek = as.integer(priceData[start_index+i-day,'weekth'])
    #�����Ѿ��仯���ж�ѭ��
    if(currentWeek != week)
    {
      break
    }
    return_data[i] = priceData[start_index+i-day,'volatility']
    length = length + 1
  }
  #����ֵ���б� ��һ���ǵ��ܵ��ǵ����ڶ�����ʵ�����ݸ���
  return(list(return_data,length))
}

#rules��ʾ���� 1Ϊ�ǣ�-1Ϊ����0��ʾ�����ǣ�λ������ֵ�����ܼ�
#rules <- c(-1,1,0,0,0) ��ʾ��һ�����ܶ��ǵ����
#����rules�������������
#�����������ݼ�
#�ú����������й����ǵ���λ��ָ���������c(1,2)����c(1),c(2),c(1,2)�������
#����������һ�ǵ������ܶ��ǵ���������һ�ܶ��ǵ�������
generateRulespos <- function(days){
  if(length(days) == 1)
  {
    return(list(days[1]))
  }
  else
  {
    #�ݹ���������Ӽ�
    rules = generateRulespos(days[2:length(days)])
    extend_rules = lapply(rules,function(x,n){c(n,x)},n=days[1])
    rules = c(list(days[1]),rules,extend_rules)
    return(rules)
  }
}

#����λ�ò�������һ��rule��ÿ��λ�����ǣ�1��������-1������ѡ��
#�����������c(1,2),����c(1,1,0,0,0) c(1,-1,0,0,0) c(-1,1,0,0,0) c(-1,-1,0,0,0)
getRuleByPos<-function(posRule,tradeDays=5)
{
  if(length(posRule) == 1)
  {
    #ֻ��һ��λ�ã���ô����λ����Ϊ1��-1
    rule = rep(0,tradeDays)
    rule1 = rep(0,tradeDays)
    rule[posRule[1]] = 1
    rule1[posRule[1]] = -1
    return(list(rule,rule1))
  }
  else
  {
    #�ݹ���⣬�Ȼ������λ�õ�rules��Ȼ�󽫶�Ӧλ������Ϊ1��-1
    otherRule = getRuleByPos(posRule[2:length(posRule)],tradeDays)
    positiveRule = lapply(otherRule,function(x,n){x[n]=1;x},n=posRule[1])
    negRule = lapply(otherRule,function(x,n){x[n]=-1;x},n=posRule[1])
    #�ϲ�rule���γ��е�rule�б�
    rules = c(positiveRule,negRule)
    return(rules)
  }
}

#�������еĹ��򣬶���ÿ�����ĵ��죬��������Ϊ�����ǣ�1��������-1��������Ϊ0 ��ʾ������
#ֻ��rulesС�ڵ���prune��rule����Ȥ
generateRules<-function(prune=5,tradeDays=5)
{
  rules = list()
  #�������е�λ�����
  pos = generateRulespos(c(1:tradeDays))
  for(i in 1:length(pos))
  {
    #��֦
    if(length(pos[[i]]) > prune)
    {
      next
    }
    rule = getRuleByPos(pos[[i]],tradeDays)
    rules = c(rule,rules)
  }
  return(rules)
}


#�ж��ǵ��Ƿ���Ϲ���
isSatisfiedRule<- function(data,rule)
{
  for(i in 1 : length(rule))
  {
    #����Ϊ0�����������ǵ�����ǵ�
    if(rule[i] == 0)
    {
      next
    }
    #���������
    if(rule[i] != data[i])
    {
      return(FALSE)
    }
  }
  return(TRUE)
}

#���չ������ģʽƥ��
compareToRule <- function(rule,mydata,tradeDays=5)
{
  total = nrow(mydata)
  i = 1
  #�������ܹ����ж�����
  numweek = 0
  #���Ϲ��������
  num_satisfied = 0
  while(i < total)
  {
    #����ѭ��
    infoInOneWeek = getVolatilityInOneWeek(mydata,i,tradeDays)
    vol = infoInOneWeek[[1]]
    num = infoInOneWeek[[2]]
    i = i + num
    #������Ϲ�����ô�����1
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

#����rules���ϣ������еĻ�������

getBasePropByRules <- function(rules,mydata,tradeDays=5)
{
  base_prob = list()
  for(i in 1 : length(rules))
  {
    result = compareToRule(rules[[i]],mydata,tradeDays)
    base_prob[[i]] = list(rule=rules[[i]],ratio=result[1],num_satisfied=result[2],numweek=result[3])
  }
  return(base_prob)  
}

#�ϲ�rules��������������
#��������rule ǰ����ͬ��P(A)��һλΪ0,P(B)��һλ��Ϊ0���ٺ����λ����Ϊ0�����Լ�����������
#����c(1,0,0,0,0) �� c(1,1,0,0,0) ���Լ���� ��һ�Ǻ��ܶ�Ҳ�ǵ���������

#�Ƿ�����γ���������
isConditionProb <- function(r,r1)
{
  match = (r == r1)
  #P(A) P(B) ֻ����һ�����ͬ
  if(length(match[match==FALSE]) != 1)
    return(FALSE)
  index = which(match==FALSE)
  #Ϊ0ʱ���ܼ�����������
  if(r[index] !=0 && r1[index] !=0)
  {
    return(FALSE)
  }
  #����������һλ����ôindex�����ֵ��Ϊ0
  if(index != length(r))
  {
    if(!all(r[(index+1) : length(r)] == 0))
      return(FALSE)
    if(!all(r1[(index+1) : length(r1)] == 0))
      return(FALSE)
  }
  return(TRUE)
}

#������������
conditionProb <- function(baseProb1,baseProb2)
{
  #�����ĸ�������Ϊǰ������
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

#���ݻ������� ������е���������
getConditionRules <- function(mybase_prob)
{
  condition_rules <- list()
  index_condition_rule <- 1
  #�Ի�����������ƥ��
  for(i in 1 : length(mybase_prob))
  {
    
    rule = mybase_prob[[i]]
    j = i + 1
    while(j<=length(mybase_prob))
    {
      rule1 = mybase_prob[[j]]
      #�������������Ժϲ�
      if(isConditionProb(rule$rule,rule1$rule))
      {
        newRule = conditionProb(rule,rule1)
        #����õ�����������
        condition_rules[[index_condition_rule]] = newRule
        index_condition_rule = index_condition_rule + 1
      }
      j = j + 1
    }
    
  }
  
  return(condition_rules)
}

#�Ի��������Լ��������ʽ��й��ˣ�ʹ��prop.test���в���
#����һ������ ��p-value��0.05���ϵĹ�����Ϊ��������ͳ�����ԵĹ���
#������飬ԭ����Ϊ���ʴ���confProp
getInterestingRules <- function(confProp,conf,base_prob,condition_rules)
{
  interestingRules = list()
  rule_index = 1
  #Ѱ�һ�����������������rule
  for(i in 1:length(base_prob))
  {
    #���ϱ����ĸ���
    satisfied = base_prob[[i]]$num_satisfied
    #������
    numweek = base_prob[[i]]$numweek
    #����
    ratio = base_prob[[i]]$ratio
    #���м������
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #���p-value �������conf������Ϊ��������
    if(p$p.value > conf && ratio >=confProp)
    {
      rule = base_prob[[i]]
      rule$type = 'base'
      rule$pvalue = p$p.value
      interestingRules[[rule_index]] = rule
      rule_index = rule_index + 1
    }
  }
  #Ѱ��������������������rule
  for(i in 1:length(condition_rules))
  {
    #���ϱ����ĸ���
    satisfied = condition_rules[[i]]$numcons
    #������
    numweek = condition_rules[[i]]$numpre
    #����
    ratio = condition_rules[[i]]$prob
    #���м������
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #���p-value �������conf������Ϊ��������
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
#����SN�㷨
#����һ������ ��p-value��0.05���ϵĹ�����Ϊ��������ͳ�����ԵĹ���
#������飬ԭ����Ϊ���ʴ���confProp,pvalue����conf
#p-vaule ����conf
SN_weekly <- function(stockdata,confProp=0.6,conf=0.5,tradeDays=5,prune=5)
{
  #��ʼ������
  stockdata = initialData(stockdata)
  
  #����������ֻ�Գ���С��prune�Ĺ������Ȥ
  
  #ÿ������ 1Ϊ�ǣ�-1Ϊ����0��ʾ�����ǣ�λ������ֵ�����ܼ� 
  #������һ�ǣ��ܶ���,���಻���� ��ʾΪ ,c(1,-1,0,0,0)
  #�������е����ڹ���
  rules = generateRules(prune=prune,tradeDays)
  #���������rule���Լ����ֵ�Ƶ��������
  base_prob = getBasePropByRules(rules,stockdata,tradeDays)
  
  #������������
  condition_rules = getConditionRules(base_prob)
  #�õ�����˼����������
  interestingRules = getInterestingRules(confProp,conf,base_prob,condition_rules)
  
  return(interestingRules)
}

############################################################################
#������ʼ
############################################################################
#����ϵͳ����
Sys.setlocale(locale = 'C')
#��ȡIBM����
#IBM <- getSymbols("IBM",auto.assign=FALSE,from='2010-01-01')
load('ibm.data')
#��Ȩ��Ϣ
#IBM=adjustOHLC(IBM)
#��ȡ����,������Ϊ�ļ�Ϊ��
path = "D:/stock/FOREX"
files <- dir(path)
files = c('AUDJAP.txt')
f = files[1]
fname <- file.path(path,f)
#��������
#try(stockdata <- read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
try(stockdata <- read.zoo(fname,header=TRUE, format ="%Y-%m-%d",sep="\t",index.column=1),TRUE) 
#stockdata<-IBM
colnames(stockdata) <- c("Open","High","Low","Close","Volume","Amount")
#����ʱ���ʽ
index(stockdata) <- as.POSIXct(index(stockdata))
#ת��Ϊxts����
stockdata <- as.xts(stockdata)

#ÿ�ܽ�������
#tradeDays = 5
#�����ż�ֵ
confProp=0.55
#pvalue�ż�ֵe
conf=0.05

#��������SN�㷨
myrules <- SN_weekly(stockdata,confProp,conf,tradeDays=5,prune=3)

print(myrules)
