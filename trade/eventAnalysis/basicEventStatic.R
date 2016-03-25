basicEventStatic = function(pricedata,eventdates,preperd,afterperd)
{
  pricedata$votile = pricedata$Close - pricedata$Open 
  
  id = c()
  for(d in dates)
  {
    i = findInterval(as.Date(d),index(pricedata))
    #i = which(index(pricedata) == d)
    if( i == 0) next
    id = c(id,i)
  }
  
  result = data.frame()
  for(i in id)
  {
    sep  = preperd
    pre = (i - preperd):i
      
    if(pre[1] < 1) next;
    
    startafter = i + afterperd[1]
    endafter =  i + afterperd[length(afterperd)]
    
    if(endafter > length(index(pricedata))) next

    prevotile = pricedata[pre,]$votile
    premean = mean(abs(prevotile),na.rm=T)
    
    preupratio = length(prevotile[prevotile>0])/length(prevotile)
    aftervotile = as.numeric(pricedata[endafter,]$Close) - as.numeric(pricedata[startafter,]$Open)
    
    votileafter = pricedata[startafter:endafter,]$votile
    aftermean = mean(abs(votileafter),na.rm=T)
    
    result = rbind(result,data.frame(eventdate=index(pricedata)[i],startdate=index(pricedata)[startafter],enddate=index(pricedata)[endafter],premean = premean,aftermean=aftermean, preupratio = preupratio,aftervotile=aftervotile ))
  }
  
  totalnum = nrow(result)
  totolvotile = sum(result$aftervotile)
  premean = mean(result$premean)
  aftermean = mean(result$aftermean)
  preupratio = mean(result$preupratio)
  afterupratio = result$aftervotile
  afterupratio = length(afterupratio[afterupratio>0])/length(afterupratio)
  print('result:')
  print(result)
  print(paste('totalnum:',totalnum))
  print(paste('totolvotile:',totolvotile))
  print(paste('premean:',premean))
  print(paste('aftermean:',aftermean))
  print(paste('preupratio:',preupratio))
  print(paste('afterupratio:',afterupratio))
  print('binom test,preupratio != afterupratio')
  print(binom.test(floor(totalnum*preupratio),totalnum,p=afterupratio))
  return(result)
}