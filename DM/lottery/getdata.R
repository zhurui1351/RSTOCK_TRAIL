require(RCurl)
require(XML)
#round = 1
#url = 'http://saishi.zgzcw.com/summary/liansaiAjax.action?source_league_id=36&currentRound=4&season=2004-2005&seasonType='
#http://saishi.zgzcw.com/soccer/league/36/2011-2012/
result = data.frame()

seasons = c('2003-2004','2004-2005','2005-2006','2006-2007','2007-2008','2008-2009',
            '2009-2010','2010-2011','2011-2012','2012-2013','2013-2014','2014-2015',
            '2015-2016','2016-2017')
for(season in seasons)
{
  print(season)
  for(round in 1:38)
  {
    print(round)
    url = paste('http://saishi.zgzcw.com/summary/liansaiAjax.action?source_league_id=36&currentRound=',round,'&season=',season,sep='')
    webpage <- getURL(url,encoding = "utf8")
    webpage <- readLines(tc <- textConnection(webpage),encoding='gbk'); close(tc)
    webpage = enc2utf8(webpage)
    webpage = trimws(webpage)
    
    pagetree <- htmlTreeParse(webpage,encoding='UTF-8',asText = TRUE,useInternalNodes = TRUE)
    
    trs = getNodeSet(pagetree,'//table/tr')
    hrefs = getNodeSet(pagetree,'//table/tr/td[7]')
    
    for(i in 1:length(trs))
    {
      hs = getHTMLLinks(hrefs[[i]])
      context = readHTMLList(trs[[i]])
      r = data.frame(时间=context[1],主队=context[2],比分=context[3],客队=context[4],半场=context[5],
                     亚=hs[1],欧=hs[2],析=hs[3],round=round,season=season)
      result = rbind(result,r)
    }
  }
  
}
result$id = 1:nrow(result)
write.csv(result,file='d:/football.csv',row.names=F)

fault_asia = c()
#爬取亚洲赔率
result_asia = data.frame()
urls = result$亚
ids = result$id
for(i in 1:length(urls))
{
 # Sys.sleep(5)
  bug = 
  tryCatch({
  url = urls[i]
  if(is.na(url)) next
  id = ids[i]
  print(url)
  print(i)
  webpage <- getURL(url,encoding = "utf8")
  webpage = enc2utf8(webpage)
  tables = readHTMLTable(webpage,header = F)
  t1 = tables[[3]]
  },error=function(e){0})
  if(is.null(t1) || nrow(t1) ==0) next
  if(bug == 0)
  {
    fault_asia = c(fault_asia,i)
    next
  }
  
  colnames(t1) = c('序号','公司','初始_主','初始_盘','初始_客','最新_主','最新_盘','最新_客',
                   '最新概_主','最新概_客','最新凯利_主','最新凯利_客','赔付率','历史')
  t1$序号 = id
  result_asia = rbind(result_asia,t1)
  
}
write.csv(result_asia,file='d:/football_asia_lottery.csv',row.names=F)


Sys.sleep(1000)
#爬取欧洲赔率
fault_eur = c()
result_eur = data.frame()
urls = result$欧
ids = result$id
for(i in 2248:length(urls))
{
  Sys.sleep(1)
  bug = 
    tryCatch({
      url = urls[i]
      if(is.na(url)) next
      id = ids[i]
      webpage <- getURL(url,encoding = "utf8")
      webpage = enc2utf8(webpage)
      tables = readHTMLTable(webpage,header = F,encoding='utf8')
      t1 = tables[[3]]
    },error=function(e){0})
  if(is.null(t1) || nrow(t1) ==0) next
  if(bug ==0 ){
    fault_eur = c(fault_eur,i)
    next
  }
  colnames(t1) = c('序号','公司','初始_胜','初始_平','初始_负','最新_胜','最新_平','最新_负',
                   '最新概_主','最新概_平','最新概_客','最新凯利_主','最新凯利_平' ,'最新凯利_客','赔付率','历史')
  t1$序号 = id
  result_eur = rbind(result_eur,t1)
  print(url)
  print(i)
}
write.csv(result_eur,file='d:/football_eur_lottery.csv',row.names=F)
