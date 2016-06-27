require(RCurl)
httpheader=c("Authorization"="d0b1ed341de14306b0b3784367c335cf4f72cda746308ac1342dde9a0785555f");
getdata=function(url){
  http=paste("https://api.wmcloud.com:443/data/v1",url,sep = "")
  return (getURL(http, httpheader=httpheader, ssl.verifypeer = FALSE,.encoding="utf8"))
}
result=getdata("/api/master/getSecID.json?field=&assetClass=&ticker=000001,600000&partyID=&cnSpell=")
cat(result)
cat(result,file="Rout.csv")