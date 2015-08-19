codeTable = readHycode()
code = '600390'
p = readOneStock(code)
pweek = readOnestockweek(code,shindex_week,codeTable)
rtotal = showcurrentPostion(date=Sys.Date(),code=code)

if(rtotal$Close < rtotal$stopprice)
  print(paste("notice",code,"less than stopprice you must clean it!!!"),sep=' ')
