readHycode =  function()
{
  file = "D:/data/stock/code/hycode.txt"
  codeTable = read.table(file,head=T,sep=',',colClasses=rep('character',4))
  return(codeTable)
}