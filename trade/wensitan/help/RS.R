
RS = function(base,taget)
{
  shdeltratio = na.omit(Delt(base)* 100)
  pdeltratio = na.omit(Delt(taget) * 100)
  
  rsdata = na.omit(merge(shdeltratio,pdeltratio))
  
  rs = rsdata[,2] - rsdata[,1]
  return(rs)
}