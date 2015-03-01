#amout为最低交易量，如黄金100为一手
volatilityPositionForTotalEq <- function(pricedata,n=10,ratio=0.01,amount = 1)
{
  vpdata = ATR(pricedata,n)
  computePosition = function(...)
  {
    para = list(...)
    if( is.null(para$eq) || is.null(para$tradeDate))
    {
      print(para)
      stop('error parameters in volatilityPositionForTotalEq')
    }
    atr = vpdata[para$tradeDate]$atr
    #atr为null说明该交易发生在n天以内，暂时没有atr，返回0 不交易
    if(is.na(atr))
    {
      return(0)
    }
    else
    {
      return(as.numeric(trunc(para$eq * ratio/(atr * amount))))
    }
  }
  return(computePosition)
}