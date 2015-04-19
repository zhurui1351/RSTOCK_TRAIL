//+------------------------------------------------------------------+
//|                                                   nbar_type1.mq4 |
//|                        Copyright 2015, MetaQuotes Software Corp. |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "Copyright 2015, MetaQuotes Software Corp."
#property link      "https://www.mql5.com"
#property version   "1.00"
#property strict
#define MAGICMA 20150002
//--- input parameters
input int      n;
input int      m;
input bool     up;
input bool     isLong;// 是否做多
int count_n  ;
int count_m  ;
bool pre_n;
bool ishold;
double lot = 0.01;
int slip = 2;

void holdorder()
{
    count_n++;
   if(count_n == n)
   {
      int res= 0 ;
      if(isLong == True)
      {
         res =  OrderSend(Symbol(),OP_BUY,lot,Ask,slip,0,0,"",MAGICMA,0,Red);   
      } 
      else
      {
         res =  OrderSend(Symbol(),OP_SELL,lot,Bid,slip,0,0,"",MAGICMA,0,Red);  
      }                          
      if(res > 0)
      { 
        ishold = True;
      }
      count_n = 0;
      pre_n = False;
   }
}

//平仓所有订单
void clearOrders()
{
   for(int cnt=OrdersTotal();cnt>=0;cnt--)
	{
		if(!OrderSelect(cnt,SELECT_BY_POS,MODE_TRADES)) continue;
		if(OrderSymbol()==Symbol() && OrderMagicNumber()==MAGICMA)
		{   
		   if(OrderType() == OP_BUY){
		      while(!OrderClose(OrderTicket(),OrderLots(),Bid,slip));
		   }
		   if(OrderType() == OP_SELL){
		      while(!OrderClose(OrderTicket(),OrderLots(),Ask,slip));
		   }
		   
		}
   }
}
//+------------------------------------------------------------------+
//| Expert initialization function                                   |
//+------------------------------------------------------------------+
int OnInit()
  {
//---
     count_n = 0 ;
     count_m = 0 ;
     pre_n = FALSE;
     ishold = FALSE;
//---
   return(INIT_SUCCEEDED);
  }
//+------------------------------------------------------------------+
//| Expert deinitialization function                                 |
//+------------------------------------------------------------------+
void OnDeinit(const int reason)
  {
//---
   
  }
//+------------------------------------------------------------------+
//| Expert tick function                                             |
//+------------------------------------------------------------------+
void OnTick()
  {
//---
   pre_n = True;
   if(Volume[0]>1) return; //新bar时去交易
   bool uporfall =((Close[1] - Open[1]) > 0)?True:False;//前一个bar涨跌情况
   if(ishold == False) //未持仓
   {
      if(pre_n == False && up == True && uporfall == True)
      {
         pre_n = True;
      }
      if(pre_n == False && up == False && uporfall == False)
      {
         pre_n = True;
      }
      if(pre_n == True && up == True && uporfall ==True)
      {
        holdorder();
      }
      else if(pre_n == True && up == False && uporfall ==False)
      {
          holdorder();
      }
      else
      {
         count_n = 0;
         pre_n = False;
      }
   }
   else // 已经持仓
   {
      count_m++;
      if(count_m == m)
      {
         clearOrders();
         count_m = 0;
      }
   }
   
  }
//+------------------------------------------------------------------+
