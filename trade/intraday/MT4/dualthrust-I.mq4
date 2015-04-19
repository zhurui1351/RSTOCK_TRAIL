//+------------------------------------------------------------------+
//|                                                         test.mq4 |
//|                        Copyright 2015, MetaQuotes Software Corp. |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "Copyright 2015, MetaQuotes Software Corp."
#property link      "https://www.mql5.com"
#property version   "1.00"
#property strict
#define MAGICMA  20150001
//+------------------------------------------------------------------+
//| Expert initialization function                                   |
//+------------------------------------------------------------------+
double val_hh;
double val_hc;
double val_lc;
double val_ll;
double range;
double upper_line;
double lower_line;
int day = Day();
double lot = 0.01;
int slip = 2;
int enableTrade = 0;

void calRange()
{
   int val_index_hh = iHighest(NULL,PERIOD_D1,MODE_HIGH,3,1);
   val_hh = iHigh(NULL,PERIOD_D1,val_index_hh);
   
   int val_index_hc = iHighest(NULL,PERIOD_D1,MODE_CLOSE,3,1);
   val_hc =iClose(NULL,PERIOD_D1,val_index_hc);
   
   int val_index_lc = iLowest(NULL,PERIOD_D1,MODE_CLOSE,3,1);
   val_lc = iClose(NULL,PERIOD_D1,val_index_lc);
   
   int val_index_ll = iLowest(NULL,PERIOD_D1,MODE_LOW,3,1);
   val_ll = iLow(NULL,PERIOD_D1,val_index_ll);
   
   range =MathMax(val_hh-val_lc,val_hc-val_ll);
   
   upper_line = iOpen(NULL,PERIOD_D1,0) + 0.2 * range;
   lower_line = iOpen(NULL,PERIOD_D1,0) + 0.25 * range;
   
   Print("upper_line:",upper_line);
   Print("lower_line:",lower_line);
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
   enableTrade = 0;
}
void CheckTime(){  // 是否换日
   if(day < Day()){ // 隔日
      clearOrders(); //日内平仓
      day = Day(); 
      calRange();   
   }
}


int OnInit()
  {
//---
//---
   EventSetTimer(50);
   calRange();
   return(INIT_SUCCEEDED);
  
  }
//+------------------------------------------------------------------+
//| Expert deinitialization function                                 |
//+------------------------------------------------------------------+
void OnDeinit(const int reason)
  {
//---
    EventKillTimer();
   
  }
  
void OnTimer()
{
//12点59分以后平仓
  if(Hour()==23 && Minute()>59)
  {
      clearOrders();
  }
}
//+------------------------------------------------------------------+
//| Expert tick function                                             |
//+------------------------------------------------------------------+
void OnTick()
  {
//---
   CheckTime();//检查隔日,如果隔日平仓
   if(enableTrade == 0) //持仓为0 开始交易
   {
      if(Volume[0]>1) return; //新bar时去交易
      if(Close[1] > upper_line)
      {
         int res=OrderSend(Symbol(),OP_BUY,lot,Ask,slip,0,0,"",MAGICMA,0,Red);         
         if(res > 0)
         { 
            enableTrade = 1;
         }
      }
      if(Close[1] < lower_line)
      {
         int res=OrderSend(Symbol(),OP_SELL,lot,Bid,slip,0,0,"",MAGICMA,0,Red);
         if(res > 0)
         {
            enableTrade = -1;
         }
         
      }
   }
   if(enableTrade == -1) //持空单
   {
      if(Volume[0]>1) return;
      if(Close[1] > upper_line)
      {
         clearOrders();
      }
   }
   if(enableTrade == 1) //持多单
   {
      if(Volume[0]>1) return;
      if(Close[1] < lower_line)
      {
         clearOrders();
      }
   }

  }
//+------------------------------------------------------------------+
