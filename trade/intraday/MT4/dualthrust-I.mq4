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
input double upperratio;
input double lowerratio;
input bool isAtr;
input bool isInnerday;
extern string logname;
extern double trailingStop = 50;
extern int takeProfit = 20;

double upper_line;
double lower_line;
int day = Day();
double lot = 0.01;
int slip = 2;
int enableTrade = 0;
bool New_Bar = False;
int myopenedticket = 0;
void Fun_New_Bar()                              
  {                                             
   static datetime New_Time=0;                  
   New_Bar=false;                               
   if(New_Time!=Time[0])                        
     {
      New_Time=Time[0];                         
      New_Bar=True;                                
     }
  }
  
  
void writeTradingLog(string symbol,double lots,datetime opentime ,double openprice,datetime closetime,double closeprince,double profit)
{
   string ordertype = "";
   if(enableTrade == -1)
   {
      ordertype = "sell";
   }
    if(enableTrade == 1)
   {
      ordertype = "buy";
   }
   int file_handle=FileOpen(logname,FILE_READ|FILE_WRITE|FILE_CSV);
   if(file_handle!=INVALID_HANDLE)
     {
       FileSeek(file_handle,0,SEEK_END);
       FileWrite(file_handle,ordertype,symbol,lots,opentime,openprice,closetime,closeprince,profit);
       FileClose(file_handle);
     }
     else
     {
      Print("file open erro:",file_handle);
     }
}
void writeInfoLog(string info)
{
   int file_handle=FileOpen(logname,FILE_READ|FILE_WRITE|FILE_CSV);
   if(file_handle!=INVALID_HANDLE)
     {
       FileSeek(file_handle,0,SEEK_END);
       FileWrite(file_handle,info);
       FileClose(file_handle);
     }
     else
     {
      Print("file open erro:",file_handle);
     }
}

void calRange()
{
   double range;
   if(isAtr)
   {
      range = iATR(NULL,PERIOD_D1,3,1);  
   }
   else
   {
      int val_index_hh = iHighest(NULL,PERIOD_D1,MODE_HIGH,3,1);
      double val_hh = iHigh(NULL,PERIOD_D1,val_index_hh);  
      int val_index_hc = iHighest(NULL,PERIOD_D1,MODE_CLOSE,3,1);
      double val_hc =iClose(NULL,PERIOD_D1,val_index_hc); 
      int val_index_lc = iLowest(NULL,PERIOD_D1,MODE_CLOSE,3,1);
      double val_lc = iClose(NULL,PERIOD_D1,val_index_lc);
      int val_index_ll = iLowest(NULL,PERIOD_D1,MODE_LOW,3,1);
      double val_ll = iLow(NULL,PERIOD_D1,val_index_ll); 
      range =MathMax(val_hh-val_lc,val_hc-val_ll);       
   }
   upper_line = iOpen(NULL,PERIOD_D1,0) + upperratio * range;
   lower_line = iOpen(NULL,PERIOD_D1,0) - lowerratio * range;
   string info = TimeToStr(Time[0]) + "  " + "upperline:" + DoubleToString(upper_line) +"  lowerline:" +DoubleToString(lower_line);
   writeInfoLog(info);
   Print("upper_line:",upper_line);
   Print("lower_line:",lower_line);
}
//平仓订单
void clearOrders()
{
   if(myopenedticket == 0 ) return;
     bool res = OrderSelect(myopenedticket,SELECT_BY_TICKET);
     if(res)
     {
         if(OrderType() == OP_BUY){
   		   while(!OrderClose(OrderTicket(),OrderLots(),Bid,slip));
   		   
   		}
   	  if(OrderType() == OP_SELL){
   		    while(!OrderClose(OrderTicket(),OrderLots(),Ask,slip));
   		  
   		}
   		writeTradingLog(OrderSymbol(),OrderLots(),OrderOpenTime(),OrderOpenPrice(),OrderCloseTime(),OrderClosePrice(),OrderProfit());
     }
     else
     {
        Alert("we dont select this order:",myopenedticket);
     }  
   myopenedticket = 0 ;
   calRange();
   enableTrade = 0;
}
void CheckTime(){  // 是否换日
   if(day < Day() && enableTrade ==0){ // 隔日空仓重新计算range
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
//11点59分以后如果日内交易则平仓
  if(Hour()==23 && Minute()>59 && isInnerday)
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
   CheckTime();//检查隔日,如果隔日重算参数
   Fun_New_Bar();
   if(New_Bar == False) return; //新bar时去交易
   if(enableTrade == 0) //持仓为0 开始交易
   {
      if(Close[1] > upper_line)
      {
         int res= 0;
         while(res <= 0)
         {
           res = OrderSend(Symbol(),OP_BUY,lot,Ask,slip,0,0,"",MAGICMA,0,Red); 
         }       
         myopenedticket = res;
         enableTrade = 1;
      }
      if(Close[1] < lower_line)
      {
         int res= 0;
         while(res<=0)
         {
            res = OrderSend(Symbol(),OP_SELL,lot,Bid,slip,0,0,"",MAGICMA,0,Red);
         }
         myopenedticket = res;
         enableTrade = -1;
      }
   }
   
   if(enableTrade == -1) //持空单
   {
     bool res = OrderSelect(myopenedticket,SELECT_BY_TICKET);
      if(Close[1] > upper_line)
      {
         clearOrders();
      }
      else if(res)
      {
         if(OrderOpenPrice() - Close[1] > Point * takeProfit)
         {
            upper_line = upper_line - Point * trailingStop;
            string info = TimeToStr(Time[0]) + "  " + "change upperline to:" +  DoubleToString(upper_line) ;
            writeInfoLog(info);
         }
      }
   }
   if(enableTrade == 1) //持多单
   {
      bool res = OrderSelect(myopenedticket,SELECT_BY_TICKET);
      if(Close[1] < lower_line)
      {
         clearOrders();
      }
      else if(res)
      {
         if( Close[1] - OrderOpenPrice()  > Point * takeProfit)
         {
            lower_line = lower_line + Point * trailingStop;
            string info = TimeToStr(Time[0]) + "  " + "change lowerline to:" + DoubleToString(lower_line) ;
            writeInfoLog(info);
         }
      }
   }

  }
//+------------------------------------------------------------------+
