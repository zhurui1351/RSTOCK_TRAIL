filterDaoChuizi = function(daydate,mg)
{
  allcodes = names(mg)
  l = lapply(allcodes,function(p,date){
   # print(p)
    n = mg[[p]]
    current = n[date]
    if(nrow(current) == 1 &&!is.na(current$stage30) && !is.na(current$stage5) && current$stage30 ==2 && current$stage5 ==4)
    {
       cuizi = (Lo(current) - Cl(current)) / abs(Cl(current)-Op(current))
       shangying = (current[,'High'] - Cl(current)) / Cl(current)
       if(!is.nan(cuizi) && !is.nan(shangying) && cuizi< -2 && shangying < 0.01)
       {
         return(p)
       }
       else
      {
         return(NULL)
       }
    }
    else
    {
      return(NULL)
    }
    
  }
  ,daydate)
  
}


# for(p in allcodes)
# {
#   n = mg[[p]]
#   current = n[daydate]
#   if(nrow(current) == 1 && current$stage30 ==2 && current$stage5 ==4)
#   {
#     cuizi = (Lo(current) - Cl(current)) / abs(Cl(current)-Op(current))
#     if(!is.nan(cuizi) && cuizi< -2)
#     {
#       print(1)
#     }
#     else
#     {
#       print(0)    }
#   }
#   else
#   {
#     print(0)    
#   }
# }