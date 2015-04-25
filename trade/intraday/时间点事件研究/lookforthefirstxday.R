
result_dates = list()
date_index = 1

daydata = to.daily(priceData)
daydata$weekday = strftime(index(daydata), "%u")
daydata$weeks = strftime(index(daydata), "%U")
daydata$month = strftime(index(daydata), "%m")

flag_m = as.numeric(daydata[1,]$m)
isfirst = F

for(i in 1 : nrow(daydata))
{
  current_m = as.numeric(daydata[i,]$m)
  
  if(current_m != flag_m)
  {
    isfirst = F
    flag_m = current_m
  }
  
  if(isfirst == T)
  {
    next
  }
  
  current_weekday = as.numeric(daydata[i,]$weekday)
  if(current_weekday == 5)
  {
    isfirst = T
    result_dates[[date_index]] = time(daydata[i,])
    date_index = date_index + 1
  }
}