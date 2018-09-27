#https://ram-n.github.io/weatherData/example_Humidity.html
#https://ram-n.github.io/weatherData/
require('weatherData')
getHumidity <- function (city_code, date_string) {
  hdf <- getWeatherForDate(city_code, date_string, 
                           opt_detailed=TRUE,
                           opt_custom_columns=T, custom_columns=4)
  #add one column that helps identify the city.
  # This is needed so that city name is retained 
  # when we stack data frames vertically
  hdf$Station <- city_code
  return(hdf)
}

library(plyr)
humidity_df <- ldply(cities_to_compare, getHumidity, "2005-05-01")

x = getWeatherForDate('chengdu', '2005-04-01',opt_detailed=TRUE,opt_all_columns=T)

hdf <- getWeatherForDate('chengdu', start_date='2005-04-23', end_date = '2005-05-26',
                         opt_detailed=TRUE,
                         opt_all_columns=TRUE)

hdf =  hdf[,c('DateUTC','TemperatureC','Dew_PointC','Humidity','Sea_Level_PressurehPa','Wind_SpeedKm_h')]
hdf = hdf[order(hdf$DateUTC,decreasing = F),]

hdf_xts = xts(hdf,order.by = as.POSIXct(hdf$DateUTC))
hdf_xts = hdf_xts['2017-04-23/2017-05-26']

hour_seq = seq.POSIXt(as.POSIXct('2017-01-01'),as.POSIXct('2017-12-31'),by='hour')
hdf_f = hdf_xts[hour_seq,]
hdf_f = as.data.frame(hdf_f)
#rownames(hdf_f) = NULL
#h = as.numeric(strftime(hdf_f$DateUTC,'%H')) + 1
#hdf_f$hour = h
#hdf_f$DateUTC = as.character(as.Date(hdf_f$DateUTC))
#hdf_f = hdf_f[,c('DateUTC','hour','TemperatureC','Dew_PointC','Humidity','Sea_Level_PressurehPa','Wind_SpeedKm_h')]

#填充数据
days = unique(substring(hdf_f$DateUTC,1,10))
hdf_complete = data.frame()
for(day in days)
{
  
  dayinfo = subset(hdf_f,substring(hdf_f$DateUTC,1,10) == day)
  if(nrow(dayinfo) == 24) 
  {
     hdf_complete = rbind(hdf_complete,dayinfo)
     next
  }
  
  dayinfo_complete = data.frame()
  
  for(i in 1:nrow(dayinfo))
  {
    d = dayinfo[i,]  
    
    dateutc =as.character(d$DateUTC)
    dateutc1 = addhour(dateutc,1)
    dateutc2 = addhour(dateutc,2)
    
    d1 = d
    d1$DateUTC = dateutc1
    
    d2 = d
    d2$DateUTC = dateutc2
    
    dayinfo_complete = rbind(dayinfo_complete,d)
    dayinfo_complete = rbind(dayinfo_complete,d1)
    dayinfo_complete = rbind(dayinfo_complete,d2)
    
  }
  
  hdf_complete = rbind(hdf_complete,dayinfo_complete)
}
colnames(hdf_complete)[1] = 'Date'
rownames(hdf_complete) = NULL
#hdf_complete = hdf_complete[order(hdf_complete$Date),]
write.csv(hdf_complete,'d:/weatherdata.csv',row.names=F)


addhour = function(dateutc,n=1)
{
  hour = substring(dateutc,12,13)
  hour = as.numeric(hour)+n
  hour = ifelse(hour<10,paste('0',as.character(hour),sep=''),as.character(hour))
  substring(dateutc,12,13) = hour
  return(dateutc)
}
