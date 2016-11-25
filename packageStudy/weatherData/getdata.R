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
humidity_df <- ldply(cities_to_compare, getHumidity, "2014-05-01")

x = getWeatherForDate('shanghai', '2016-11-16',opt_detailed=TRUE,opt_all_columns=T)

hdf <- getWeatherForDate('shanghai', start_date='2015-01-01', end_date = '2015-12-31',
                         opt_detailed=TRUE,
                         opt_all_columns=T)

hdf =  hdf[,c('DateUTC','TemperatureC','Dew_PointC','Humidity','Sea_Level_PressurehPa','Wind_SpeedKm_h')]
hdf = hdf[order(hdf$DateUTC,decreasing = F),]

hdf_xts = xts(hdf,order.by = as.POSIXct(hdf$DateUTC))
hdf_xts = hdf_xts['2015']
hour_seq = seq.POSIXt(as.POSIXct('2015-01-01'),as.POSIXct('2015-12-31'),by='hour')
hdf_f = hdf_xts[hour_seq,]
hdf_f = as.data.frame(hdf_f)
rownames(hdf_f) = NULL
h = as.numeric(strftime(hdf_f$DateUTC,'%H')) + 1
hdf_f$hour = h
hdf_f$DateUTC = as.character(as.Date(hdf_f$DateUTC))
hdf_f = hdf_f[,c('DateUTC','hour','TemperatureC','Dew_PointC','Humidity','Sea_Level_PressurehPa','Wind_SpeedKm_h')]

#填充数据
days = unique(hdf_f$DateUTC)
hdf_complete = data.frame()
for(day in days)
{
  dayinfo = subset(hdf_f,DateUTC == day)
  if(length(dayinfo$hour) == 24) 
  {
     hdf_complete = rbind(hdf_complete,dayinfo)
     next
  }
  diff = setdiff(1:24,dayinfo$hour)
  for(d in diff)
  {
    w = order((dayinfo$hour - d),decreasing = F)[1]
    w = dayinfo$hour[w]
    r = subset(dayinfo,hour == w)
    r$hour = d
    dayinfo = rbind(dayinfo,r)
  }
  hdf_complete = rbind(hdf_complete,dayinfo)
}
colnames(hdf_complete)[1] = 'Date'
hdf_complete = hdf_complete[order(hdf_complete$Date,hdf_complete$hour),]
write.csv(hdf_complete,'d:/weatherdata.csv',row.names=F)


