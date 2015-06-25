#http://rstudio.github.io/dygraphs/
require(dygraphs)
#图上显示每个数据点的热点
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector()


lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

#时间区域标注

dygraph(nhtemp, main="New Haven Temperatures") %>% 
  dySeries(label="Temp (F)", color="black") %>%
  dyShading(from="1920-1-1", to="1930-1-1", color="#FFE6E6") %>%
  dyShading(from="1940-1-1", to="1950-1-1", color="#CCEBD6")

#demo
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

dygraph(lungDeaths) %>% dyRangeSelector()

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)


hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
#两个y轴
temperature <- ts(frequency = 12, start = c(1980, 1),
                  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 
                           25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
rainfall <- ts(frequency = 12, start = c(1980, 1),
               data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 
                        135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
weather <- cbind(rainfall, temperature)

# assign the "rainfall" series to the y2 axis
dygraph(weather) %>%
  dySeries("rainfall", axis = 'y2')

#legend
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dySeries("V1", label = "Temperature (F)") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

#timezone
datetimes <- seq.POSIXt(as.POSIXct("2015-01-01", tz="GMT"),
                        as.POSIXct("2015-01-02", tz="GMT"), by="3 hours")
values <- rnorm(length(datetimes))
series <- xts(values, order.by = datetimes, tz="GMT")
#时间不一致，时区使用的本地时区
dygraph(series)
dygraph(series) %>% 
  dyOptions(useDataTimezone = TRUE)


dygraph(ldeaths, main = "All", group = "lung-deaths")
dygraph(mdeaths, main = "Male", group = "lung-deaths")
dygraph(fdeaths, main = "Female", group = "lung-deaths")

#roll
dygraph(discoveries, main = "Important Discoveries") %>% 
  dyRoller(rollPeriod = 5)


#Event Lines
dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyEvent(date = "1950-6-30", "Korea", labelLoc = "bottom") %>%
  dyEvent(date = "1965-2-09", "Vietnam", labelLoc = "bottom")

#limit lines
getSymbols("MSFT", from = "2014-06-01", auto.assign=TRUE)
dygraph(MSFT[, 4], main = "Microsoft Share Price") %>% 
  dySeries("MSFT.Close", label = "MSFT") %>%
  dyLimit(as.numeric(MSFT[1, 4]), color = "red")

#注释
dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyAnnotation("1950-7-1", text = "A", tooltip = "Korea") %>%
  dyAnnotation("1965-1-1", text = "B", tooltip = "Vietnam")
presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = TRUE, width = 60)
}

dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  presAnnotation("1950-7-1", text = "Korea") %>%
  presAnnotation("1965-1-1", text = "Vietnam")


getSymbols(c("MSFT", "HPQ"), from = "2014-06-01", auto.assign=TRUE)
stocks <- cbind(MSFT[,2:4], HPQ[,2:4])
dygraph(stocks, main = "Microsoft and HP Share Prices") %>% 
  dySeries(c("MSFT.Low", "MSFT.Close", "MSFT.High"), label = "MSFT") %>%
  dySeries(c("HPQ.Low", "HPQ.Close", "HPQ.High"), label = "HPQ")