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