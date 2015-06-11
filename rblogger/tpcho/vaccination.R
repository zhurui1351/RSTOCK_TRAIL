#https://biomickwatson.wordpress.com/2015/04/09/recreating-a-famous-visualisation/
#http://feedproxy.google.com/~r/RBloggers/~3/yZDxJe1e1ng/?utm_source=feedburner&utm_medium=email
library(plyr)
library(reshape2)
library(ggplot2)
library(gplots)
require(grid)

csv = "C:/R/code/RSTOCK_TRAIL/rblogger/tpcho/MEASLES_Cases_1909-2001_20150610225007.csv"

readDiseaseData <- function(csv) {
  dis <- read.csv(csv, skip = 2, header = TRUE, stringsAsFactors = FALSE)
  dis[dis=="-"] <- 0
  dis[, 3:62] <- sapply(dis[, 3:62], as.numeric) 
  dis <- dis[dis$YEAR>=1930,]
  dis.m   <- melt(dis[, c(1, 3:61)], variable.name = "state", id.vars = "YEAR")
  dis.agg <- aggregate(value ~ state + YEAR, dis.m, sum)
  return(dis.agg)
}

polio <- readDiseaseData(csv)
head(polio)
#heatmap.2(as.matrix(dis[,2:60]), trace="none", key=FALSE)
#获取人口数据 ，注意格式可能经常变换

getPopData <- function(years = "0009", skip1 = 23, skip2 = 81, rows = 49, names = 1900:1909, keep = 1:11) {
  u  <- paste("http://www.census.gov/popest/data/state/asrh/1980s/tables/st", years, "ts.txt", sep = "")
  p1 <- read.table(u, skip = skip1, nrows = rows, header = F, stringsAsFactors = FALSE)
  p2 <- read.table(u, skip = skip2, nrows = rows, header = F, stringsAsFactors = FALSE)
  p12 <- join(p1, p2, by = "V1")
  p12 <- p12[, keep]
  colnames(p12) <- c("state", names)
  # 1900-1970 are in thousands with commas
  if(as.numeric(substring(years, 1, 1)) < 7) {
    p12[, 2:11] <- sapply(p12[, 2:11], function(x) gsub(",", "", x))
    p12[, 2:11] <- sapply(p12[, 2:11], as.numeric)
    p12[, 2:11] <- sapply(p12[, 2:11], function(x) 1000*x)
  }
  return(p12)
}

popn <- list(p1900 = getPopData(),
             p1910 = getPopData(years = "1019", names = 1910:1919),
             p1920 = getPopData(years = "2029", names = 1920:1929),
             p1930 = getPopData(years = "3039", names = 1930:1939),
             p1940 = getPopData(years = "4049", skip1 = 21, skip2 = 79, , names = 1940:1949),
             p1950 = getPopData(years = "5060", skip1 = 27, skip2 = 92, rows = 51, names = 1950:1959, keep = c(1, 3:7, 9:13)),
             p1960 = getPopData(years = "6070", skip1 = 24, skip2 = 86, rows = 51, names = 1960:1969, keep = c(1, 3:7, 9:13)),
             p1970 = getPopData(years = "7080", skip1 = 14, skip2 = 67, rows = 51, names = 1970:1979, keep = c(2:8, 11:14)),
             p1980 = getPopData(years = "8090", skip1 = 11, skip2 = 70, rows = 51, names = 1980:1990, keep = 1:12))

popn.df <- join_all(popn, by = "state", type = "full")

#合并人口和疾病表，处理州名不一致的情况
statenames <- toupper(state.name)
statenames <- gsub(" ", ".", statenames)
states <- data.frame(sname = statenames, sabb = state.abb)
m <- match(polio$state, states$sname)
polio$abb <- states[m, "sabb"]

popn.m <- melt(popn.df)
colnames(popn.m) <- c("abb", "YEAR", "pop")
popn.m$YEAR <- as.numeric(as.character(popn.m$YEAR))
polio.pop <- join(polio, popn.m, by = c("YEAR", "abb"))
polio.pop$cases <- (100000 / polio.pop$pop) * polio.pop$value

head(polio.pop)

#绘图

ggplot(na.omit(polio.pop)) + geom_dotplot(aes(x = factor(YEAR), fill = cases), color = "white", binwidth = 1, 
                                          dotsize = 0.4, binpositions = "all", method = "histodot") + facet_grid(abb~.) + theme_bw() + 
  scale_fill_continuous(low = "floralwhite", high = "red") + geom_vline(xintercept = 32) + 
  scale_y_discrete(breaks = NULL) + theme(panel.border = element_blank(), strip.text.y = element_text(angle = 0)) + 
  scale_x_discrete(breaks = seq(min(polio.pop$YEAR), max(polio.pop$YEAR), 5)) + 
  labs(x = "Year", y = "cases / 100000", title = "Poliomyelitis 1921 - 1971")



ggplot(na.omit(polio.pop)) + geom_rect(aes(xmin = YEAR, xmax = YEAR+1, ymin = 0, ymax = 12, fill = cases)) + 
  facet_grid(abb~.) + theme_bw() + scale_y_discrete(breaks = NULL) + scale_fill_continuous(low = "floralwhite", high = "red") + 
  theme(panel.border = element_blank(), panel.margin = unit(1, "mm"), strip.text.y = element_text(angle = 0)) + 
  geom_vline(xintercept = 1955) + scale_x_continuous(breaks = seq(min(polio.pop$YEAR), max(polio.pop$YEAR), 5)) + 
  labs(x = "Year", y = "cases / 100000", title = "Poliomyelitis 1921 - 1971")

cols <- c(colorRampPalette(c("white", "cornflowerblue"))(10), colorRampPalette(c("yellow", "red"))(30))

ggplot(na.omit(polio.pop)) + geom_rect(aes(xmin = YEAR, xmax = YEAR+1, ymin = 0, ymax = 12, fill = cases), color = "white") + 
  facet_grid(abb~.) + theme_bw() +scale_y_discrete(breaks = NULL) + scale_fill_gradientn(colours = cols) + 
  theme(panel.border = element_blank(), panel.margin = unit(1, "mm"), strip.text.y = element_text(angle = 0)) + 
  geom_vline(xintercept = 1955) + scale_x_continuous(breaks = seq(min(polio.pop$YEAR), max(polio.pop$YEAR), 5)) + 
  labs(x = "Year", y = "cases / 100000", title = "Poliomyelitis 1921 - 1971")