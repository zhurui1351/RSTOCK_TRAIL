#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/08-PCA/chapter08.R
library('ggplot2')
prices <- read.csv(file.path('c:/data', 'stock_prices.csv'),
                   stringsAsFactors = FALSE)
prices[1, ]
library('lubridate')

prices <- transform(prices, Date = ymd(Date))
library('reshape')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

#相关矩阵

cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlation = correlations),
       aes(x = Correlation, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])
principal.component <- pca$loadings[, c(1)]
loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings),
       aes(x = Loading, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

market.index <- predict(pca)[, c(1)]
#与道指比较
dji.prices <- read.csv(file.path('c:/data', 'DJI.csv'),
                      stringsAsFactors = FALSE)
dji.prices <- transform(dji.prices, Date = ymd(Date))

# Twelfth code snippet
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))


comparison <- data.frame(Date = dates,
                         MarketIndex = market.index,
                         DJI = dji)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')
ggplot(alt.comparison,
       aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, DJI = scale(DJI))

alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()





student<- data.frame(
  x1=c(148,139,160,149,159,142,153,150,151),
  x2=c(41 ,34 , 49 ,36 ,45 ,31 ,43 ,43, 42),
  x3=c(72 ,71 , 77 ,67 ,80 ,66 ,76 ,77,77),
  x4=c(78 ,76 , 86 ,79 ,86 ,76 ,83 ,79 ,80)
)
student.pr <- princomp(student,cor=TRUE)
summary(student.pr,loadings=TRUE)
screeplot(student.pr,type="lines")
