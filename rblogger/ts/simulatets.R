#http://ellisp.github.io/blog/2015/11/15/linear-model-timeseries/
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(showtext)
library(latex2exp)
library(extrafont)
#-------------set up-------------
# Fonts and themes:
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

# sample and population size:
n <- 200
popn <- n * 10


#----------simulate data---------
set.seed(123)

# Linear model with a time series random element, n * 10 in length:
df1 <- data.frame(x = rnorm(popn)) %>%
  mutate(y = 1 + 0.3 * x + scale(arima.sim(list(ar = 0.99), popn)),
         ind = 1:popn,
         type = "TimeSeries")
# cut back to just the first n points:
df1 <- df1[1:n, ]


# Same linear model, with i.i.d. white noise random element:
df2 <- data.frame(x = rnorm(n)) %>%
  mutate(y = 1 + 0.3 * x + rnorm(n),
         ind = 1:n,
         type = "CrossSection")

# draw the time series response:
p0 <- df1 %>%
  ggplot(aes(x = ind, y = y)) +
  geom_line() +
  labs(x = "Time") +
  ggtitle("Simulated response variable from linear model\nwith time series random element")



df_both <- rbind(df1, df2)


for(i in 5:n){
  
  # I name the images i + 1000 so alphabetical order is also numeric
  png(paste0(i + 1000, ".png"), 700, 600, res = 100)
  
  df1_tmp <- df1[1:i, ]
  df2_tmp <- df2[1:i, ]
  
  residuals1 <- data.frame(res = residuals(lm(y ~ x, data = df1_tmp)), 
                           ind = 1:i, 
                           type = "TimeSeries")
  residuals2 <- data.frame(res = residuals(lm(y ~ x, data = df2_tmp)), 
                           ind = 1:i, 
                           type = "CrossSection")
  
  # connected scatter plots:
  p1 <- ggplot(df_both[c(1:i, (n + 1) : (n + i)), ], aes(x, y, colour = ind)) +
    facet_wrap(~type, ncol = 2) +
    geom_path() +
    geom_point() +
    geom_abline(intercept = 1, slope = 0.3) +
    geom_smooth(method = "lm", se = FALSE, size = 2, colour = "red") +
    theme(legend.position = "none") +
    xlim(range(df_both$x)) +
    ylim(range(df_both$y)) +
    ggtitle(paste("Connected scatterplot showing regression on first", i, "points"))
  
  
  # Residuals plots  
  p2 <- residuals1 %>%
    rbind(residuals2) %>%
    mutate(type = factor(type, levels = c("CrossSection", "TimeSeries"))) %>%
    ggplot(aes(x = ind, y = res)) +
    scale_x_continuous(limits = c(0, n)) +
    facet_wrap(~type) +
    geom_line() +
    geom_point() +
    ggtitle("Residuals from regression so far") +
    labs(x = "Time", y = "Residuals")
  
  grid.arrange(p1, p2)
  
  dev.off()
  
}
# combine them into an animated GIF using  ImageMagick
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 10 *.png "timeseries.gif"')



set.seed(123)
n <- 1000

# white noise:
wn <- ts(rnorm(n))

# initialise the first two values:
ar1 <- ma1 <- arma11 <- arma22 <- wn[1:2]

# loop through and create the 3:1000th values:
for(i in 3:n){
  ar1[i]      <- ar1[i - 1] * 0.8 + wn[i]
  ma1[i]      <- wn[i - 1]  * 0.8 + wn[i]
  arma11[i]   <- arma11[i - 1] * 0.8 + wn[i - 1] * 0.8 + wn[i] 
  arma22[i]   <- arma22[i - 1] * 0.8 + arma22[i - 2]  * (-0.3) + 0.8 * wn[i-1] - 0.3 * wn[i-2] + wn[i]
}

# turn them into time series, and for the last two, "integrate" them via cumulative sum
ar1 <- ts(ar1)
ma1 <- ts(ma1)
arma11 <- ts(arma11)
arima111 <- ts(cumsum(arma11))
arima222 <- ts(cumsum(cumsum(arma22)))

for(i in 3:n){
  png(paste0(i + 1000, ".png"), 800, 800, res = 100)
  par(mfrow = c(3, 2), cex.main = 1.5, cex = 0.8, family = "Calibri")
  plot(wn[1:i], main = latex2exp("$\\epsilon ~ N(0, \\sigma)"), 
       bty = "l", type = "l", ylab = "x = white noise", xlab = "")
  
  plot(ar1[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + \\epsilon_t$"), 
       bty = "l", type = "l", ylab = "x =AR (1)", xlab = "")
  
  plot(ma1[1:i], main = latex2exp("$x_t = 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
       bty = "l", type = "l", ylab = "x = MA(1)", xlab = "")
  
  plot(arma11[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + 0.8\\epsilon_{t-1} + \\epsilon_t$"),
       bty = "l", type = "l", ylab = "x = ARMA(1, 1)", xlab = "")
  
  plot(arima111[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
       bty = "l", type = "l", ylab = "y = ARIMA(1, 1, 1)", xlab = "")
  mtext(latex2exp("$y_t = x_t + x_{t-1} + ... + x_0$"), cex = 1.3, line = -0.5)
  
  plot(arima222[1:i], main =  latex2exp(
    "$x_t = 0.8x_{t-1} - 0.3x_{t-2} - 0.3\\epsilon_{t-2} + 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
    bty = "l", type = "l", ylab = "z = ARIMA(2, 2, 2)", xlab = "")
  mtext(latex2exp("$y_t = x_t + x_{t-1} + ... + x_0$"), cex = 1.3, line = -0.5)
  mtext(latex2exp("$z_t = y_t + y_{t-1} + ... + y_0$"), cex = 1.3, line = -2.0)
  dev.off()
}
