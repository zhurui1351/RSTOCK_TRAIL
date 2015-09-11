#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/05-Regression/chapter05.R
library('ggplot2')
ages <- read.csv(file.path('c:/data', 'longevity.csv'))

ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
  geom_density() +
  facet_grid(Smokes ~ .)

ages <- read.csv(file.path('c:/data', 'longevity.csv'))
guess <- 73
with(ages, mean((AgeAtDeath - guess) ^ 2))

ages <- read.csv(file.path('c:/data', 'longevity.csv'))

guess.accuracy <- data.frame()

for (guess in seq(63, 83, by = 1))
{
  prediction.error <- with(ages,
                           mean((AgeAtDeath - guess) ^ 2))
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                     Error = prediction.error))
}

ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
  geom_point() +
  geom_line()

ages <- read.csv(file.path('c:/data', 'longevity.csv'))

constant.guess <- with(ages, mean(AgeAtDeath))

with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))

smokers.guess <- with(subset(ages, Smokes == 1),
                      mean(AgeAtDeath))

non.smokers.guess <- with(subset(ages, Smokes == 0),
                          mean(AgeAtDeath))

ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                         non.smokers.guess,
                                         smokers.guess))

with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))


library('ggplot2')

heights.weights <- read.csv(file.path('c:/data',
                                      '01_heights_weights_genders.csv'),
                            header = TRUE,
                            sep = ',')

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method = 'lm')

fitted.regression <- lm(Weight ~ Height,
                        data = heights.weights)

coef(fitted.regression)
intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]

predict(fitted.regression)
true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)
residuals(fitted.regression)
plot(fitted.regression, which = 1)


x <- 1:10
y <- x ^ 2
fitted.regression <- lm(y ~ x)
plot(fitted.regression, which = 1)

x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
sum(squared.errors)


x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
mse


x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse

#r2 当仅使用均值预测 与使用模型预测后 比较效果，如果不比均值好，那么为0 ，如果完美，那么为1
mean.mse <- 1.09209343
model.mse <- 0.954544

r2 <- 1 - (model.mse / mean.mse)
r2


top.1000.sites <- read.csv(file.path('c:/data', 'top_1000_sites.tsv'),
                           sep = '\t',
                           stringsAsFactors = FALSE)

ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) +
  geom_point()

ggplot(top.1000.sites, aes(x = PageViews)) +
  geom_density()

ggplot(top.1000.sites, aes(x = log(PageViews))) +
  geom_density()
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point()

ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)

summary(lm.fit)

lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
             data = top.1000.sites)
summary(lm.fit)


lm.fit <- lm(log(PageViews) ~ HasAdvertising,
             data = top.1000.sites)
summary(lm.fit)$r.squared

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)
summary(lm.fit)$r.squared

lm.fit <- lm(log(PageViews) ~ InEnglish,
             data = top.1000.sites)
summary(lm.fit)$r.squared


x <- 1:10
y <- x ^ 2

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

cor(x, y)
