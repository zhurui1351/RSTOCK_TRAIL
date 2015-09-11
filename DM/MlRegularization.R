#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/06-Regularization/chapter06.R
library('ggplot2')
set.seed(1)
x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)
#GAM模型拟合
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + 
  geom_point() +
  geom_smooth(se = FALSE)

x.squared <- x ^ 2

ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

summary(lm(y ~ x))$r.squared
summary(lm(y ~ x.squared))$r.squared
#多项式回归
set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

ggplot(df, aes(x = X, y = Y)) +
  geom_point()

summary(lm(Y ~ X, data = df))
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)

summary(lm(Y ~ X + X2 + X3, data = df))
#奇异点问题
df <- transform(df, X4 = X ^ 4)
df <- transform(df, X5 = X ^ 5)
df <- transform(df, X6 = X ^ 6)
df <- transform(df, X7 = X ^ 7)
df <- transform(df, X8 = X ^ 8)
df <- transform(df, X9 = X ^ 9)
df <- transform(df, X10 = X ^ 10)
df <- transform(df, X11 = X ^ 11)
df <- transform(df, X12 = X ^ 12)
df <- transform(df, X13 = X ^ 13)
df <- transform(df, X14 = X ^ 14)
df <- transform(df, X15 = X ^ 15)

summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14,
           data = df))
#正交多项式

summary(lm(Y ~ poly(X, degree = 14), data = df))


poly.fit <- lm(Y ~ poly(X, degree = 1), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 3), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 5), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 25), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()
#交叉验证
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# Thirteenth code snippet
n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}
performance <- data.frame()

for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
  
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))
  
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                                              newdata = test.df))))
}
#可以看到测试误差变大的转折点
ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

#正则化 避免过度拟合

lm.fit <- lm(y ~ x)
model.complexity <- sum(coef(lm.fit) ^ 2)

lm.fit <- lm(y ~ x)
l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
x <- as.matrix(cbind(x,rev(x)))

library('glmnet')

glmnet(x, y)


set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}


library('glmnet')

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))

lambdas <- glmnet.fit$lambda

performance <- data.frame()

for (lambda in lambdas)
{
  performance <- rbind(performance,
                       data.frame(Lambda = lambda,
                                  RMSE = rmse(test.y,
                                              with(test.df,
                                                   predict(glmnet.fit,
                                                           poly(X, degree = 10),
                                                           s = lambda)))))
}


ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()

ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  scale_x_log10()

best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])
glmnet.fit <- with(df, glmnet(poly(X, degree = 10), Y))
coef(glmnet.fit, s = best.lambda)


ranks <- read.csv(file.path('c:/data', 'oreilly.csv'),
                  stringsAsFactors = FALSE)

library('tm')

documents <- data.frame(Text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)
x <- as.matrix(dtm)
y <- rev(1:100)

# Twenty-eighth code snippet
set.seed(1)

library('glmnet')

# Twenty-ninth code snippet
performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5))
{
  for (i in 1:50)
  {
    indices <- sample(1:100, 80)
    
    training.x <- x[indices, ]
    training.y <- y[indices]
    
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    
    glm.fit <- glmnet(training.x, training.y)
    
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))
    
    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    RMSE = rmse))
  }
}

# Thirtieth code snippet
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

# Thirty-first code snippet
y <- rep(c(1, 0), each = 50)

# Thirty-second code snippet
regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-third code snippet
regularized.fit <- glmnet(x, y)

regularized.fit <- glmnet(x, y, family = 'gaussian')

regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-fourth code snippet
predict(regularized.fit, newx = x, s = 0.001)

ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)

library('boot')

inv.logit(predict(regularized.fit, newx = x, s = 0.001))

set.seed(1)

performance <- data.frame()

for (i in 1:250)
{
  indices <- sample(1:100, 80)
  
  training.x <- x[indices, ]
  training.y <- y[indices]
  
  test.x <- x[-indices, ]
  test.y <- y[-indices]
  
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    
    error.rate <- mean(predicted.y != test.y)
    
    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}

# Thirty-eighth code snippet
ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
  scale_x_log10()