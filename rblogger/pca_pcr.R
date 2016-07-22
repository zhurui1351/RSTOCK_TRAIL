#http://www.r-bloggers.com/performing-principal-components-regression-pcr-in-r/
require(pls)
set.seed (1000)
pcr_model <- pcr(Sepal.Length~., data = iris, scale = TRUE, validation = "CV")
summary(pcr_model)

validationplot(pcr_model)
validationplot(pcr_model, val.type="MSEP")
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
coefplot(pcr_model)
train <- iris[1:120,]
y_test <- iris[120:150, 1]
test <- iris[120:150, 2:5]

pcr_model <- pcr(Sepal.Length~., data = train,scale =TRUE, validation = "CV")

pcr_pred <- predict(pcr_model, test, ncomp = 3)
mean((pcr_pred - y_test)^2)