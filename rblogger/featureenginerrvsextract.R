#http://appliedpredictivemodeling.com/blog/2015/7/28/feature-engineering-versus-feature-extraction
library(caret)
data("cars")
head(cars)

example_index = sample(nrow(cars),nrow(cars)/2)
example_cars = cars[example_index,]
test_cars = cars[-example_index,]
#做PCA主成成分分析
pca_pp <- preProcess(example_cars[, 1:2], method = c("center", "scale", "pca"))
#第一主成成分可以解释大部分变异，但作为分类而言 似乎第二主成成分更好
train_pc <- predict(pca_pp, example_cars[, 1:2])
test_pc <- predict(pca_pp, test_cars[, 1:2])
