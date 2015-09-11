#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/10-Recommendations/chapter10.R

df <- read.csv(file.path('c:/data', 'example_data.csv'))
head(df)
distance.matrix <- function(df)
{
  distance <- matrix(rep(NA, nrow(df) ^ 2), nrow = nrow(df))
  
  for (i in 1:nrow(df))
  {
    for (j in 1:nrow(df))
    {
      distance[i, j] <- sqrt((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2)
    }
  }
  
  return(distance)
}

k.nearest.neighbors <- function(i, distance, k = 5)
{
  return(order(distance[i, ])[2:(k + 1)])
}


knn <- function(df, k = 5)
{
  distance <- distance.matrix(df)
  
  predictions <- rep(NA, nrow(df))
  
  for (i in 1:nrow(df))
  {
    indices <- k.nearest.neighbors(i, distance, k = k)
    predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5, 1, 0)
  }
  
  return(predictions)
}


df <- transform(df, kNNPredictions = knn(df))

sum(with(df, Label != kNNPredictions))

rm('knn')
library('class')
df <- read.csv(file.path('c:/data', 'example_data.csv'))
n <- nrow(df)

set.seed(1)

indices <- sort(sample(1:n, n * (1 / 2)))

training.x <- df[indices, 1:2]
test.x <- df[-indices, 1:2]

training.y <- df[indices, 3]
test.y <- df[-indices, 3]

# There's a bug here!
predicted.y <- knn(training.x, test.x, training.y, k = 5)
sum(predicted.y != test.y)
length(test.y)
logit.model <- glm(Label ~ X + Y, data = df[indices, ])

predictions <- as.numeric(predict(logit.model, newdata = df[-indices, ]) > 0)
sum(predictions != test.y)
installations <- read.csv(file.path('c:/data', 'installations.csv'))

head(installations)
library('reshape')
user.package.matrix <- cast(installations, User ~ Package, value = 'Installed')
user.package.matrix[, 1]
user.package.matrix[, 2]
row.names(user.package.matrix) <- user.package.matrix[, 1]
user.package.matrix <- user.package.matrix[, -1]
similarities <- cor(user.package.matrix)
nrow(similarities)
ncol(similarities)
similarities[1, 1]
similarities[1, 2]
distances <- -log((similarities / 2) + 0.5)
k.nearest.neighbors <- function(i, distances, k = 25)
{
  return(order(distances[i, ])[2:(k + 1)])
}

installation.probability <- function(user, package, user.package.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function (neighbor) {user.package.matrix[user, neighbor]})))
}

installation.probability(1, 1, user.package.matrix, distances)



most.probable.packages <- function(user, user.package.matrix, distances, k = 25)
{
  return(order(sapply(1:ncol(user.package.matrix),
                      function (package)
                      {
                        installation.probability(user,
                                                 package,
                                                 user.package.matrix,
                                                 distances,
                                                 k = k)
                      }),
               decreasing = TRUE))
}

user <- 1

listing <- most.probable.packages(user, user.package.matrix, distances)

colnames(user.package.matrix)[listing[1:10]]