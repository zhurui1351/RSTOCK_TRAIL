#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/07-Optimization/chapter07.R
height.to.weight <- function(height, a, b)
{
  return(a + b * height)
}
heights.weights <- read.csv(file.path('c:/data', '01_heights_weights_genders.csv'))
coef(lm(Weight ~ Height, data = heights.weights))

squared.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2))
}

for (a in seq(-1, 1, by = 1))
{
  for (b in seq(-1, 1, by = 1))
  {
    print(squared.error(heights.weights, a, b))
  }
}

#使用optim优化
optim(c(0, 0),
      function (x)
      {
        squared.error(heights.weights, x[1], x[2])
      })


a.error <- function(a)
{
  return(squared.error(heights.weights, a, 0))
}

curve(sapply(x, function (a) {a.error(a)}), from = -1000, to = 1000)

b.error <- function(b)
{
  return(squared.error(heights.weights, 0, b))
}

curve(sapply(x, function (b) {b.error(b)}), from = -1000, to = 1000)
#岭回归优化
ridge.error <- function(heights.weights, a, b, lambda)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2) + lambda * (a ^ 2 + b ^ 2))
}

lambda <- 1

optim(c(0, 0),
      function (x)
      {
        ridge.error(heights.weights, x[1], x[2], lambda)
      })

a.ridge.error <- function(a, lambda)
{
  return(ridge.error(heights.weights, a, 0, lambda))
}
curve(sapply(x, function (a) {a.ridge.error(a, lambda)}), from = -1000, to = 1000)

b.ridge.error <- function(b, lambda)
{
  return(ridge.error(heights.weights, 0, b, lambda))
}
curve(sapply(x, function (b) {b.ridge.error(b, lambda)}), from = -1000, to = 1000)

absolute.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(abs(errors)))
}

a.absolute.error <- function(a)
{
  return(absolute.error(heights.weights, a, 0))
}

curve(sapply(x, function (a) {a.absolute.error(a)}), from = -1000, to = 1000)

#metropolis 方法
english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                     'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                     'w', 'x', 'y', 'z')
caesar.cipher <- list()

inverse.caesar.cipher <- list()
for (index in 1:length(english.letters))
{
  caesar.cipher[[english.letters[index]]] <- english.letters[index %% 26 + 1]
  inverse.caesar.cipher[[english.letters[index %% 26 + 1]]] <- english.letters[index]
}

print(caesar.cipher)


apply.cipher.to.string <- function(string, cipher)
{
  output <- ''
  
  for (i in 1:nchar(string))
  {
    output <- paste(output, cipher[[substr(string, i, i)]], sep = '')
  }
  
  return(output)
}

apply.cipher.to.text <- function(text, cipher)
{
  output <- c()
  
  for (string in text)
  {
    output <- c(output, apply.cipher.to.string(string, cipher))
  }
  
  return(output)
}

apply.cipher.to.text(c('sample', 'text'), caesar.cipher)

generate.random.cipher <- function()
{
  cipher <- list()
  
  inputs <- english.letters
  
  outputs <- english.letters[sample(1:length(english.letters), length(english.letters))]
  
  for (index in 1:length(english.letters))
  {
    cipher[[inputs[index]]] <- outputs[index]
  }
  
  return(cipher)
}

modify.cipher <- function(cipher, input, output)
{
  new.cipher <- cipher
  
  new.cipher[[input]] <- output
  
  old.output <- cipher[[input]]
  
  collateral.input <- names(which(sapply(names(cipher),
                                         function (key) {cipher[[key]]}) == output))
  
  new.cipher[[collateral.input]] <- old.output
  
  return(new.cipher)
}

propose.modified.cipher <- function(cipher)
{
  input <- sample(names(cipher), 1)
  
  output <- sample(english.letters, 1)
  
  return(modify.cipher(cipher, input, output))
}

# Seventeenth code snippet
load(file.path('data', 'lexical_database.Rdata'))

# Eighteength code snippet
lexical.database[['a']]
lexical.database[['the']]
lexical.database[['he']]
lexical.database[['she']]
lexical.database[['data']]

# Nineteenth code snippet
one.gram.probability <- function(one.gram, lexical.database = list())
{
  lexical.probability <- lexical.database[[one.gram]]
  
  if (is.null(lexical.probability) || is.na(lexical.probability))
  {
    return(.Machine$double.eps)
  }
  else
  {
    return(lexical.probability)
  }
}

# Twentieth code snippet
log.probability.of.text <- function(text, cipher, lexical.database = list())
{
  log.probability <- 0.0
  
  for (string in text)
  {
    decrypted.string <- apply.cipher.to.string(string, cipher)
    log.probability <- log.probability +
      log(one.gram.probability(decrypted.string, lexical.database))
  }
  
  return(log.probability)
}

# Twenty-first code snippet
metropolis.step <- function(text, cipher, lexical.database = list())
{
  proposed.cipher <- propose.modified.cipher(cipher)
  
  lp1 <- log.probability.of.text(text, cipher, lexical.database)
  lp2 <- log.probability.of.text(text, proposed.cipher, lexical.database)
  
  if (lp2 > lp1)
  {
    return(proposed.cipher)
  }
  else
  {
    a <- exp(lp2 - lp1)
    x <- runif(1)
    
    if (x < a)
    {
      return(proposed.cipher)
    }
    else
    {
      return(cipher)
    }
  }
}

# Twenty-second code snippet
decrypted.text <- c('here', 'is', 'some', 'sample', 'text')

# Twenty-third code snippet
encrypted.text <- apply.cipher.to.text(decrypted.text, caesar.cipher)

# Twenty-fourth code snippet
set.seed(1)

cipher <- generate.random.cipher()

results <- data.frame()

number.of.iterations <- 50000

for (iteration in 1:number.of.iterations)
{
  log.probability <- log.probability.of.text(encrypted.text,
                                             cipher,
                                             lexical.database)
  
  current.decrypted.text <- paste(apply.cipher.to.text(encrypted.text,
                                                       cipher),
                                  collapse = ' ')
  
  correct.text <- as.numeric(current.decrypted.text == paste(decrypted.text,
                                                             collapse = ' '))
  
  results <- rbind(results,
                   data.frame(Iteration = iteration,
                              LogProbability = log.probability,
                              CurrentDecryptedText = current.decrypted.text,
                              CorrectText = correct.text))
  
  cipher <- metropolis.step(encrypted.text, cipher, lexical.database)
}

write.table(results,
            file = file.path('data/results.tsv'),
            row.names = FALSE,
            sep = '\t')