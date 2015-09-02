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
