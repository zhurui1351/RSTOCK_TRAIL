#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/01-Introduction/ufo_sightings.R
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(plyr)       # For data manipulation
library(scales)     # We'll need to fix date formats in plots
ufo <- read.delim(file.path("c:/data", "ufo", "ufo_awesome.tsv"),
                  sep = "\t",
                  stringsAsFactors = FALSE,
                  header = FALSE, 
                  na.strings = "")

summary(ufo)
head(ufo)
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 |
                      nchar(ufo$DateReported) != 8,
                    FALSE,
                    TRUE)
length(which(!good.rows))      
ufo <- ufo[good.rows, ]    
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")


get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)
location.matrix <- do.call(rbind, city.state)

ufo <- transform(ufo,
                 USCity = location.matrix[, 1],
                 USState = location.matrix[, 2],
                 stringsAsFactors = FALSE)

ufo$USState <- state.abb[match(ufo$USState, state.abb)]
ufo.us <- subset(ufo, !is.na(USState))

summary(ufo.us)
head(ufo.us)

quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")

ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))

new.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(breaks = "50 years")

ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format = "%Y-%m")
sightings.counts <- ddply(ufo.us, .(USState,YearMonth), nrow)

date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by = "month")
date.strings <- strftime(date.range, "%Y-%m")

states.dates <- lapply(state.abb, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates),
                           stringsAsFactors = FALSE)

all.sightings <- merge(states.dates,
                       sightings.counts,
                       by.x = c("s", "date.strings"),
                       by.y = c("USState", "YearMonth"),
                       all = TRUE)
names(all.sightings) <- c("State", "YearMonth", "Sightings")

all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(state.abb)))
all.sightings$State <- as.factor(all.sightings$State)


state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")


state.pop <- read.csv(file.path('c:/data/census.csv'), stringsAsFactors=FALSE)
state.pop$abbs <- sapply(state.pop$State, function(x) state.abb[grep(paste('^', x, sep=''), state.name)])
all.sightings$Sightings.Norm <- sapply(1:nrow(all.sightings), 
                                       function(i) all.sightings$Sightings[i] / state.pop$X2000[which(state.pop$abbs== all.sightings$State[i])])


state.plot.norm <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings.Norm)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/02-Exploration/chapter02.R
data.file <- file.path('c:/data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')
heights <- with(heights.weights, Height)
summary(heights)
quantile(heights)
quantile(heights, probs = seq(0, 1, by = 0.20))

ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 1)

ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 5)

ggplot(heights.weights, aes(x = Height)) +
  geom_histogram(binwidth = 0.01)

ggplot(heights.weights, aes(x = Height)) +
  geom_density()

ggplot(heights.weights, aes(x = Height, fill = Gender)) +
  geom_density()

ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density()

ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density() +
  facet_grid(Gender ~ .)

m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
  geom_density()

set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)
range(normal.values)
range(cauchy.values)

ggplot(data.frame(X = normal.values), aes(x = X)) +
  geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) +
  geom_density()

gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) +
  geom_density()

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point()

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

ggplot(heights.weights[1:20, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()
ggplot(heights.weights[1:200, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()
ggplot(heights.weights[1:2000, ], aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth()

#
# Snippet 34
#

# Visualize how gender depends on height and weight.
ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = "none") + 
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw()

# An alternative using bright colors.
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) +
  geom_point()

#
# Snippet 35
#

heights.weights <- transform(heights.weights,
                             Male = ifelse(Gender == 'Male', 1, 0))

logit.model <- glm(Male ~ Weight + Height,
                   data = heights.weights,
                   family = binomial(link = 'logit'))

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Gender, alpha = 0.25)) +
  scale_alpha(guide = "none") + 
  scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
  theme_bw() +
  stat_abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = 'black')