#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/04-Ranking/priority_inbox.R

library('tm')
library('ggplot2')
library('plyr')
library('reshape2')

data.path <- file.path("C:/data", "03-Classification", "data")
easyham.path <- file.path(data.path, "easy_ham")
msg.full <- function(path)
{
  con <- file(path, open = "rt")
  msg <- readLines(con)
  close(con)
  return(msg)
}

get.from <- function(msg.vec)
{
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from  != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

get.subject <- function(msg.vec)
{
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0)
  {
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else
  {
    return("")
  }
}

get.msg <- function(msg.vec)
{
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

get.date <- function(msg.vec)
{
  date.grep <- grepl("^Date: ", msg.vec)
  date.grep <- which(date.grep == TRUE)
  date <- msg.vec[date.grep[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}


parse.email <- function(path)
{
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(file.path(easyham.path, p)))

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")


date.converter <- function(dates, pattern1, pattern2)
{
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)
allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)
priority.df <- allparse.df[with(allparse.df, order(Date)), ]
#训练集
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]

#from.weight <- ddply(priority.train,.(From.EMail),summarise,Freq=length(Subject))

from.weight <- melt(with(priority.train, table(From.EMail)), 
                    value.name="Freq")
from.weight <- from.weight[with(from.weight, order(Freq)), ]
from.ex <- subset(from.weight, Freq > 6)
#绘图
from.scales <- ggplot(from.ex) +
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,
                xmax = 1:nrow(from.ex) + 0.5,
                ymin = 0,
                ymax = Freq,
                fill = "lightgrey",
                color = "darkblue")) +
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +
  coord_flip() +
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5, hjust = 1))
#坐标变换
from.weight <- transform(from.weight,
                         Weight = log(Freq + 1),
                         log10Weight = log10(Freq + 1))
#变换坐标后 重新绘图
from.rescaled <- ggplot(from.weight, aes(x = 1:nrow(from.weight))) +
  geom_line(aes(y = Weight, linetype = "ln")) +
  geom_line(aes(y = log10Weight, linetype = "log10")) +
  geom_line(aes(y = Freq, linetype = "Absolute")) +
  scale_linetype_manual(values = c("ln" = 1,
                                   "log10" = 2,
                                   "Absolute" = 3),
                        name = "Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())


find.threads <- function(email.df)
{
  response.threads <- strsplit(email.df$Subject, "re: ")
  is.thread <- sapply(response.threads,
                      function(subj) ifelse(subj[1] == "", TRUE, FALSE))
  threads <- response.threads[is.thread]
  senders <- email.df$From.EMail[is.thread]
  threads <- sapply(threads,
                    function(t) paste(t[2:length(t)], collapse = "re: "))
  return(cbind(senders,threads))
}

threads.matrix <- find.threads(priority.train)

email.thread <- function(threads.matrix)
{
  senders <- threads.matrix[, 1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq),
                          senders.freq,
                          log(senders.freq + 1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors=FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.EMail", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(threads.matrix)
#根据线程中最活跃的发件人确定权重
thread.counts <- function(thread, email.df)
{
  # Need to check that we are not looking at the original message in a thread, 
  # so we check the subjects against the 're:' cue.
  thread.times <- email.df$Date[which(email.df$Subject == thread |
                                        email.df$Subject == paste("re:", thread))]
  freq <- length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(difftime(max.time, min.time, units = "secs"))
  if(freq < 2)
  {
    return(c(NA, NA, NA))
  }
  else
  {
    trans.weight <- freq / time.span
    log.trans.weight <- 10 + log(trans.weight, base = 10)
    return(c(freq, time.span, log.trans.weight))
  }
}

# This function uses the threads.counts function to generate a weights
# for all email threads.
get.threads <- function(threads.matrix, email.df)
{
  threads <- unique(threads.matrix[, 2])
  thread.counts <- lapply(threads,
                          function(t) thread.counts(t, email.df))
  thread.matrix <- do.call(rbind, thread.counts)
  return(cbind(threads, thread.matrix))
}
thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors = FALSE)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE)


term.counts <- function(term.vec, control)
{
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
  return(rowSums(as.matrix(vec.tdm)))
}

thread.terms <- term.counts(thread.weights$Thread,
                            control = list(stopwords = TRUE))
thread.terms <- names(thread.terms)

term.weights <- sapply(thread.terms,
                       function(t) mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed = TRUE)]))
term.weights <- data.frame(list(Term = names(term.weights),
                                Weight = term.weights),
                           stringsAsFactors = FALSE,
                           row.names = 1:length(term.weights))
msg.terms <- term.counts(priority.train$Message,
                         control = list(stopwords = TRUE,
                                        removePunctuation = TRUE,
                                        removeNumbers = TRUE))
msg.weights <- data.frame(list(Term = names(msg.terms),
                               Weight = log(msg.terms, base = 10)),
                          stringsAsFactors = FALSE,
                          row.names = 1:length(msg.terms))

# Remove words that have a zero weight
msg.weights <- subset(msg.weights, Weight > 0)

# This function uses our pre-calculated weight data frames to look up
# the appropriate weightt for a given search.term.  We use the 'term'
# parameter to dertermine if we are looking up a word in the weight.df
# for it message body weighting, or for its subject line weighting.
get.weights <- function(search.term, weight.df, term = TRUE)
{
  if(length(search.term) > 0)
  {
    if(term)
    {
      term.match <- match(names(search.term), weight.df$Term)
    }
    else
    {
      term.match <- match(search.term, weight.df$Thread)
    }
    match.weights <- weight.df$Weight[which(!is.na(term.match))]
    if(length(match.weights) < 1)
    {
      return(1)
    }
    else
    {
      return(mean(match.weights))
    }
  }
  else
  {
    return(1)
  }
}

# Our final step is to write a function that will assign a weight to each message based
# on all of our, we create a function that will assign a weight to each message based on
# the mean weighting across our entire feature set.
rank.message <- function(path)
{
  msg <- parse.email(path)
  # Weighting based on message author
  
  # First is just on the total frequency
  from <- ifelse(length(which(from.weight$From.EMail == msg[2])) > 0,
                 from.weight$Weight[which(from.weight$From.EMail == msg[2])],
                 1)
  
  # Second is based on senders in threads, and threads themselves
  thread.from <- ifelse(length(which(senders.df$From.EMail == msg[2])) > 0,
                        senders.df$Weight[which(senders.df$From.EMail == msg[2])],
                        1)
  
  subj <- strsplit(tolower(msg[3]), "re: ")
  is.thread <- ifelse(subj[[1]][1] == "", TRUE, FALSE)
  if(is.thread)
  {
    activity <- get.weights(subj[[1]][2], thread.weights, term = FALSE)
  }
  else
  {
    activity <- 1
  }
  
  # Next, weight based on terms    
  
  # Weight based on terms in threads
  thread.terms <- term.counts(msg[3], control = list(stopwords = TRUE))
  thread.terms.weights <- get.weights(thread.terms, term.weights)
  
  # Weight based terms in all messages
  msg.terms <- term.counts(msg[4],
                           control = list(stopwords = TRUE,
                                          removePunctuation = TRUE,
                                          removeNumbers = TRUE))
  msg.weights <- get.weights(msg.terms, msg.weights)
  
  # Calculate rank by interacting all weights
  rank <- prod(from,
               thread.from,
               activity, 
               thread.terms.weights,
               msg.weights)
  
  return(c(msg[1], msg[2], msg[3], rank))
}

# Find splits again
train.paths <- priority.df$Path[1:(round(nrow(priority.df) / 2))]
test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):nrow(priority.df)]

# Now, create a full-featured training set.
train.ranks <- suppressWarnings(lapply(train.paths, rank.message))
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.paths, train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)
names(train.ranks.df) <- c("Message", "Date", "From", "Subj", "Rank", "Type")
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

# Set the priority threshold to the median of all ranks weights
priority.threshold <- median(train.ranks.df$Rank)

# Visualize the results to locate threshold
threshold.plot <- ggplot(train.ranks.df, aes(x = Rank)) +
  stat_density(aes(fill="darkred")) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_fill_manual(values = c("darkred" = "darkred"), guide = "none") +
  theme_bw()
ggsave(plot = threshold.plot,
       filename = file.path("images", "01_threshold_plot.pdf"),
       height = 4.7,
       width = 7)

# Classify as priority, or not
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)

# Now, test our ranker by performing the exact same procedure on the test data
test.ranks <- suppressWarnings(lapply(test.paths,rank.message))
test.ranks.matrix <- do.call(rbind, test.ranks)
test.ranks.matrix <- cbind(test.paths, test.ranks.matrix, "TESTING")
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors = FALSE)
names(test.ranks.df) <- c("Message","Date","From","Subj","Rank","Type")
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)
test.ranks.df$Priority <- ifelse(test.ranks.df$Rank >= priority.threshold, 1, 0)

# Finally, we combine the data sets.
final.df <- rbind(train.ranks.df, test.ranks.df)
final.df$Date <- date.converter(final.df$Date, pattern1, pattern2)
final.df <- final.df[rev(with(final.df, order(Date))), ]

# Save final data set and plot results.
write.csv(final.df, file.path("data", "final_df.csv"), row.names = FALSE)

testing.plot <- ggplot(subset(final.df, Type == "TRAINING"), aes(x = Rank)) +
  stat_density(aes(fill = Type, alpha = 0.65)) +
  stat_density(data = subset(final.df, Type == "TESTING"),
               aes(fill = Type, alpha = 0.65)) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_alpha(guide = "none") +
  scale_fill_manual(values = c("TRAINING" = "darkred", "TESTING" = "darkblue")) +
  theme_bw()
ggsave(plot = testing.plot,
       filename = file.path("images", "02_testing_plot.pdf"),
       height = 4.7,
       width = 7)