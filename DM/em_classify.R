#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/03-Classification/email_classify.R
library('tm')
library('ggplot2')

spam.path <- file.path("c:/data", "spam")
spam2.path <- file.path("c:/data", "spam_2")
easyham.path <- file.path("c:/data", "easy_ham")
easyham2.path <- file.path("c:/data", "easy_ham_2")
hardham.path <- file.path("c:/data", "hard_ham")
hardham2.path <- file.path("c:/data", "hard_ham_2")
#抖动点
x <- runif(1000, 0, 40)
y1 <- cbind(runif(100, 0, 10), 1)
y2 <- cbind(runif(800, 10, 30), 2)
y3 <- cbind(runif(100, 30, 40), 1)

val <- data.frame(cbind(x, rbind(y1, y2, y3)),
                  stringsAsFactors = TRUE)
#用jitter产生微小的随机抖动 以免相同的点被覆盖
ex1 <- ggplot(val, aes(x, V2)) +
  geom_jitter(aes(shape = as.factor(V3)),
              position = position_jitter(height = 2)) +
  scale_shape_discrete(guide = "none", solid = FALSE) +
  geom_hline(aes(yintercept = c(10,30)), linetype = 2) +
  theme_bw() +
  xlab("X") +
  ylab("Y")

#获取文件内容
get.msg <- function(path)
{
  con <- file(path, open = "rt")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  close(con)
  return(paste(msg, collapse = "\n"))
}
#读入所有的垃圾邮件，形成list
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) {
                     get.msg(file.path(spam.path, p))
                     }
                   )
#词频文档矩阵
get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = 2)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

spam.tdm <- get.tdm(all.spam)

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# Add the term density and occurrence rate
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

head(spam.df[with(spam.df,order(-occurrence)),])

#建立易识别的垃圾邮件的矩阵
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                             function(i)
                             {
                               length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                             })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)
head(easyham.df[with(easyham.df,order(-occurrence)),])

#分类函数，prior是先验概率 c是当词汇不是资料库里的词汇时，是垃圾邮件的概率，如果处理为0 ，会有很大的问题
classify.email <- function(path, training.df, prior = 0.5, c = 1e-6)
{
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}
#对不易分类的垃圾邮件采用算法
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(file.path(hardham.path, p), training.df = spam.df))

hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(file.path(hardham.path, p), training.df = easyham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)
#将简单分类封装成函数
spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# Classify them all!
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                          function(p)
                                          {
                                            spam.classifier(file.path(easyham2.path, p))
                                          }))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                          function(p)
                                          {
                                            spam.classifier(file.path(hardham2.path, p))
                                          }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                       function(p)
                                       {
                                         spam.classifier(file.path(spam2.path, p))
                                       }))
