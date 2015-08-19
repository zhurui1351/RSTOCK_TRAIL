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
                     print(p)
                     get.msg(file.path(spam.path, p))
                     }
                   )
