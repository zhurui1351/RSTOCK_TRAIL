#https://aschinchon.wordpress.com/2015/08/03/going-bananas-2-a-needle-in-a-haystack/
#随机生成串，并在pi中寻找
library(rvest)
library(stringr)
library(reshape2)
library(ggplot2)
library(extrafont)
windowsFonts(Comic=windowsFont("Comic Sans MS"))
library(dplyr)
library(magrittr)
library(scales)
p = html("http://www.geom.uiuc.edu/~huberty/math5337/groupe/digits.html")
f = html("http://www.goldennumber.net/wp-content/uploads/2012/06/Phi-To-100000-Places.txt")
e = html("http://apod.nasa.gov/htmltest/gifcity/e.2mil")
p %>%  
  html_text() %>% 
  substr(., regexpr("3.14",.), regexpr("Go to Historical",.)) %>% 
  gsub("[^0-9]", "", .)  %>% 
  substr(., 1, 100000) -> p
f %>%  
  html_text() %>% 
  substr(., regexpr("1.61",.), nchar(.)) %>% 
  gsub("[^0-9]", "", .) %>%  
  substr(., 1, 100000) -> f
e %>%  
  html_text() %>% 
  substr(., regexpr("2.71",.), nchar(.)) %>% 
  gsub("[^0-9]", "", .) %>% 
  substr(., 1, 100000) -> e
r = paste0(sample(0:9, 100000, replace = TRUE), collapse = "")
results=data.frame(Cut=numeric(0), Pi=numeric(0), Phi=numeric(0), e=numeric(0), Random=numeric(0))
bins=20
dgts=6
samp=min(10^dgts*2/100, 10000)
for (i in 1:bins) {
  cut=100000/bins*i
  p0=substr(p, start=0, stop=cut)
  f0=substr(f, start=0, stop=cut)
  e0=substr(e, start=0, stop=cut)
  r0=substr(r, start=0, stop=cut)
  sample(0:(10^dgts-1), samp, replace = FALSE) %>% str_pad(dgts, pad = "0") -> comb
  comb %>% sapply(function(x) grepl(x, p0)) %>% sum() -> p1
  comb %>% sapply(function(x) grepl(x, f0)) %>% sum() -> f1
  comb %>% sapply(function(x) grepl(x, e0)) %>% sum() -> e1
  comb %>% sapply(function(x) grepl(x, r0)) %>% sum() -> r1
  results=rbind(results, data.frame(Cut=cut, Pi=p1, Phi=f1, e=e1, Random=r1))
}
results=melt(results, id.vars=c("Cut") , variable.name="number", value.name="matches")
opts=theme(
  panel.background = element_rect(fill="darkolivegreen1"),
  panel.border = element_rect(colour="black", fill=NA),
  axis.line = element_line(size = 0.5, colour = "black"),
  axis.ticks = element_line(colour="black"),
  panel.grid.major = element_line(colour="white", linetype = 1),
  panel.grid.minor = element_blank(),
  axis.text.y = element_text(colour="black"),
  axis.text.x = element_text(colour="black"),
  text = element_text(size=20, family="Comic"),
  legend.text = element_text(size=25),
  legend.key = element_blank(),
  legend.position = c(.75,.2),
  legend.background = element_blank(),
  plot.title = element_text(size = 30))
ggplot(results, aes(x = Cut, y = matches/samp, color = number))+
  geom_line(size=1.5, alpha=.8)+
  scale_color_discrete(name = "")+
  scale_x_continuous(breaks=seq(100000/bins, 100000, by=100000/bins))+
  scale_y_continuous(labels = percent)+
  theme(axis.text.x = element_text(angle = 90, vjust=.5, hjust = 1))+
  labs(title=paste0("Finding ",dgts, "-size strings into 100.000-digit numbers"), 
       x="Cut Position", 
       y="% of Matches")+opts
