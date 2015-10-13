#https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
preg = data.frame(name=c('John Smith','Jane Doe','Mary Johnson'),treatmenta =c(NA,4,6),treatmentb=c(18,1,7))
preg
preg2 = data.frame(treatment =c('a','b'),John.Smith =c(NA,18),Jane.Doe=c(4,1),Mary.Johnson=c(6,7))
preg2

library(tidyr)
library(dplyr)
preg2 <- preg %>% 
  gather(treatment, n, treatmenta:treatmentb) %>%
  mutate(treatment = gsub("treatment", "", treatment)) %>%
  arrange(name, treatment)

song <- billboard3 %>% 
  select(artist, track, year, time) %>%
  unique() %>%
  mutate(song_id = row_number())