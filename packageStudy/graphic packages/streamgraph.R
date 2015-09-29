#安装包 devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
packageVersion("streamgraph")
library(dplyr)

ggplot2::movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) -> dat

streamgraph(dat, "genre", "n", "year", interactive=TRUE) %>%
  sg_axis_x(20, "year", "%Y") %>%
  sg_fill_brewer("PuOr")