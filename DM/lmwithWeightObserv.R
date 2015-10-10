#http://bc.bojanorama.pl/2015/09/linear-models-with-weighted-observations/
suppressMessages(local({
  library(dplyr)
  library(ggplot2)
  library(survey)
  library(knitr)
  library(tidyr)
  library(broom)
}))

set.seed(666)
N <- 30 # number of observations
aggregated <- data.frame(x=1:5) %>%
  mutate( y = round(2 * x + 2 + rnorm(length(x)) ),
          freq = as.numeric(table(sample(1:5, N, 
                                         replace=TRUE, prob=c(.3, .4, .5, .4, .3))))
  )
aggregated
# Disaggregated data
individuals <- aggregated[ rep(1:5, aggregated$freq) , c("x", "y") ]
ggplot(aggregated, aes(x=x, y=y, size=freq)) + geom_point() + theme_bw()

models <- list( 
  ind_lm = lm(y ~ x, data=individuals),
  raw_agg = lm( y ~ x, data=aggregated),
  ind_svy_glm = svyglm(y~x, design=svydesign(id=~1, data=individuals),
                       family=gaussian() ),
  ind_glm = glm(y ~ x, family=gaussian(), data=individuals),
  wei_lm = lm(y ~ x, data=aggregated, weight=freq),
  wei_glm = glm(y ~ x, data=aggregated, family=gaussian(), weight=freq),
  svy_glm = svyglm(y ~ x, design=svydesign(id=~1, weights=~freq, data=aggregated),
                   family=gaussian())
)

results <- do.call("rbind", lapply( names(models), function(n) cbind(model=n, tidy(models[[n]])) )) %>%
  gather(stat, value, -model, -term)
results %>% filter(stat=="estimate") %>% 
  select(model, term, value) %>%
  spread(term, value)

results %>% filter(stat=="std.error") %>%
  select(model, term, value) %>%
  spread(term, value)


# p-values
results %>% filter(stat=="p.value") %>%
  mutate(p=format.pval(value)) %>%
  select(model, term, p) %>%
  spread(term, p)

