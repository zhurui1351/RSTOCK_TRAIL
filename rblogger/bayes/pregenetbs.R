#http://www.sumsar.net/blog/2015/11/a-bayesian-model-to-calculate-whether-my-wife-is-pregnant/
period_onset <- as.Date(c("2014-07-02", "2014-08-02", "2014-08-29", "2014-09-25",
                          "2014-10-24", "2014-11-20", "2014-12-22", "2015-01-19"))
days_between_periods <- as.numeric(diff(period_onset))
