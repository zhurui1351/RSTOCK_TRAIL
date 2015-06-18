library(lubridate)

#获取时间的各个部分

date <- now()
year(date)
minute(date)
onth(date)
wday(date)
week(date)

date <- dmy("01-01-2010")
x1=ymd_hms('2010-0202 01:01:01')
x2=ymd_hms('2010-0203 02:01:05')

month(date)
month(date) <- 2

print(date)

#加减天等

date <- date - days(1)
date <- date - months(1)
date <- date - years(1)
date <- date - weeks(1)

#时区转化
x <- as.POSIXct("2009-08-07 00:00:01", tz = "America/New_York")
with_tz(x, "GMT")

#修改日期

update(date, year = 2010, month = 1, day = 1)

#时间间隔
start_2011 <- ymd_hms("2011-01-01 12:00:00")
start_2010 <- ymd_hms("2010-01-01 11:50:29")
difftime(start_2011,start_2010,units = "secs")

halloween <- ymd("2010-10-31")
christmas <- ymd("2010-12-25")
interval <- new_interval(halloween, christmas)
interval / dweeks(1)
#取摸
interval %% months(1)
#duration
new_duration(60)
dminutes(1)
1:3 * dhours(1)
start_2011 + dyears(1)
start_2012 <- ymd_hms("2012-01-01 12:00:00")
start_2012 + dyears(1)
dweeks(1) + ddays(6) + dhours(2) + dminutes(1.5) + dseconds(3)

#取整
april20 <- ymd_hms("2010-04-20 11:33:29")
round_date(april20, "day")

#thanksgivenday 不固定日期 ，而是十一月的第四个星期四
date <- ymd("2010-01-01")
month(date) <- 11
#当前周几
wday(date, label = TRUE, abbr = FALSE)
#周四的话就加3天
date <- date + days(3)
wday(date, label = TRUE, abbr = FALSE)
#添加三周
date + weeks(3)

#Memorial 五月的最后一个周一

date <- ymd("2010-01-01")
month(date) <- 5
#寻找本月最后一天
date <- ceiling_date(date, "month") - days(1)
wday(date, label = TRUE, abbr = FALSE)


lakers$date <- ymd(lakers$date)
qplot(date, 0, data = lakers, colour = game_type)
qplot(wday(date, label = TRUE, abbr = FALSE), data = lakers,
      geom = "histogram")
lakers$time <- ms(lakers$time)
lakers$time <- as.duration(lakers$time)
lakers$time <- dminutes(c(12, 24, 36, 48, 53)[lakers$period]) - lakers$time
