require(dplyr)
data.frame(x = letters) %>% sapply(class)
data_frame(x = letters) %>% sapply(class)
data_frame(x = 1:3, y = list(1:5, 1:10, 1:20))

data.frame(`crazy name` = 1) %>% names()
data_frame(`crazy name` = 1) %>% names()


data_frame(x = 1:5, y = x ^ 2)
data_frame(x = 1:5) %>% class()

df <- data.frame(a = 1:2, b = 1:2)
str(df[, "a"])
tbldf <- tbl_df(df)
str(tbldf[, "a"])

l2 <- replicate(26, sample(100), simplify = FALSE)
names(l2) <- letters
microbenchmark::microbenchmark(
  as_data_frame(l2),
  as.data.frame(l2)
)

location(iris)
iris2 <- iris
location(iris2)
changes(iris2, iris)
iris3 <- mutate(iris, Sepal.Length = Sepal.Length * 2)

#http://www.milanor.net/blog/dplyr-do-tips-for-using-and-programming/

set.seed(100)
ds <- data.frame(group=c(rep("a",100), rep("b",100), rep("c",100)), 
                 x=rnorm(n = 300, mean = 3, sd = 2), y=rnorm(n = 300, mean = 2, sd = 2))

#类型转换
ds <- tbl_df(ds)
ds
#使用.作为占位符
ds %>% do(head(.))
ds %>% group_by(group) %>% do(head(.))
ds %>% group_by(group) %>% do(out=tail(.$x, 3))
ds %>% group_by(group) %>% do(data.frame(out=tail(.$x, 3)))
my_fun <- function(x, y){
  res_x = mean(x) + 2
  res_y = mean(y) * 5 
  return(data.frame(res_x, res_y))
}
ds %>% group_by(group) %>% do(out=my_fun(x=.$x, y=.$y))

ds %>% group_by(group) %>% do(my_fun(x=.$x, y=.$y))
