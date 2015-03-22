#looking for the minnum for f(x1,x2) =sqr(x1) + sqr(x2) + sqr(x3) - 2*x1x2 -2*x1*x3 -2*x2x3
# -5<x1<5 -5<x2<5 -5<x3<5

#i初始化种群 gene = c(x1,x2,x3)
population =list()
for(i in 1:16)
{
  gene = c(x1=runif(1,min=-5,max=5),x2=runif(1,min=-5,max=5),x3=runif(1,min=-5,max=5))
  population[[i]] = gene
  
}

#fit fuction
fit <- function(x1,x2,x3)
{
  return(x1*x1 + x2*x2 +x3*x3- 2*x1*x2 - 2*x1*x3 -2*x2*x3)
}
# 计算得分，
orderPopulation = function(population)
{
  score=sapply(population,function(x){fit(x[1],x[2],x[3])})
  
  #select ,order first
  order_index = order(score,decreasing=T)
  tmp = list()
  for(i in 1:length(order_index))
  {
    tmp[[i]]  = population[[order_index[i]]]
  }
  population = tmp
}
population = orderPopulation(population)
# 将个体按得分排序，使用位置索引来衡量权重，比如排在第七位的权重就是7/sum(1:length(population))
#寻找随机概率落在哪个得分段
findPos = function(x,l)
{
  for(i in 1:l)
  {
    if(sum(1:i) >=x)
      return(i)
  }
}

#recombination
recombination = function(g1,g2){
  l = length(g1)
  breakpoint =floor(runif(1,min=1,max=l))
  temp = g1[breakpoint:l]
  g1[breakpoint:l] = g2[breakpoint:l]
  g2[breakpoint:l] =temp
  return(list(g1,g2))
}

#Mutation
mutation = function(g,ratio){
  range =c(-10 : -1,1:10)
  l = length(g)
  mutationpoint =floor(runif(1,min=1,max=l))
  g[mutationpoint] = g[mutationpoint] + sample(range,1) * ratio
  return(g)
  
}
#突变率
mutation_ratio = 0.3
#结合律
recombination_ratio = 0.6
generations = 100
count = 1

gap = 0.01
while(count < generations )
{
  #上一代最好的个体和得分
  best_populaton = population[[16]]
  best_score = fit(best_populaton[[1]],best_populaton[[2]],best_populaton[[3]])
  # select  形成下一代
  next_population = list()
  for(i in 1:8)
  {
    n = findPos(sample(1:sum(1:16),1),16)
    c1 = population[[n]]
    n = findPos(sample(1:sum(1:16),1),16)
    c2 = population[[n]]
    children = list(c1,c2)
    #recombination
    if(runif(1,0,1) < mutation_ratio)
    {
      children = recombination(c1,c2)
    }
    if(runif(1,0,1)<mutation_ratio)
    {
      children[[1]] = mutation(children[[1]],0.01)
    }
    if(runif(1,0,1)<mutation_ratio)
    {
      children[[2]] = mutation(children[[1]],0.01)
    }
    next_population = append(next_population,children)
    next_population = orderPopulation(next_population)
  }
  best_child_populaton = next_population[[16]]
  best_child_score = fit(best_child_populaton[[1]],best_child_populaton[[2]],best_child_populaton[[3]])
  gap= best_score - best_child_score
  count = count + 1
}
