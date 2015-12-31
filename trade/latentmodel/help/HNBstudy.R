#模型是否正则
#g位对应的图结构，data是数据集
isRegular = function(g,mydata)
{
  lnodes = g$lnodenames
  #所有节点的水平数
  lv = mapply(levels,mydata)
  for(n in lnodes)
  {
    node = g$nodes[[n]]
    nb = c(node$parents,node$children)
    lv_sub = lv[nb]
    lv_cnt = sapply(lv_sub, length)
    total = prod(lv_cnt)
    m = max(lv_cnt)
    mcard = total / m 
    if( length(lv[[n]]) > mcard)
    {
      return(F)
    }
  }
  return(T)
}
# 分解图,按根分解
decompost = function(g)
{
  classnode = g$classnode
  subnodes = g$nodes[[classnode]]$children
  
}
#势学习,指定模型，得到每个隐变量的最优势
learncard = function(g)
{
  
}