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
  subgraphs = lapply(subnodes,function(x,gr){ 
    sub = subgraph(gr,classnode,x)
    return(sub)
  },g)
}

getBestCard_subgraph = function(g,mydata)
{
  lnodes = g$lnodenames
  resolv = 0
  mnodes = g$mnodenames 
  
  ln = lapply(lnodes,function(x){2:4})
  names(ln) = lnodes
  #势组合的排列
  ln = expand.grid(ln)
  
  #
  models = list()
  imodel = 1
  #对每一个排列组合学习势以及相关参数
  for(ri in 1 : nrow(ln))
  {
    print(ri)
    row = ln[ri,]
    new_data = mydata
    i = 0 
    while(i < length(lnodes))
    {
      #首先解决仅依赖于数据集中存在的潜变量
      ls = sapply(lnodes,function(l,dat,g){
        ch = g$nodes[[l]]$children
        if(length(setdiff(ch,colnames(dat))) == 0 )
        {
          return(l)
        }
      },new_data,g,simplify=F,USE.NAMES=F)
      ls = unlist(ls)
      for( l in ls)
      {
        childset = g$nodes[[l]]$children
        flatent = getLCAformularFromgraph(g,childset)
        res = poLCA(flatent, 
                    maxiter=50000, nclass=row[1,l], 
                    nrep=10, data=new_data[,childset],verbose=F)
        latentclass = as.factor(res$predclass)
        tmpnames = colnames(new_data)
        new_data = cbind(new_data,latentclass)
        colnames(new_data) = c(tmpnames,l)
      }
      i = i + length(ls)
    }
    #贝叶斯参数学习以及评分
    bn_graph = as.graph.bn(g)
    bn = bn.fit(bn_graph,data=new_data[,getallnodenames(g)])
    bic = BIC(bn,data=new_data[,getallnodenames(g)])
    isreg = isRegular(g,new_data)
    if(isreg)
    {
      models[[imodel]] = list(bng=bn_graph,data=new_data,bic=bic)
      imodel = imodel + 1
    }
  }
  bics = sapply(models,function(x){x$bic})
  models_sort = models[order(bics,decreasing = T)]
}

#势学习,指定模型，得到每个隐变量的最优势
learncard = function(g)
{
  
}

#结构学习
#通过引入父节点形成新的模型

#检查能否引入父节点

isCanAddParent = function(g,node)
{
  if(!hasnode(g,node))
  {
    warning(paste('there is no node in g',node,sep=','))
    return(F)
  }
  
  if(!is.element(node,g$lnodenames))
  {
    return(T)
  }
  
  if(length(c(g$nodes[[node]]$children,g$nodes[[node]]$parents)) == 2)
  {
    return(F)
  }
  return(T)
}
getModelsfromparentintro = function(g)
{
  nodes_2child = c()
  for(i in 1 : length(g$nodes))
  {
    nodename = names(g$nodes[i])
    node = g$nodes[[i]]
    if(length(node$children) >= 2)
    {
      if(isCanAddParent(g,nodename))
      {
        nodes_2child = c(nodes_2child,nodename)
      }
    }
  }
  models = list()
  imodels = 1
  for(node in nodes_2child)
  {
    comb = combn(g$nodes[[node]]$children,2)
    for(i in 1:ncol(comb))
    {
      ch = comb[,i]
      sg = parent_introduction(g,node,ch[1],ch[2])
      models[[ imodels]] = sg
      imodels =  imodels + 1
    }
  }
  
}
