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
  return(subgraphs)
}

getBestCard_subgraph = function(g,mydata,card = 4)
{
  lnodes = g$lnodenames
  resolv = 0
  mnodes = g$mnodenames 
  
  #子图不含隐变量
  if(length(lnodes) == 0)
  {
    bn_graph = as.graph.bn(g)
    bn = bn.fit(bn_graph,data=mydata[,getallnodenames(g)])
    bic = BIC(bn,data=mydata[,getallnodenames(g)])
    isreg = isRegular(g,mydata)
    if(isreg)
    {
      return(list(bng=bn_graph,data=mydata,bic=bic))
    }
    else
    {
      return(NULL)
    }
  }
  
  ln = lapply(lnodes,function(x){2:card})
  names(ln) = lnodes
  #势组合的排列
  ln = expand.grid(ln)
  
  #选择最优BIC模型
  models = list()
  imodel = 1
  #对每一个排列组合学习势以及相关参数
  for(ri in 1 : nrow(ln))
  {
   # print(ri)
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
      ls = ls[!(ls %in% colnames(new_data))]
      for( l in ls)
      {
        nclass=ifelse(length(row) == 1,row,row[1,l])
        childset = g$nodes[[l]]$children
        flatent = getLCAformularFromgraph(g,childset)
        resdata = as.data.frame(new_data[,childset])
        colnames(resdata) = childset
        res = poLCA(flatent, 
                    maxiter=50000, nclass=nclass, 
                    nrep=3, data=resdata,verbose=T)
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
    isreg = T
    if(isreg)
    {
      models[[imodel]] = list(bng=bn_graph,data=new_data,bic=bic)
      imodel = imodel + 1
    }
  }
  bics = sapply(models,function(x){x$bic})
  models_sort = models[order(bics,decreasing = F)]
  return(models_sort[1])
}

#势学习,指定模型，得到每个隐变量的最优势
learncard = function(g,mydata)
{
  
  data = mydata
  subgraphs = decompost(g)
  for(i in 1:length(subgraphs))
  {
    print(i)
    sg = subgraphs[[i]]
    #不含隐变量
    if(length(sg$lnodenames) == 0 ) next
    sg_best = getBestCard_subgraph(sg,mydata) 
    if(is.null(sg_best))
      next
    sgdata = sg_best[[1]]$data
    tmpname = colnames(data)
    data = cbind(data,sgdata[,sg$lnodenames])
    colnames(data)=c(tmpname,sg$lnodenames)
  }
  return(data)
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
  
  if(is.element(node,g$mnodenames))
  {
    return(F)
  }
  
  if(length(c(g$nodes[[node]]$children,g$nodes[[node]]$parents)) %in% c(2,3))
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
  return(models)
}

#删除节点形成新的模型
#仅能删除只有不超过2个孩子的隐节点
getModelsfromdeletenode = function(g)
{
  lnodes = g$lnodenames
  if(length(lnodes) < 2)
  {
    return(NULL)
  }
  nodes_delete = c()
  for(lnode in lnodes)
  {
    if(length(g$nodes[[lnode]]$children) <= 2)
      nodes_delete = c(nodes_delete,lnode)
  }
  
  models = list()
  imodels = 1
  for(node in nodes_delete)
  {
    sg = node_deletion(g,node)
    models[[imodels]] = sg
    imodels = imodels + 1
  }
  return(models)
}

issinglelyconlatent = function(g,node)
{
  if(!hasnode(g,node))
  {
    warning('g has not this node')
    return(F)
  }
  if( !is.element(node,g$lnodenames))
  {
    return(F)
  }
  
  if(length(c(g$nodes[[node]]$parents,g$nodes[[node]]$children)) == 2)
    return(T)
  else
    return(F)
}

#通过变换父节点生成新的模型
getModelsfromalterparent = function(g)
{
  nodes_alter_p = c()
  nodes_alter_sb = c()
  nodes = c(g$mnodenames,g$lnodenames)
  for(node in nodes)
  {
    p = g$nodes[[node]]$parents
    if(length(p) != 1 ) next
    gp =  g$nodes[[p]]$parents
    sb = g$nodes[[node]]$nbr
    #没有祖父节点且没有非叶子的邻居节点
    if( length(gp) == 0 && length(intersect(sb,g$lnodenames)) == 0)
    {
      next
    }
    #父节点是隐节点，且没有邻居或只有一个邻居
    if(is.element(p,g$lnodenames) && length(sb) %in% c(0,1))
    {
      next
    }
    if(!( is.element(p,g$lnodenames) && length(sb) == 1 && issinglelyconlatent(g,sb)))
    {
      if(length(gp) > 0 )
      {
        nodes_alter_p = c(node,nodes_alter_p)  
      }
    }
    if(!( is.element(p,g$lnodenames) && length(sb) == 1 && issinglelyconlatent(g,p)))
    {
      if(length(intersect(sb,g$lnodenames)) > 0)
      {
        nodes_alter_sb = c(node,nodes_alter_sb) 
      } 
    }
  }
  
  models = list()
  imodels = 1
  
  for(node in nodes_alter_p)
  {
    p = g$nodes[[node]]$parents
    gp =  g$nodes[[p]]$parents
    sg = parent_alteration(g,p,node,gp)
    models[[imodels]] = sg
    imodels = imodels + 1
  }
  for(node in nodes_alter_sb)
  {
    p = g$nodes[[node]]$parents
    sb =  g$nodes[[node]]$nbr
    sb = intersect(sb,g$lnodenames)
    for(s in sb)
    {
      sg = parent_alteration(g,p,node,s)
      models[[imodels]] = sg
      imodels = imodels + 1
    }
  }
  return(models)
}

combindlist = function(l0,l1)
{
  listtmp = l0
  n0 = length(l0)
  n1 = length(l1)
  if(n0 == 0)
    return(l1)
  if(n1 ==0)
    return(l0)
  for(i in 1:length(l1))
  {
    listtmp[[n0+i]] = l1[[i]]
  }
  return(listtmp)
}

#潜变量分层贝叶斯学习
HNBstudy = function(mydata,mnodes,classnode)
{
 # mnodes =unique(c('longstatus','Close','smastatus','ccistatus','rsistatus'
  #                 ,'rocstatus'))
  
  graph0 = graph(mnodes)
  graph0 = assignClassNode(graph0,classnode)
  graph0 = initNBgraph(graph0)
  plot.mygraph(graph0)
  bn_graph0 = as.graph.bn(graph0)
  bn0 = bn.fit(bn_graph0,data=mydata[,c(mnodes,classnode)],method ='mle')
  bic0 = BIC(bn0,data=mydata[,c(mnodes,classnode)])
  
  g = graph0
  bic_c = bic0
  bic_t = -Inf
  
  allgraph = list()
  iall = 1
  
  while((bic_c-bic_t) > 0.1)
  {
    bic_t = bic_c
    subg_addp = getModelsfromparentintro(g)
    subg_alterp = getModelsfromalterparent(g)
    subg_delet = getModelsfromdeletenode(g)
    
    sugs = combindlist(subg_addp,subg_alterp)
    sugs = combindlist(sugs,subg_delet)
    
    dif = 0.1
    bic_t = Inf
    
    
    bic_c = -Inf
    tmpg = NULL
    print(length(sugs))
    
    if(length(sugs) == 0)
    {
      break
    }
    
    for(i in 1:length(sugs))
    {
      print(i)
      sg = sugs[[i]]
      sg_card = learncard(sg,mydata)
      if(length(setdiff(getallnodenames(sg),colnames(sg_card))) > 0 )
        next
      bn_sg = as.graph.bn(sg)
      bn = bn.fit(bn_sg,data=sg_card[,getallnodenames(sg)],method ='mle')
      bic = BIC(bn,data=sg_card[,getallnodenames(sg)])
      if(bic > bic_c)
      {
        bic_c = bic
        tmpg = sg
        print(bic)
        plot.mygraph(sg)
      }
    }
    
    g = tmpg
    allgraph[[iall]] = g
    iall = iall + 1
  }
 return(allgraph)
}