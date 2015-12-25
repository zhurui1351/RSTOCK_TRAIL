require(igraph)

############################
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir("D:/Rcode/code/RSTOCK_TRAIL/trade/latentmodel/help")
######################################

getNBformularFromgraph = function(g)
{
  if( is.null(g$classnode))
  {
    stop('no class node')
  }
  
  childs = childnodes(g,g$classnode)
  if(length(childs) == 0)
  {
    stop('no child node for the class node')
  }
  frh = paste(childs,collapse = '+')
  f = paste(g$classnode,'~',frh)
  return(as.formula(f))
}

getLCAformularFromgraph = function(g,nodes)
{
  allnodes = c(g$mnodenames,g$classnode,g$lnodenames)
  if(length(setdiff(nodes,allnodes))>0)
  {
    stop('not all the nodes are in the graph')
  }
  
  f1 = paste(nodes,collapse = ',')
  f2 = paste('cbind(',f1,')','~ 1')
  f = as.formula(eval(f2))
  return(f)
}

########handy test code to compute HNB
#初始化为朴素贝叶斯分类器
mnodes = c('smastatus','ccistatus','cmostatus','shortstatus','longstatus','rocstatus',
           'sarstatus','wprstatus','kdjstatus','chkVostatus','obvstatus','cmostatus','trixstatus')
graph0 = graph(mnodes)
graph0 = assignClassNode(graph0,'leadclflag')
graph0 = initNBgraph(graph0)
plot.mygraph(graph0)

#数据准备
datasubset = analysedata_2000_6[,c('leadclflag',mnodes)]
data_train = as.data.frame(datasubset['1994/1999'])
data_train = na.omit(data_train)
data_test = as.data.frame(datasubset['2001'])
data_test = na.omit(data_test)

data_train =as.data.frame(ifelse(data_train=='more',1,ifelse(data_train == 'less',2,ifelse(data_train=='up','up','down'))))
data_test =  as.data.frame(ifelse(data_test=='more',1,ifelse(data_test == 'less',2,ifelse(data_test=='up','up','down'))))

#朴素贝叶斯的分类效果
fnb = getNBformularFromgraph(graph0)

model = naiveBayes(fnb,data=data_train,na.action = na.pass,laplace=1)
pr = predict(model,data_test)
table(data_test$leadclflag,pr)

#生成潜变量结构模型

childset = c('kdjstatus','smastatus')
flatent = getLCAformularFromgraph(graph0,childset)
res = poLCA(flatent, 
            maxiter=50000, nclass=2, 
            nrep=10, data=data_train[,childset])


pr_l1 = ifelse(res$posterior[,1] > res$posterior[,2],'1','2')
d_new_train = cbind(data_train,pr_l1)
colnames(d_new_train) = c(colnames(data_train),'temp0')


d_new_test = data_test[,childset]
pr_l2 = poLCA.posterior(res,mapply(as.numeric,d_new_test))
d_new_test = cbind(data_test,pr_l2)
colnames(d_new_test) = c(colnames(data_test),'temp0')


graph1 =parent_introduction(graph0,graph0$classnode,childset[1],childset[2])
plot.mygraph(graph1)
fnb = getNBformularFromgraph(graph1)

model = naiveBayes(fnb,data=d_new_train,na.action = na.pass)
pr = predict(model,d_new_test)
table(data_test$leadclflag,pr)


