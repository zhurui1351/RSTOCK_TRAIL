require(igraph)
require(poLCA)
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

###################################
#数据准备
mnodes = c('Close','smasignal','ccisignal','rsisignal','macdsignal','adxsignal','mfisignal','bbandssignal','rocsignal',
           'sarsignal','wprsignal','kdjsignal','tdisignal','kstsignal','chkADsignal','obvsignal','cmosignal',
           'cmfsignal','trixsignal','willimadsignal','emvsignal' )
datasubset = analysedata[,c('leadclflag',mnodes)]

data_train = analysedata_2000_6
data_train = as.data.frame(data_train['1994/2014'])
data_train = na.omit(data_train)
data_test = as.data.frame(datasubset['2001'])
data_test = na.omit(data_test)

mnodes = colnames(data_train)
mnodes = mnodes[2:length(mnodes)]
##编码数据
ecode=function(x){
  
  tmp = sapply(x,function(x){
    if(x == 'less')
      return('1')
    else if (x == 'more')
      return('2')
    else if (x == 'middle')
      return('3')
    else if (x == 'up')
      return('1')
    else if (x == 'down')
      return('2')
    else if(x == 'short')
      return('1')
    else if(x == 'long')
      return('2')
    else if(x == 'hold')
      return('3')
    
    else return(as.character(x))
  })
  return(tmp)
}
#xx = as.data.frame(mapply(ecode,data_train))

data_train = as.data.frame(mapply(ecode,data_train),row.names=rownames(data_train))
data_test =  as.data.frame(mapply(ecode,data_test),row.names=rownames(data_test))

########handy test code to compute HNB
#初始化为朴素贝叶斯分类器
mnodes =unique(c('longstatus','Close','shortstatus','smastatus','ccistatus','rsistatus'
           ,'rocstatus','sarstatus','wprstatus','kdjstatus'
           ,'chkVostatus','obvstatus','cmostatus','trixstatus'))

mnodes =unique(c('longstatus','Close','shortstatus','smastatus','ccistatus','rsistatus'
                 ,'rocstatus'))

graph0 = graph(mnodes)
graph0 = assignClassNode(graph0,'leadclflag')
graph0 = initNBgraph(graph0)
plot.mygraph(graph0)

bn_graph0 = as.graph.bn(graph0)
bn1 = bn.fit(bn_graph0,data=data_train[,c(mnodes,'leadclflag')],method ='mle')
aic1 = AIC(bn1,data=data_train[,c(mnodes,'leadclflag')])
bic1 = BIC(bn1,data=data_train[,c(mnodes,'leadclflag')])

#朴素贝叶斯的分类效果
fnb = getNBformularFromgraph(graph0)

model = naiveBayes(fnb,data=data_train,na.action = na.pass,laplace=1)
pr = predict(model,data_train)
table(data_train$leadclflag,pr)

#生成潜变量结构模型

childset = c('kdjstatus','smastatus')
flatent = getLCAformularFromgraph(graph0,childset)
res = poLCA(flatent, 
            maxiter=50000, nclass=2, 
            nrep=10, data=data_train[,childset])


pr_l1 = ifelse(res$posterior[,1] > res$posterior[,2],'1','2')
d_new_train = cbind(data_train,pr_l1)
colnames(d_new_train) = c(colnames(data_train),'temp0')

graph1 =parent_introduction(graph0,graph0$classnode,childset[1],childset[2])
plot.mygraph(graph1)
fnb = getNBformularFromgraph(graph1)

model1 = naiveBayes(fnb,data=d_new_train,na.action = na.pass)
pr1 = predict(model,d_new_train)
table(d_new_train$leadclflag,pr1)

bn_graph1 = as.graph.bn(graph1)
bn1 = bn.fit(bn_graph1,data=d_new_train,method ='mle')
aic1 = AIC(bn1,data=d_new_train)
bic1 = BIC(bn1,data=d_new_train)
#iter 2
childset = c('cmostatus','rocstatus')
flatent1 = getLCAformularFromgraph(graph1,childset)
res1 = poLCA(flatent1, 
            maxiter=50000, nclass=2, 
            nrep=10, data=d_new_train[,childset])


pr_l2 = ifelse(res1$posterior[,1] > res1$posterior[,2],'1','2')
d_new_train1 = cbind(d_new_train,pr_l2)
colnames(d_new_train1) = c(colnames(d_new_train),'temp1')

graph2 =parent_introduction(graph1,graph1$classnode,childset[1],childset[2])
plot.mygraph(graph2)
fnb = getNBformularFromgraph(graph2)

model2 = naiveBayes(fnb,data=d_new_train1,na.action = na.pass)
pr1 = predict(model2,d_new_train1)
table(d_new_train1$leadclflag,pr1)


#分层贝叶斯学习

HNBstudy(data_train,mnodes,'leadclflag')

