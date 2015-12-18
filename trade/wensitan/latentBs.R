require(igraph)

#初始化图，第一次初始化后都是显变量的节点
graph = function(nodes)
{
  numnodes = length(nodes)
  g = list()
  g$nodes = list()

  
  g$lnodenames = c()
  g$mnodenames = nodes
  
  for( n in nodes)
  {
    g$nodes[[n]] = list()
    g$nodes[[n]]$children = character(0)
    g$nodes[[n]]$parents = character(0)
    g$nodes[[n]]$nbr = character(0)
    
    g$arcs = matrix(nrow = 0,ncol=2,dimnames = list(NULL,c('from','to')))
  }
  return(g)
}

setarc = function(g,from,to)
{
  nodes = c(g$lnodenames,g$mnodenames)
  if(!(from %in% nodes && to %in% nodes))
    print(" error node name in setarc")
  if(nrow(subset(g$arcs,g$arcs[,'from']==from & g$arcs[,'to']==to)) > 0)
    return(g)
  
  g$arcs = rbind(g$arcs,c(from,to))
 
  g$nodes[[to]]$nbr = c(g$nodes[[from]]$children,g$nodes[[to]]$nbr)
  for(nbr in g$nodes[[to]]$nbr)
  {
    g$nodes[[nbr]]$nbr = c(g$nodes[[nbr]]$nbr,to)
  }
  g$nodes[[to]]$parents = c(g$nodes[[to]]$parents,from)
  
  g$nodes[[from]]$children = c(g$nodes[[from]]$children,to)
  
  return(g)
}

deletearc = function(g,from,to)
{
  nodes = c(g$lnodenames,g$mnodenames)
  if(!(from %in% nodes && to %in% nodes))
  {
    print('no that node')
    return(g)
  }
  
  lineno = which(g$arcs[,"from"] ==from & g$arcs[,'to'] ==to)
  lineno = ifelse(length(lineno) == 0,0,lineno)
  if(lineno == 0)
  {
    print('no that arcs')
    return(g)
  }
  
  g$arcs =g$arcs[-lineno,]

  g$nodes[[from]]$children = g$nodes[[from]]$children[-which(g$nodes[[from]]$children == to)]
  g$nodes[[to]]$parents =  g$nodes[[to]]$parents[-which( g$nodes[[to]]$parents == from)]
  
  for(node in g$nodes[[to]]$nbr)
  {
    #查看是否有其他共有父亲节点
    np = intersect(g$nodes[[node]]$parents,g$nodes[[to]]$parents)
    if(length(np) > 1)
    {
      break
    }
    
    g$nodes[[to]]$nbr =  g$nodes[[to]]$nbr[-which( g$nodes[[to]]$nbr == node)]
    g$nodes[[node]]$nbr =  g$nodes[[node]]$nbr[-which( g$nodes[[node]]$nbr == to)]
    
  }
  return(g)
}

setarcs = function(g,arcs)
{
  for(i in 1 : nrow(arcs))
  {
    g = setarc(g,arcs[i,]['from'],arcs[i,]['to'])
  }
  return(g)
}



plot.mygraph = function(g)
{
  if(nrow(g$arcs) > 0)
  {
    vedges = as.vector(t(g$arcs))
    gr = make_graph(vedges)
    plot(gr)
  }
 
}

addlatentnodes = function(g,nodes)
{
  if(any(nodes %in% c(g$lnodenames,g$mnodenames))) 
  {
    warning('the nodes have repeated names')
    return(g)
  }
  g$lnodenames = c(g$lnodenames,nodes)
  g$nodes = c(g$nodes,nodes)
  for( n in nodes)
  {
    g$nodes[[n]] = list()
    g$nodes[[n]]$children = character(0)
    g$nodes[[n]]$parents = character(0)
    g$nodes[[n]]$nbr = character(0)
  }
  return(g)
}



arcs = matrix(
  c("a","b","b","c","a","d",'c','d'),ncol=2, byrow=TRUE,
dimnames= list(c(),c("from","to")))

g = graph(nodes)
g = setarcs(g,arcs)
plot.mygraph(g)

lnodes = c('f','g')

g1 = addlatentnodes(g,lnodes)
g2 = deletearc(g,from='a',to = 'b')
