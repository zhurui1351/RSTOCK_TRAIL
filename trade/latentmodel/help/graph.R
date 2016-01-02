#初始化图，第一次初始化后都是显变量的节点
require('bnlearn')
require(igraph)
require(poLCA)
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
  nodes = c(g$lnodenames,g$mnodenames,g$classnode)
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
  nodes = c(g$lnodenames,g$mnodenames,g$classnode)
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
  if(any(nodes %in% c(g$lnodenames,g$mnodenames,g$classnode))) 
  {
    warning('the nodes have repeated names')
    return(g)
  }
  g$lnodenames = c(g$lnodenames,nodes)
  #g$nodes = c(g$nodes,nodes)
  for( n in nodes)
  {
    g$nodes[[n]] = list()
    g$nodes[[n]]$children = character(0)
    g$nodes[[n]]$parents = character(0)
    g$nodes[[n]]$nbr = character(0)
  }
  return(g)
}

hasnode = function(g,nodename)
{
  if(nodename %in% c(g$mnodenames, g$lnodenames,g$classnode))
    return(T)
  return(F)
}

assignClassNode = function(g,classnode)
{
  if(hasnode(g,classnode))
  {
    g$classnode = classnode
  }
  else
  {
    g$classnode = classnode
    g$nodes[[classnode]] = list()
    g$nodes[[classnode]]$children = character(0)
    g$nodes[[classnode]]$parents = character(0)
    g$nodes[[classnode]]$nbr = character(0)
  }
  return(g)
}

haschild = function(g,pnode,chil)
{
  if(!hasnode(g,pnode))
  {
    warning('no pnode in graph')
    return(F)
  }
  if(is.element(chil,g$nodes[[pnode]]$children))
    return(T)
  return(F)
}

childnodes = function(g,node)
{
  if(!hasnode(g,node))
  {
    warning('no node in graph')
    return(NULL)
  }
  return(g$nodes[[node]]$child)
}

parentnodes = function(g,node)
{
  if(!hasnode(g,node))
  {
    warning('no node in graph')
    return(NULL)
  }
  return(g$nodes[[node]]$parents)
}

newlnodename = function(g)
{
  allnames = c(g$mnodenames,g$lnodenames,g$classnode)
  name = 'temp'
  i = 0;
  lname = paste(name,i,sep='')
  while(is.element(lname,allnames))
  {
    i = i + 1;
    lname = paste(name,i,sep='')
  }
  return(lname)
}

parent_introduction = function(g,pnode,chil1,chil2)
{
  if(!(hasnode(g,pnode) && hasnode(g,chil1) && hasnode(g,chil2)))
  {
    warning('these nodes are not in nodes of g')
    return(g)
  }
  if(!(haschild(g,pnode,chil1) && haschild(g,pnode,chil2)))
  {
    warning('no child in the parent')
    return(g)
  }
  
  lname = newlnodename(g)
  g = deletearc(g,from=pnode,to=chil1)
  g = deletearc(g,from = pnode,to=chil2)
  g = addlatentnodes(g,lname)
  arcs = matrix(c(pnode,lname,lname,chil1,lname,chil2),ncol=2,byrow=T,dimnames= list(c(),c("from","to")))
  g = setarcs(g,arcs)
}

parent_alteration = function(g,pnode,chil,newp)
{
  if(!(hasnode(g,pnode) && hasnode(g,chil) && hasnode(g,newp)))
  {
    warning('these nodes are not in nodes of g')
    return(g)
  }
  if(!(haschild(g,pnode,chil)))
  {
    warning('no child in the parent')
    return(g)
  }
  
  g = deletearc(g,from=pnode,to=chil)
  g = setarc(g,from = newp,to = chil)
  
  return(g)
}

node_deletion = function(g,node)
{
  if(!(hasnode(g,node) ))
  {
    warning('these nodes are not in nodes of g')
    return(g)
  }
  
  parents = parentnodes(g,node)
  childs = childnodes(g,node)
  
  if(is.null(parents))
  {
    if(!is.null(childs))
    {
      for(ch in childs)
      {
        g = deletearc(g,from=node,to = ch)
      }
      return(g)
    }
  }
  else
  {
    if(is.null(childs))
    {
      for(p in parents)
      {
        g = deletearc(g,from=p,to=node)
      }
      return(g)
      
    }
    else
    {
      for(p in parents)
      {
        for(ch in childs)
        {
          g = deletearc(g,from=node,to = ch)
          g = setarc(g,from=p,to = ch)
        }
      }
      return(g)
      
    }
  }
  
}

initNBgraph = function(g)
{
  if(is.null(g$classnode))
  {
    warning('no class node')
    return(g)
  }
  for(m in g$mnodenames)
  {
    g = setarc(g,from=g$classnode,to=m)
  }
  return(g)
}

testgraph = function()
{
  arcs = matrix(
    c("a","b","b","c","a","d",'c','d'),ncol=2, byrow=TRUE,
    dimnames= list(c(),c("from","to")))
  
  g = graph(nodes)
  g = setarcs(g,arcs)
  plot.mygraph(g)
  
  lnodes = c('f','g')
  
  g1 = addlatentnodes(g,lnodes)
  g1 = setarc(g,'f','g')
  plot.mygraph(g1)
  
  g2 = deletearc(g1,from='a',to = 'b')
  plot.mygraph(g2)
  
  g3 = parent_introduction(g,'a','b','d')
  plot.mygraph(g3)
  
  g4 = node_deletion(g3,'b')
  plot.mygraph(g4)
  
}

as.graph.bn = function(g)
{
  nodes = c(g$lnodenames,g$mnodenames,g$classnode)
  ug = empty.graph(nodes)
  arcs(ug, ignore.cycles = TRUE) = g$arcs
  return(ug)
}

getallnodenames = function(g)
{
  return(c(g$mnodenames,g$lnodenames,g$classnode))
}

# 给定起点A，子节点B，形成A为根，B以及B的所有后代的子图
subgraph = function(g,classnode,subnode)
{
  allnodes = getallnodenames(g)
  
  from =g$arcs[,'from']
  to = g$arcs[,'to']
  
  subtreenodes = unique(subset(g$arcs,from == subnode,select='to'))
  leftnodes = c(classnode,subnode,subtreenodes)
  
  leftarcs = subset(g$arcs,from %in% leftnodes & to %in% leftnodes)
  leftmnodes = intersect(c(subnode,subtreenodes),g$mnodenames)
  leftlnodes = intersect(c(subnode,subtreenodes),g$lnodenames)
  
  subg = graph(leftnodes)
  subg = setarcs(subg,leftarcs)
  subg$mnodenames = leftmnodes
  subg$lnodenames = leftlnodes
  subg$classnode = classnode
 # plot.mygraph(subg)
  return(subg)
}