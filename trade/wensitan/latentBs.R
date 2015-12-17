require(igraph)
graph = function(nodes)
{
  numnodes = length(nodes)
  g = list()
  g$nodes = list()
  
  g$nodenames = nodes
  
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

setarcs = function(g,arcs)
{
  for(i in 1 : nrow(arcs))
  {
    g = setarc(g,arcs[i,]['from'],arcs[i,]['to'])
  }
  return(g)
}

rootnodes = function(g)
{
  
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

arcs = matrix(
  c("a","b","b","c","a","d",'c','d'),ncol=2, byrow=TRUE,
dimnames= list(c(),c("from","to")))

g = graph(nodes)
g = setarcs(g,arcs)
plot.mygraph(g)
