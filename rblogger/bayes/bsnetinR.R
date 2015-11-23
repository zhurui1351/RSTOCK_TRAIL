#http://www.bnlearn.com/book-useR/
library(bnlearn)
data(marks)
str(marks)
#初始建立一个空图
ug = empty.graph(names(marks))
#加入边，如果是无向的话 则两个方向都加入
arcs(ug, ignore.cycles = TRUE) = matrix(
   c("MECH","VECT","MECH","ALG","VECT","MECH",
      "VECT","ALG","ALG", "MECH","ALG", "VECT",
       "ALG", "ANL","ALG", "STAT","ANL", "ALG",
       "ANL", "STAT","STAT","ALG","STAT","ANL"),
  ncol=2, byrow=TRUE,
 dimnames= list(c(),c("from","to")))
ug

dag = empty.graph(names(marks))
arcs(dag) = matrix(
   c("VECT","MECH","ALG","MECH","ALG","VECT",
       "ANL","ALG","STAT","ALG","STAT","ANL"),
   ncol=2, byrow=TRUE,
   dimnames= list(c(),c("from","to")))
dag

#从链接矩阵构造贝叶斯网
mat = matrix(c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
              nrow = 5,
              dimnames = list(nodes(dag), nodes(dag)))
mat
dag2 = empty.graph(nodes(dag))
amat(dag2) = mat
all.equal(dag, dag2)

#修改网络结构
#添加边
dag3 = empty.graph(nodes(dag))
dag3 = set.arc(dag3, "VECT", "MECH")
dag3 = set.arc(dag3, "ALG", "MECH")
dag3 = set.arc(dag3, "ALG", "VECT")
dag3 = set.arc(dag3, "ANL", "ALG")
dag3 = set.arc(dag3, "STAT", "ALG")
dag3 = set.arc(dag3, "STAT", "ANL")
all.equal(dag, dag3)

all.equal(ug, moral(dag))
#拓扑排序
node.ordering(dag)
#邻节点
nbr(dag, "ANL")
#马尔科夫毯,马尔科夫毯在贝叶斯网络中的表现形式是该特征（即该结点）的父结点、子结点以及子结点的父结点
mb(dag, "ANL")
chld = children(dag, "VECT")
par = parents(dag, "VECT")
o.par = sapply(chld, parents, x = dag)
unique(c(chld, par, o.par[o.par != "VECT"]))

"ANL" %in% mb(dag, "ALG")

score(dag, data = marks, type = "loglik-g")
dag.eq = reverse.arc(dag, "STAT", "ANL")
score(dag.eq, data = marks, type = "loglik-g")

vstructs(dag)
vstructs(dag, moral = F)
all.equal(cpdag(dag), cpdag(dag.eq))
all.equal(moral(dag), moral(dag.eq))

dag2 = drop.arc(dag, from = "STAT", to = "ANL")
dag3 = drop.arc(dag, from = "ALG", to = "VECT")

vstructs(dag2)
vstructs(dag3)

#使用deal包
library(deal)
deal.net = network(marks)
deal.net
m = paste("[MECH][VECT|MECH][ALG|MECH:VECT]",
           "[ANL|ALG][STAT|ALG:ANL]", sep = "")
deal.net = as.network(m, deal.net)
deal.net

library(catnet)
cat.net = cnCatnetFromEdges(names(marks),
                            list(MECH = NULL, VECT = "MECH",
                                   ALG = c("MECH", "VECT"), ANL = "ALG",
                                    STAT = c("ALG", "ANL")))
chld = cnEdges(cat.net)$VECT
chld = cnEdges(cat.net)$VECT
o.par = sapply(chld,
               function(node){ cnEdges(cat.net)[[node]]})
unique(unlist(c(chld, par, o.par[o.par != "VECT"])))
em = empty.graph(names(marks))
arcs(em) = cnMatEdges(cat.net)
em = model2network(deal::modelstring(deal.net))


#绘制网络结构
hl2 = list(arcs = vstructs(dag2, arcs = TRUE),
            lwd = 4, col = "black")
hl3 = list(arcs = vstructs(dag3, arcs = TRUE),
              lwd = 4, col = "black")
graphviz.plot(dag2, highlight = hl2, layout = "fdp",
                 main="dag2")
graphviz.plot(dag3, highlight = hl3, layout = "fdp",
                 main="dag3")
graphviz.plot(cpdag(dag2), highlight = hl2,
                 layout= "fdp",main="cpdag(dag2)")
graphviz.plot(cpdag(dag3), highlight = hl3,
                 layout= "fdp",main="cpdag(dag3)")


#结构学习
#grow-shrink 结构学习
bn.gs = cextend(gs(marks,undirected=F))
bn.gs
plot(bn.gs)
all.equal(bn.gs, iamb(marks))
all.equal(bn.gs, inter.iamb(marks))
all.equal(bn.gs, iamb(marks, test = "mc-cor"))

#爬山法
bn.hc = hc(marks)
bn.hc
plot(bn.hc)

score(bn.gs, data = marks, type = "bic-g")
score(bn.hc, data = marks, type = "bic-g")

#参数学习
#连续变量，得到回归系数
fitted = bn.fit(bn.gs, data = marks)
fitted$ALG
#修改相关条件概率
fitted$ALG = list(coef = c("(Intercept)" = 25,"ANL" = 0.5, "STAT" = 0.25), sd = 6.5)
fitted$ALG
#从头构造一个bn.fit对象
MECH.par = list(coef = c("(Intercept)" = -10,
                         "VECT" = 0.5, "ALG" = 0.6), sd = 13)
VECT.par = list(coef = c("(Intercept)" = 10,"ALG" = 1), sd = 10)
ALG.par = list(coef = c("(Intercept)" = 25,
                         "ANL" = 0.5, "STAT" = 0.25), sd = 6.5)
 ANL.par = list(coef = c("(Intercept)" = 25,
                           "STAT" = 0.5), sd = 12)
STAT.par = list(coef = c("(Intercept)" = 43),
                   sd = 17)
 dist = list(MECH = MECH.par, VECT = VECT.par,
               ALG = ALG.par, ANL = ANL.par,
               STAT = STAT.par)
 fitted2 = custom.fit(bn.gs, dist = dist)
#离散化
 dmarks = discretize(marks, breaks = 2,method = "interval")
 bn.dgs = gs(dmarks)
 bn.dhc = hc(dmarks)
 all.equal(cpdag(bn.dgs), cpdag(bn.dhc))
 fitted = bn.fit(bn.dhc, data = dmarks)
 fitted$ALG
 
 
 #Pearl’s Causality
 #含隐藏变量下对因果网有影响
 latent = factor(c(rep("A", 44), "B",
                    rep("A", 7), rep("B", 36)))
  bn.A = hc(marks[latent == "A", ])
  bn.B = hc(marks[latent == "B", ])
  modelstring(bn.A)
  modelstring(bn.B)
  bn.LAT = hc(cbind(dmarks, LAT = latent))
  bn.LAT
  