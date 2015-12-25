library(bnlearn)
library(gRain)

#构建网络
yn <- c("yes", "no")

b = cptable(~B,values = c(1,99),levels = yn)
e = cptable(~E,values = c(2,98),levels = yn)

a = cptable(~ A | B + E,values =c(95,5,94,6,29,71,0.1,99.9),levels = yn)

ma = cptable(~ M | A,values=c(9,1,5,95),levels = yn)
ja = cptable(~ J | A,values=c(7,3,1,99),levels = yn)

plist <- compileCPT(list(b,e,a,ma,ja),details=10)
gin1 <- grain(plist)
summary(gin1)
plot(gin1)
bn1 = as.bn(gin1)
bn.fit1 = as.bn.fit(gin1)

#查询
jtree = compile(gin1)
gin_e1 <- setFinding(jtree, nodes=c("M"), states=c("yes"))
querygrain(gin_e1, nodes = "B", type = "marginal") 
querygrain(gin1, nodes = "B", type = "marginal") 

