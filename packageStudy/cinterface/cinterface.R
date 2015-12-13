# useC1.c
# void useC(int *i, int *j) {
#   j[0] = i[0]+10;
# }

# void useC2(int *i, double *d, char **c, int *l) {
#   i[0] = 11;
#   d[0] = 2.333;
#   c[1] = "g";
#   l[0] = 0;
# }
# install Rtools
#R CMD SHLIB userC1.c
dyn.load('d:/userC1.dll')
out=.C("useC", a=as.integer(c(1,2,3)), b=as.integer(c(0,2,3))) 
out

i <- 1:10                               # integer vector
d <- seq(length=3, from=1, to=2)        # real number vector
c <- c("a", "b", "c")                   # string vector
l <- c("TRUE", "FALSE")                 # logical vector
out <- .C("useC2",
            i1 = as.integer(i),
            d1 = as.numeric(d),
            c1 = as.character(c),
            l1 = as.logical(l))
out
dyn.unload("d:/userC1.dll")
