#http://baike.baidu.com/link?url=x8PVorK36s2ArEWvIbQGb6WK0U1T72BWmNeARSOAE5k58Rd0nvCOOOG_JK-XEEa3Xb2waXywTFCV7wtsA8cIgq
#返回率
#A,B,C是胜负平赔率
return_rate = function(A,B,C)
{
  return ((A*B*C) / (A*B+A*C+B*C))
}

type_rate = function(A,D)
{
  return(A/D)
}

# a,b,c - 测算的 胜负平 概率
# x,y,z - 胜负平赔率
# m,n,o - 胜负平 投注比例 m+n+o = 1
# 抽水比例 g ,返回率 p
# 胜的情况: mx =  p => m = p/x
# 平的情况: ny = p => n = p/y
# 负的情况：oz  = p => o = p/z
#  p/x + p/y + p/z = 1 = > p =  = xyz/(xy+zy+xz)

