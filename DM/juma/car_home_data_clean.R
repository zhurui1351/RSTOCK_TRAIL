require(rjson)
path = 'D:/car_home2.txt'
content = readLines(path)

all_names = c("变速箱","倒档数","换挡方式","前进档位","弹簧片数","后桥描述","后桥速比","后桥允许载荷","悬挂形式","额定转速","发动机","技术路线","扭矩","排放标准","排量","汽缸排列形式","汽缸数","燃料种类","最大马力","最大扭矩转速","最大输出功率","地方排放标准","车身高度","车身宽度","车身长度","吨位级别","公告型号","轮距","牵引总质量","驱动形式","整车重量","轴距","总质量","最高车速","最小转弯直径","name","车名","品牌","车系","厂商指导价","驾驶室","卧铺尺寸","准乘人数","座位排数","轮胎规格","轮胎数","油箱气罐材质","油箱气罐容量")

result = data.frame()
for(i in 1 : length(content))
{
  print(i)
  con = content[i]
  json_data <- fromJSON(content,unexpected.escape='keep',method='R')
  x = unlist(json_data)
  n_x = names(x)
  xx = sapply(n_x, function(x1){
    m = strsplit(x1,'.',fixed = T)
    m = m[[1]][length(m[[1]])]
    return(m[[1]][length(m[[1]])])
  })
  xx = unlist(xx)
  xx = gsub('[：/ ]','',xx)
  names(xx) = NULL
  names(x) = xx
  
  l = list()
  for(j in 1:length(all_names))
  {
     k = all_names[j]
     v = x[k]
     names(v) = NULL
     
     l1 = list(v)
     names(l1) = k
     l = append(l,l1)
  }
  r = as.data.frame(l)
  result = rbind(result,r)
}


#xxx = data.frame(标题=xx,值 = x)
#write.csv(xxx,file='d:/1.csv',row.names = F)
