require(rjson)
path = 'D:/car_home2.txt'
content = readLines(path)

all_names = c("ABS防抱死",
              "name",
              "pic",
              "百公里油耗",
              "变速箱",
              "产地",
              "产品规格",
              "车名",
              "车桥型号",
              "车桥型式",
              "车身尺寸",
              "车身长度",
              "车体结构",
              "车厢高度",
              "档位数",
              "底盘品牌",
              "底盘型号",
              "地方排放标准",
              "额定功率",
              "额定载重",
              "额定载重量",
              "发动机",
              "发动机形式",
              "功率",
              "货箱尺寸",
              "货箱高度",
              "货箱宽度",
              "货箱形式",
              "货箱长度",
              "机油规格",
              "机油类型",
              "可替换滤清器零件号",
              "可替换滤清器型号",
              "轮胎规格",
              "轮胎数",
              "轮胎系列",
              "排放标准",
              "排量",
              "品牌",
              "品牌车系",
              "上市年份",
              "油泵类型",
              "原产地",
              "长度",
              "整车类型",
              "制动方式",
              "制动力分配(EBDCBC等)",
              "制动形式",
              "中控台彩色大屏",
              "轴数",
              "助力类型",
              "助力形式",
              "最大马力"
)

result = data.frame()

titles = c()
for(i in 1 : length(content))
{
  print(i)
  con = content[i]
  json_data <- fromJSON(con,unexpected.escape='keep',method='R')
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
  titles = c(titles,xx)
  
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
#write.csv(result,file='d:/2.csv',row.names = F)
