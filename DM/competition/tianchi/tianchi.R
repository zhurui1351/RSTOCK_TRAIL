require(e1071)
require(data.table)
require(mlogit)
DATA_DIR = "d:/tianchi/"
MALL_ID = 'm_7800'
shop_infos = read.csv(paste(DATA_DIR,"训练数据-ccf_first_round_shop_info.csv",sep=''),stringsAsFactors=F)
shop_infos_1 = subset(shop_infos,mall_id == MALL_ID )

# user_shop_behavior
user_shop_behavior = read.csv(paste(DATA_DIR,"训练数据-ccf_first_round_user_shop_behavior.csv",sep=''),stringsAsFactors=F)
datasets_learn = merge(user_shop_behavior,  shop_infos[c('shop_id', 'mall_id')], on='shop_id')
datasets_learn_1 = subset(datasets_learn,mall_id==MALL_ID)

trainset = datasets_learn_1[1:18000,]
testset = datasets_learn_1[18000:nrow(datasets_learn_1),]

sample_size = nrow(trainset)
wifi_ranks = data.frame()

wifi_list = list()
shot_list = list()

#shop_wifi = data.frame()
shop_wifi = data.table()
for(i in 1:sample_size)
{
  print(i)
  record = trainset[i,]
  
  shop_id = record$shop_id
  
  wifi_info = record$wifi_infos
  wifi_info = strsplit(wifi_info,';')
  

  
  wifi_rank = c()
  result = c()
  
  for(w in wifi_info[[1]])
  {
    w_values = strsplit(w,'|',fixed=T)
    result = c(result,as.numeric(w_values[[1]][2]))
    r = data.table(rowno=i,shop_id,w_values[[1]][1],w_values[[1]][2],w_values[[1]][3])
    shop_wifi = rbind(shop_wifi,r,stringsAsFactors=F,deparse.level = 0)
  }
}  
  
colnames(shop_wifi) = c('rowno','shop_id','wifi_id','strength','connected')

wide_table = dcast(shop_wifi[,1:4], rowno+shop_id ~ wifi_id,drop=FALSE, fill=0,fun=as.numeric)
wide_table$shop_id = as.factor(wide_table$shop_id)

m = mlogit(shop_id~.,data=wide_table[,-1])

#排序
  result_x = order(result,decreasing=T)
  wifi_rank = c()
  for(m in 1:5)
  {
    if(length(result)>5)
    {
      i1 = result_x[m]
      w_id = strsplit(wifi_info[[1]][i1],'|',fixed=T)
      w_id = w_id[[1]][1]
    }
    else
    {
      w_id = NA
    }
    wifi_rank = c(wifi_rank,w_id)  
  }
  wifi_rank = c(shop_id,wifi_rank)
  wifi_ranks = rbind(wifi_ranks,wifi_rank,stringsAsFactors=F,deparse.level = 0)
}



colnames(wifi_ranks) = c('shop_id','rank1','rank2','rank3','rank4','rank5')
model = naiveBayes(shop_id~.,data=wifi_ranks[,1:2])
predict(model,wifi_ranks[,1:2])



all_shopid = unique(wifi_ranks$shop_id)
r = subset(wifi_ranks,shop_id == 's_683053')
x = subset(wifi_ranks,rank1 == 'b_26345723')
y = subset(wifi_ranks,rank1 == 'b_26345723' & shop_id == 's_3284083')

pr = predict(model,wifi_ranks[,-1])

#分析
all_wifi_id = unique(shop_wifi$wifi_id )
shops = unique(shop_wifi$shop_id)



all_infos_shop = shop_wifi[,list(strengh=mean(as.numeric(strength)),freq = .N),by=.(shop_id,wifi_id)]

all_infos_wifi = shop_wifi[,list(strengh=mean(as.numeric(strength)),freq = .N),by=.(wifi_id,shop_id)]

infos_shop = all_infos_shop[,list(count = .N),by=.(shop_id)]
infos_wifi = all_infos_wifi[,list(count = .N),by=.(wifi_id)]

#测试
testrecord = testset[1,]
w_infos = strsplit(testrecord$wifi_infos,';',fixed=T)[[1]]
for(w in w_infos)
{
  info = strsplit(w,'|',fixed=T)[[1]]
  w_id = info[1]
  strength = as.numeric(info[2])
  connected = as.logical(info[3])
  
  all_shop = shop_wifi[wifi_id == w_id]$shop_id
  infos_wifi[wifi_id == w_id]
  
}

