require(data.table)
path = '/rstudio/zhurui/name_sex.txt'
tb=fread(path,sep='\t',verbose = T,colClasses=c('character','character') ,na.strings=c("NA","NULL"),col.names=c('name','sex'))
#tb_sum[lastname =='策',]
#tb[substr(name,2,3)=='家强',]
###总的男女比例
boycount = sum(tb[,sex=='b'],na.rm = T)
girlcount = sum(tb[,sex=='g'],na.rm = T)

boyratio = boycount/(boycount+girlcount)
girlratio = girlcount / (boycount+girlcount)

#w2 尾字条件概率表
tb$lastname = substring(tb[,name],nchar(tb[,name]))
tb_sum_w2 = tb[,list(b=length(sex[sex=='b']),bratio = ((length(sex[sex=='b']) + 1)/(boycount+2)),g=length(sex[sex=='g']),gratio = ((length(sex[sex=='g'])+1)/(girlcount+2))),by=list(lastname)]

#w1 中间字条件概率表
tb_w1_1 = tb[nchar(name) ==3,]
tb_w1_1$midname = substr(tb_w1_1[,name],2,2)
tb_w1_2 = tb[nchar(name) ==4,]
tb_w1_2$midname = substr(tb_w1_2[,name],3,3)

tb_w1 = rbind(tb_w1_1,tb_w1_2)
tb_sum_w1 = tb_w1[,list(b=length(sex[sex=='b']),bratio = ((length(sex[sex=='b']) + 1)/(boycount+2)),g=length(sex[sex=='g']),gratio = ((length(sex[sex=='g'])+1)/(girlcount+2))),by=list(midname)]

# w1w2 中间+尾字
tb_w12_1 = tb[nchar(name) ==3,]
tb_w12_1$midname = substr(tb_w1_1[,name],2,3)
tb_w12_2 = tb[nchar(name) ==4,]
tb_w12_2$midname = substr(tb_w1_2[,name],3,4)

tb_w12 = rbind(tb_w12_1,tb_w12_2)
tb_sum_w12 = tb_w12[,list(b=length(sex[sex=='b']),bratio = ((length(sex[sex=='b']) + 1)/(boycount+2)),g=length(sex[sex=='g']),gratio = ((length(sex[sex=='g'])+1)/(girlcount+2))),by=list(midname)]


predict_sex = function(name,threshold = 0.5,raw = F)
{
  w2 = substring(name,nchar(name))
  if(nchar(name) == 3)
  {
    w1 = substr(name,2,2)
  }else if(nchar(name) == 4)
  {
    w1 = substr(name,3,3)
  } else
  {
    w1 = ''
  }
  if(w1 != '')
  {
    w12 = paste(w1,w2,sep='')
  }else
  {
    w12 = ''
  }
  
  p1 = tb_sum_w1[midname == w1,]
  p2 = tb_sum_w2[lastname == w2,]
  p12 = tb_sum_w12[midname == w12,]
  
  p1_b = ifelse(nrow(p1) == 0,1,p1[,bratio])
  p1_g = ifelse(nrow(p1) == 0,1,p1[,gratio])
  
  p2_b = ifelse(nrow(p2) == 0,1,p2[,bratio])
  p2_g = ifelse(nrow(p2) == 0,1,p2[,gratio])
  
  p12_b = ifelse(nrow(p12) == 0,1,p12[,bratio])
  p12_g = ifelse(nrow(p12) == 0,1,p12[,gratio])
  
  p_g = p1_g * p2_g * p12_g * girlratio
  p_b = p1_b * p2_b * p12_b * boyratio
  #归一化
  psum = p_g + p_b
  p_g = p_g / psum
  p_b = p_b / psum
  
  if(raw == T)
    return(list(girl = p_g,boy = p_b))
  else
  {
    if(p_g > threshold )
    {
      return('girl')
    }
    else if(p_b > threshold)
    {
      return('boy')
    }
    else
    {
      return('unkonw')
    }
  }
}

require(compiler)
predict_sex = cmpfun(predict_sex)
suppressWarnings(predict_sex('王艳萍',raw = T))

for ( i in 1 : 5)
{
  index = sample(1:nrow(tb),200000)
  print(Sys.time())
  pr1 = sapply(tb[index,name], predict_sex)
  print(Sys.time())
  tr1 = tb[index,sex]
  pr11 = substr(pr1,1,1)
  print(sum(pr11 == tr1) / length(tr1)) 
}
#保存概率表
write.table(tb_sum_w1[,list(midname,bratio,gratio)],'w1.txt',sep=',',quote=F,row.names = F,col.names = F)
write.table(tb_sum_w2[,list(lastname,bratio,gratio)],'w2.txt',sep=',',quote=F,row.names = F,col.names = F)
write.table(tb_sum_w12[,list(midname,bratio,gratio)],'w12.txt',sep=',',quote=F,row.names = F,col.names = F)

#计算auc
path = '/rstudio/zhurui/sex_text.txt'
tb=fread(path,sep='\t',verbose = T,colClasses=c('character','character','numeric') ,na.strings=c("NA","NULL"),col.names=c('name','gender','ratio'))
pred = prediction(tb$ratio,tb$gender)
perf <- performance(pred,'auc')

