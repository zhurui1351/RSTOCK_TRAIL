source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/collectdata/taobaodata/taobao_data_handle.R',encoding='utf8')
getsoybean_taobao = function()
{
  pattern = 'DLAMI*'
  pricedata = collectdatafromtaobao(pattern)
  path = 'D:/data/collectdata/taobaodata/commidity/soybean/'
  write.csv(as.data.frame(pricedata),file = paste(path,'dou1_1m_taobao.csv',sep=''),row.names=T,quote=F )
}


