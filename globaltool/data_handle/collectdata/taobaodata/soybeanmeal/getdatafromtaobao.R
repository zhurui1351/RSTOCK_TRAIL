source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/collectdata/taobaodata/taobao_data_handle.R',encoding='utf8')
getsoybeanmeal_taobao = function()
{
  pattern = 'DLMMI*'
  pricedata = collectdatafromtaobao(pattern)
  path = 'D:/data/collectdata/taobaodata/commidity/soybeanmeal/'
  write.csv(as.data.frame(pricedata),file = paste(path,'doubo_1m_taobao.csv',sep=''),row.names=T,quote=F )
}
