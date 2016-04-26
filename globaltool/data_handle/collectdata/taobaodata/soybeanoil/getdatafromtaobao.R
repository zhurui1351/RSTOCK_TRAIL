source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/collectdata/taobaodata/taobao_data_handle.R',encoding='utf8')
getsoybeanoil_taobao = function()
{
  pattern = 'DLYMI*'
  pricedata = collectdatafromtaobao(pattern)
  path = 'D:/data/collectdata/taobaodata/commidity/soybeanoil/'
  write.csv(as.data.frame(pricedata),file = paste(path,'douyou_1m_taobao.csv',sep=''),row.names=T,quote=F )
}