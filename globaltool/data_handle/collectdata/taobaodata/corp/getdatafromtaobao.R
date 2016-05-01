source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/collectdata/taobaodata/taobao_data_handle.R',encoding='utf8')
getcorp_taobao = function()
{
  pattern = 'DLCMI*'
  pricedata = collectdatafromtaobao(pattern)
  path = 'D:/data/collectdata/taobaodata/commidity/corp/'
  write.csv(as.data.frame(pricedata),file = paste(path,'yumi_1m_taobao.csv',sep=''),row.names=T,quote=F )
}
