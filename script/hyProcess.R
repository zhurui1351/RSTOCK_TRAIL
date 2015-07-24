## grep -r 'T110101' ./
#R CMD BATCH hyProcess.R

path = 'C:/new_zx_allin1/T0002/hq_cache'
file_hy = 'tdxhy.cfg'
hy = file.path(path,file_hy)
hy = read.table(hy,head=F,sep='|', colClasses=rep('character',4))
hy = hy[,c(2,3)]
names(hy) = c('stockcode','hy')

file_hy_code = 'tdxzs.cfg'
hy_code = file.path(path,file_hy_code)
hy_code = read.table(hy_code,head=F,sep='|',colClasses=rep('character',6))
hy_code = hy_code[,c(1,2,6)]
names(hy_code) = c('hyname','hycode','hy')


stockandhy = merge(hy,hy_code,by='hy')

path = 'D:/data/stock/code'
name = 'hycode.txt'
write.table(stockandhy,file.path(path,name),sep=',',quote=F)



