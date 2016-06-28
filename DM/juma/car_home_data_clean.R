require(rjson)
path = 'D:/car_home2.txt'
content = readLines(path)


for(i in 1 : length(content))
{
  print(i)
  con = content[i]
  json_data <- fromJSON(content,unexpected.escape='keep',method='R')
  
}

x = unlist(json_data)
n_x = names(x)
xx = sapply(n_x, function(x1){
  m = strsplit(x1,'.',fixed = T)
  m = m[[1]][length(m[[1]])]
  return(m[[1]][length(m[[1]])])
})
xx = unlist(xx)
names(xx) = NULL
