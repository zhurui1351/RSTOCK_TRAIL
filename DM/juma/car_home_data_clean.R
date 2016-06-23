require(rjson)
path = 'D:/car_home.txt'
json_data <- fromJSON(file=path,unexpected.escape='keep',method='R')


content = readLines(path)
