# 判断url是否存在
url.exists(url="www.baidu.com") # 判断url是否存在
# [1] TRUE
d <- debugGatherer() #收集调试信息
# verbose = TRUE 这时候，d$value()值是会叠加的
tmp <- getURL(url="www.baidu.com", debugfunction = d$update, verbose = TRUE)  

names(d$value())

cat(d$value()[1]) #服务器地址及端口号
cat(d$value()[2]) #服务器返回的头信息
cat(d$value()[3]) #提交给服务器的头信息
d$reset() # 清除d$value()
d$value() # 清除之后全部为空
# 查看服务器返回的头信息
## 列表形式
h <- basicHeaderGatherer()
txt <- getURL(url="http://www.baidu.com", headerfunction = h$update)
names(h$value())
h$value()

# 查看服务器返回的头信息
## 字符串形式
h <- basicTextGatherer()
txt <- getURL("http://www.baidu.com", headerfunction = h$update)
names(h$value())
# NULL # 说明是字符串形式，没有列
h$value() # 所有的内容只是一个字符串
cat(h$value()) # 用cat显示的，会比较好看
# 查看url请求的访问信息
curl <- getCurlHandle()

txt <- getURL(url="http://www.baidu.com", curl = curl)
names(getCurlInfo(curl))
getCurlInfo(curl)$response.code

getCurlInfo(curl=curl)


# 设置自己的header，把系统设置成ihpone的系统Mac OS
myheader <- c(
  "User-Agent"="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

d <- debugGatherer()
tmp <- getURL(url = "http://www.baidu.com", httpheader = myheader, debugfunction = d$update, verbose = T)

cat(d$value()[3]) # 提交给服务器的头信息，发现设置成功

# 设置其他参数，共174个参数
listCurlOptions()
# getForm()函数

# 在百度里面搜索“rcurl”的url为（浏览器为google chrome）：
url <- c("http://www.baidu.com/s?ie=utf-8&f=8&rsv_bp=1&rsv_idx=2&ch=&tn=SE_hldp02870_0v135xhf&bar=&wd=rcurl&rsv_spt=1&rsv_pq=a3ed162a0088df8f&rsv_t=43d18gWNyd6HWpqDiKov7Dm548s4HY4cgcJlXc8ujpzRW9Okec2aOb5screzftZo5DJ60Cp7aILvRK2Q&rsv_enter=1&inputT=2119")
# wd=rcurl 这里就是关键字为rcurl

getFormParams(query=url) # 查看url的结构和值
names(getFormParams(query=url))
tmp <- getForm(uri="http://www.baidu.com/s", ie="utf-8", f="8", rsv_bp="1", rsv_idx="2", ch="", tn="SE_hldp02870_0v135xhf", bar="", wd="rcurl", rsv_spt="1", rsv_pq="a3ed162a0088df8f", rsv_t="43d18gWNyd6HWpqDiKov7Dm548s4HY4cgcJlXc8ujpzRW9Okec2aOb5screzftZo5DJ60Cp7aILvRK2Q", rsv_enter="1", inputT="2119")

# 这里的getForm函数不稳定(原因还不知道)，有时候运行2到3次，才能真正找到页面

# getBinaryURL() 下载一个文件
url <- "http://rfunction.com/code/1201/120103.R"
tmp <- getBinaryURL(url)
note <- file("120103.R", open = "wb")
writeBin(tmp, note)
close(note)

# getBinaryURL() 批量下载文件
url <- "http://rfunction.com/code/1202/"
tmp <- RCurl::getURL(url, httpheader = myheader) # 获取网页

tmp_files <- strsplit(x=tmp, split="<li><a href=\"")[[1]]
tmp_files1 <- strsplit(tmp_files, split="\"")
tmp_files2 <- lapply(X=tmp_files1, function(file) {file[1]})
files <- unlist(tmp_files2)
files <- files[c(-1, -2)]

baseURL <- "http://rfunction.com/code/1202/"
for(i in 1:length(files)){
  fullURL <- paste(baseURL, files[i], sep = "")
  tmp <- getBinaryURL(fullURL)
  note <- file(paste("1202-", files[i], sep = ""), open = "wb")
  writeBin(tmp, note)
  close(note)
  
  Sys.sleep(2) # 休眠2秒
}