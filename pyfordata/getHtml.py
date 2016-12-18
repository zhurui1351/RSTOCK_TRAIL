# coding = utf-8
import requests                 #用来抓取网页的html源代码
import csv                      #将数据写入到csv文件中
import random                   #取随机数
import time                     #时间相关操作
import socket                   #在这里只用于异常处理
import http.client              #在这里只用于异常处理
from bs4 import BeautifulSoup  #用来代替正则式取源码中相应标签中的内容

def get_content(url , data = None):
    header={
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Accept-Encoding': 'gzip, deflate, sdch',
        'Accept-Language': 'zh-CN,zh;q=0.8',
        'Cache-Control': 'max-age = 0',
        'Connection': 'keep-alive',
        'Cookie': 'CNZZDATA5147345=cnzz_eid % 3D1794559590-1480925620-%26ntime%3D1480986246',
        'Host': 'www.aj52zx.com',
        'Upgrade-Insecure-Requests':'1',
        'User-Agent':'Mozilla/5.0(Windows NT 6.1) AppleWebKit/537.36(KHTML, like Gecko) Chrome/49.0.2623.75Safari/537.36'
    }
    timeout = random.choice(range(80, 180))
    while True:
        try:
            rep = requests.get(url,headers = header,timeout = timeout)
            rep.encoding = 'utf-8'
            break
        except socket.timeout as e:
            print( '3:', e)
            time.sleep(random.choice(range(8,15)))

        except socket.error as e:
            print( '4:', e)
            time.sleep(random.choice(range(20, 60)))

       # except http.client.BadStatusLine as e:
        #    print( '5:', e)
         #   time.sleep(random.choice(range(30, 80)))
#
#        except http.client.IncompleteRead as e:
#            print( '6:', e)
#            time.sleep(random.choice(range(5, 15)))

    return rep.content
    
def get_page(bs):         #获得总的场次和页面数
    page=bs.find('div', class_='page')
    span=page.find_all('span')
    num = span[0].string
    num = num[:-1]
    totalNum = int(num)
    totalPage = int(span[2].string)
    return totalNum,totalPage
    
def prase_firstPage(html_text):     #解析第一页内容
    final = []
    bs = BeautifulSoup(html_text, "html.parser")  # 创建BeautifulSoup对象
    x,y=get_page(bs)                                  #获得总的场次和页面数
    body = bs.tbody # 获取body部分
    data = body.find_all('tr')  # 找到所有tr
    for onedata in data[1:]:
        temp=[]
        tdAll = onedata.find_all('td')  # 获取td部分
        for td in tdAll:
            string=td.string       #获得每个td的文字内容
            temp.append(string)

        final.append(temp)

    return final,x,y
    

def prase_Page(html_text):      #解析一页的内容
    final = []
    bs = BeautifulSoup(html_text, "html.parser")  # 创建BeautifulSoup对象
    body = bs.tbody # 获取body部分
    data = body.find_all('tr')  # 找到所有tr
    for onedata in data[1:]:
        temp=[]
        tdAll = onedata.find_all('td')  # 获取td部分
        for td in tdAll:
            string=td.string       #获得每个td的文字内容
            temp.append(string)

        final.append(temp)

    return final

def write_data(data, name):   #将数据写入文件
    file_name = name
    with open(file_name, 'w', errors='ignore', newline='') as f:
        f_csv = csv.writer(f)
        f_csv.writerows(data)
        

    
def prase_otherPage(url,pageNum):               #解析除第一页的所有页面
    for i in range (2,pageNum):
        html = [] 
        result = []
        url = url + '?page=' + str(i)
		print url
		#翻页的url
        html = get_content(url)                #新页面的内容
        result = prase_Page(html)              #解析新的页面
        filename = 'data' + str(i)
        filename = filename + '.csv'
        write_data(result, filename)           #将解析结果写入文本 
        
    


if __name__ == '__main__':
    url ='http://www.aj52zx.com/racelist.aspx'
    html = get_content(url)
    result,num,pageNum = prase_firstPage(html)
    write_data(result, 'data1.csv')
    prase_otherPage(url,pageNum)





