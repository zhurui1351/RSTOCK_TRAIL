from scrapy.spider import BaseSpider
from scrapy.selector import HtmlXPathSelector
from scrapy.http import Request
class DmozSpider(BaseSpider):
    name = "usce"
    allowed_domains = ["bls.gov"]
    start_urls = [
        "http://download.bls.gov/pub/time.series/ce/"
    ]

    def parse(self, response):
        hxs = HtmlXPathSelector(response)
        links = hxs.select('//a/@href').extract()
        follows = []
        #filename = response.url.split("/")[-2]
        #open(filename, 'wb').write(response.body)
        print("ok")
        #print(items[1:])
        url = response.url
        for link in links[1:]:
        #link = "http://download.bls.gov/pub/time.series/ce/ce.period"
            truelink = url + link.split('/')[-1]
            follows.append(Request(truelink,callback=self.parsesuburl))
        return(follows)

    def parsesuburl(self,response):
        print("now i am in")
        filename = response.url.split("/")[-1]
        open(filename, 'wb').write(response.body)
        #print(response.url)
