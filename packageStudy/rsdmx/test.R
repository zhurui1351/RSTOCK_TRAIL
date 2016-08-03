#https://github.com/opensdmx/rsdmx/blob/master/vignettes/quickstart.Rmd#using-the-helper-approach
require(rsdmx)
#所有数据列表
url <- 'http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/all' 
xml_data <- readSDMX(url)
data <- as.data.frame(xml_data)
#可用数据库
providers <- getSDMXServiceProviders()
as.data.frame(providers)

sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                 key = list("TOT", NULL, NULL), start = 2010, end = 2011)
df <- as.data.frame(sdmx)
head(df)

#获取数据
url <- 'http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/AUS+AUT.GDP+B1_GE.CUR+VOBARSA.Q/all?startTime=2009-Q2&endTime=2011-Q4' 
xml_data <- readSDMX(url)
data <- as.data.frame(xml_data)
sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                 key = list("TOT", NULL, NULL), start = 2010, end = 2011,
                 dsd = TRUE)
df <- as.data.frame(sdmx, labels = TRUE)
head(df)


#data without DSD
sdmx.data <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                      key = list("TOT", NULL, NULL), start = 2010, end = 2011)

#DSD
sdmx.dsd <- readSDMX(providerId = "OECD", resource = "datastructure", resourceId = "MIG")

#associate data and dsd
sdmx.data <- setDSD(sdmx.data, sdmx.dsd)


tf <- tempfile(tmpdir = tdir <- tempdir()) #temp file and folder
download.file("http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Frd_e_gerdsc.sdmx.zip", tf)
sdmx_files <- unzip(tf, exdir = tdir)

#read local SDMX (set isURL = FALSE)
sdmx <- readSDMX(sdmx_files[2], isURL = FALSE)
stats <- as.data.frame(sdmx)


#concept
csUrl <- "http://data.fao.org/sdmx/registry/conceptscheme/FAO/ALL/LATEST/?detail=full&references=none&version=2.1"
csobj <- readSDMX(csUrl)
csdf <- as.data.frame(csobj)
#codelist
clUrl <- "http://data.fao.org/sdmx/registry/codelist/FAO/CL_FAO_MAJOR_AREA/0.1"
clobj <- readSDMX(clUrl)
cldf <- as.data.frame(clobj)
#dsd
dsdUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE1"
dsd <- readSDMX(dsdUrl)

#get codelists from DSD
cls <- slot(dsd, "codelists")

#get list of codelists
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))

#get a codelist
codelist <- as.data.frame(slot(dsd, "codelists"), codelistId = "CL_TABLE1_FLOWS") 

#get concepts from DSD
concepts <- as.data.frame(slot(dsd, "concepts"))