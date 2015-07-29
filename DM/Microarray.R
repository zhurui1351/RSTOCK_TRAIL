#生物挖掘项目http://bioconductor.org/
source("http://bioconductor.org/biocLite.R")
require('BiocInstaller')
biocLite()
biocLite("ALL")

library(Biobase)
library(ALL)
data(ALL)
