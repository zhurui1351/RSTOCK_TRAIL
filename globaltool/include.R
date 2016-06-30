require('quantmod')
require('TTR')
require('dygraphs')
require('lubridate')
require('dplyr')
require('data.table')
require('e1071')
require("randomForest")
require('rpart')
require('rpart.plot')
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}