groupClac <- function(line){
  initial <- c(0,0)
  s <- table(line)
  initial[as.integer(names(s))] <- as.integer(s)
  initial
}

ivCalc <- function(inputData, numberOfBin){
  inputData <- data.frame(feature = inputData[,1],
                          flag = inputData[,2])
  
  binGap <- (max(inputData[,1])-min(inputData[,1]))/numberOfBin
  inputData[,1] <- as.integer((inputData[,1]-min(inputData[,1]))/binGap)
  
  
  totalBin <- length(unique(inputData[,1]))
  
  sumValue <- as.integer(table(inputData[,2]))
  
  groupDis <- aggregate(flag~., data=inputData, groupClac)
  groupDis <- as.matrix(groupDis[,2])
  groupDisNor <- scale(groupDis, center=FALSE, scale=colSums(groupDis))
  groupDisNor <- groupDisNor[which(groupDisNor[,1]*groupDisNor[,2]!=0), ]
  
  return(sum((groupDisNor[,1]-groupDisNor[,2])*log(groupDisNor[,1]/groupDisNor[,2])))  
  
}

#杩欓噷鐨勫垎绫绘爣绛句负1锛?2
myData <- iris
myData$Species <- as.integer(myData$Species)
myData <- myData[myData$Species %in% c(1,2), ]

allIVs <- c()
for (loc in 1:(ncol(myData)-1)){
  inputData <- myData[,c(loc, ncol(myData))]
  IV <- ivCalc(inputData,10)
  allIVs <- c(allIVs, IV)
  print(sprintf("Column %d  IV %f", loc, IV))
}













