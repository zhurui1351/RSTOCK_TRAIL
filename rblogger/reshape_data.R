#http://r-exercises.com/2016/07/06/data-shape-transformation-with-reshape-solutions/
#http://r-exercises.com/2016/07/06/data-shape-transformation-with-reshape/

data1 <- data.frame(id=c("ID.1", "ID.2", "ID.3"),
                    sample1=c(5.01, 79.40, 80.37),
                    sample2=c(5.12, 81.42, 83.12),
                    sample3=c(8.62, 81.29, 85.92))

#Wide-to-Long
data2 <- reshape(data1, direction="long", varying=2:4, idvar='id', timevar="TIME", v.names="Sample")
#Long-to-Wide
reshape(data2, direction = "wide")
#Time Variables
reshape(data2, timevar = "TIME", idvar = "id", direction = "wide")
#New Row Names:
reshape(data2, direction = "wide", new.row.names = unique(data2$id))
#Convert “data2” to wide format. Set “v.names=” to the “data2” column with observations
reshape(data2, timevar = "TIME", idvar = "id", direction = "wide", v.names = "Sample")
#Set sep = "" in order to reshape “data1” to long format
reshape(data1, direction = "long", varying = 2:4, sep = "")
#Reshape “data2” to “wide“. Use the “direction =” parameter. Setting a new dataframe variable isn’t required
reshape(data2, direction = "wide")
#Use the most basic reshape command possible, in order to reshape“data2” to wide format
reshape(data2)
#Reshape “data2” to “wide“, with column names for the reshaped data of “TIME” and “Sample“
reshape(data2, idvar = c("TIME", "Sample"), timevar = "id", direction = "wide")
#Reshape “data1” by varying “sample1“, “sample2“, and “sample3“.
reshape(data1, direction = "long", varying = list(c("sample1", "sample2", "sample3")))
