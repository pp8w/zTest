library(dplyr)
library(tidyr)

#----------Initial Variables

setwd("~/_Dev/mData")
csvlist = c(
  "1952.csv",
  "ageofadaline.csv",
  "almanac.csv",
  "alvin4.csv",
  "antman.csv"
)

#----------File Loop

fileloop <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  it2 = as.numeric(gsub( ',', '', item$cGross))
  
  #generating the curves
  cMax = max(it2)
  gRatio = it2 / cMax
  dCurve = 1-gRatio
  
  #cleaning name for output
  name = gsub('.csv','',csvlist)
  
  #return value
  output = cbind(name, item$week, dCurve)
 
}

#-------Executing The Loop

list <- lapply(csvlist, fileloop)

#Converting from a list to dataframe
df <- do.call(rbind, lapply(list, data.frame, stringsAsFactors=FALSE))


#Changing characters to numeric values
#as.numeric(levels(f)[f]) is the preferred apparently
df$dCurve <- as.numeric(as.character(df$dCurve))
df$V2 <- as.numeric(as.character(df$V2))

#converting to table
tbl <- tbl_df(df)



#------Visualization




