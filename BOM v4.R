library(plyr) # Conversion
library(dplyr) #Table Ops
library(tidyr) #Table Ops
library(ggplot2) #Visualization
library(chron) #Used for pre 1969 dates

#########################################################
#########################################################
#
#                   HEADER CODE BLOCK                         
#
#########################################################
#########################################################

#--------------------------------------------------------
#             Obtaining information from CSVs
#--------------------------------------------------------

#Obtain list of movies
setwd("~/_Dev/_Lists")
readlist <- read.csv("movieList.csv", header = TRUE, stringsAsFactors = FALSE)
csvlist <- readlist$list

#Read movies from CSV files
setwd("~/_Dev/hData")
readHeader <- function(csvlist) {item = read.csv(csvlist, header = FALSE, 
                                                 stringsAsFactors = FALSE)}
mdf <- lapply(csvlist, readHeader) ##### MAIN LOOP #####
mdf <- ldply (mdf, data.frame) #Converts to Data Frame

#updating column headers
colnames(mdf) = c('id', 'adj', 'Name', 'dGross', 
                  'dist', 'rDate', 'G', 'Runtime',  'Rating', 'pBudget')
mdf <- mdf[ , !names(mdf) %in% c("adj","dist")] #removing unused columns
mdf <- tbl_df(mdf)


#--------------------------------------------------------
#             Cleaning Header Results
#--------------------------------------------------------

#HTML replacements - removing html fragments
mdf <- as.data.frame(sapply(mdf,gsub,pattern="<b>|</b>|<br/>|<i>|</i>|\\$",replacement=""))
mdf <- as.data.frame(sapply(mdf,gsub,pattern=".*date=",replacement="")) #date specific
mdf <- as.data.frame(sapply(mdf,gsub,pattern="&amp;p=.htm\">.*",replacement="")) #date specific
#print(head(mdf))


#Gross- changing from character to numerics
mdf$dGross <- as.numeric(as.character(gsub( ',', '', mdf$dGross)))
mdf$dGross <- mdf$dGross / 1000000 # converting to millions


#Budget - removing millions text then converting to numeric
mdf$pBudget <- as.character(gsub( ' million', '\\*1000000', mdf$pBudget))
mdf$pBudget <- as.character(gsub( 'N/A', '0', mdf$pBudget)) #Sets NA to 0
mdf$pBudget <- as.character(gsub( ',', '', mdf$pBudget))

pExp <- sapply(mdf$pBudget, function(x) eval(parse(text=x))) #expression to value
pExp = pExp / 1000000 # converting to millions
pExp[pExp==0] <- NA #replacing 0 with NA for log
mdf <- cbind(mdf, pExp) #binding to frame
mdf <- mdf[ , !names(mdf) %in% c("pBudget")] #removing old column


#Runtime - Changing from "[X hrs Y min]" to "[Z]", where Z is minutes
#Converting to expression then evaluating the expression
mdf$Runtime <- as.character(gsub( ' min.', '', mdf$Runtime))
mdf$Runtime <- as.character(gsub( ' hrs. ', '\\*60+', mdf$Runtime))
mdf$Runtime <- as.character(gsub( 'N/A', '0', mdf$Runtime))

rtExp <- sapply(mdf$Runtime, function(x) eval(parse(text=x))) #expression to value
mdf$Runtime <- as.numeric(rtExp)


#Simple column conversions
mdf$id <- as.character(mdf$id)
mdf$Name <- as.character(mdf$Name)
mdf$rDate <-as.Date(mdf$rDate)


#Mild replacements
mdf$Rating <- as.character(gsub( '-', '', mdf$Rating))
mdf$G <- as.character(gsub( ' ', '', mdf$G))
mdf$G <- as.character(gsub( '/', '', mdf$G))

#########################################################
#########################################################
#
#                   FRANCHISE CODE BLOCK                         
#
#########################################################
#########################################################


#--------------------------------------------------------
#             Franchise Function
#--------------------------------------------------------


franchiseloop <- function(csvlist) {
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  name = gsub('.csv','',csvlist)
  return(cbind(name,item))
}

#Obtain List
setwd("~/_Dev/_Lists")
readflist = read.csv("franchiseList.csv", header = TRUE, stringsAsFactors = FALSE)
csvflist = readflist$list

#Generate List
setwd("~/_Dev/fData")
fdf <- lapply(csvflist, franchiseloop) ##### MAIN LOOP #####
fdf <- ldply (fdf, data.frame)
fdf <-subset(fdf, date != "-" & date !="Date" & date != "Release")
colnames(fdf) <- c("name", "title", "gross", "date")

#Gross Format
fdf$gross <- as.character(gsub( '\\$', '', fdf$gross))
fdf$gross <- as.numeric(as.character(gsub( ',', '', fdf$gross)))
fdf$gross <- fdf$gross/1000000 #Convert to Millions

#Date Fix - uses Chron for pre 1969 dates
fdf$date <- as.Date(chron(fdf$date, format = c(dates = "m/d/y", times = "h:m:s")))

franchisesum <- function(flist) {
  a <- grep(flist, fdf$name) #find instances
  b <- sum(fdf$gross[a]) #sum instances
  c <- length(a) #find count
  
  output <- cbind(b,c)
  return (output)
}

#Summarize franchises by gross and average
flist <- fdf$name
flist <- unique(flist)
fsum <- lapply(flist, franchisesum) ##### MAIN LOOP #####
fsum <- ldply (fsum, data.frame)

#Join flist and franchise summary
flist <- cbind(flist, fsum)
flist <- mutate(flist, mAvg = b/c)
colnames(flist) <- c("name", "gross", "num", "mAvg")
flist <- tbl_df(flist)

#Duplicates
fdupes <- c("captainamerica", "superman", "batman", "godzillakong", 
            "ironmanfranchise", "avengersfranchise", "middleearth", 
            "thor", "wolverine")
flist <- subset(flist, !name %in% fdupes)
fdf <- subset(fdf, !name %in% fdupes)


#Creating ranks based on bins of 100
flist <- mutate(flist, fRank = round((mAvg /100)+1, digits =0))

#Jurassic Park adjusted down to fit in bins
flist$fRank[grep("jurassicpark", flist$name)]= 5


#Removing calculation columns to join with fdf
flistsub <- flist[, colnames(flist) %in% c("name","fRank")]
fdf <- join(fdf,flistsub, by='name')

#Writing files for Output
setwd("~/_Dev/_Backup")
write.csv(flist, file = "franchiseSummary.csv", row.names=FALSE) #consolidate data
write.csv(fdf, file = "franchiseDetail.csv", row.names=FALSE) #consolidate data

#Removing gross to address same name issue
fdf<- fdf[, !names(fdf) %in% c("gross")]


#--------------------------------------------------------
#             Cleaning Franchise Results
#--------------------------------------------------------

#Preparing For Franchises
nosp = mdf$Name
nosp = gsub(' ','', nosp)
nosp = gsub(':','', nosp)
#nospcol = gsub('\\(.*','', nospcol)
#print(nospcol)
mdf <- cbind(mdf,nosp)

#Preparing Nosp and binding
nosp= fdf$title
nosp = as.character(gsub( ' ', '', nosp))
nosp = as.character(gsub( ':', '', nosp))
fdf = cbind(fdf, nosp)




#print (head(fdf))

#----------Joining Franchise and Header
mdf <-join(mdf, fdf, by='nosp')
mdf <- mdf[ , !names(mdf) %in% c("name.1","date")]
mdf$fRank <- as.numeric(mdf$fRank)
mdf <- distinct(mdf)
#print(filter(mdf, franchise==1))


#Removing items without production budget
mdf <-subset(mdf, pExp>0)

#One Off Fix
ffix <- grep("fast6", mdf$id)
mdf$name[ffix]="fastandthefurious"	
mdf$fRank[ffix]="3"	


#########################################################
#########################################################
#
#                   WEEKLY CODE BLOCK                         
#
#########################################################
#########################################################

#--------------------------------------------------------
#             Initial Variables
#--------------------------------------------------------

csvlist = mdf$id
csvlist <- paste(csvlist,".csv",sep='')
csvlist <- unique(csvlist)
setwd("~/_Dev/wData")

#--------------------------------------------------------
#             Error Checking Function
#--------------------------------------------------------

PreChecks <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  wk = as.numeric(gsub( ',', '', item$week))
  #print(csvlist)
  
  #default values
  error = 0
  weekZ = 0
  
  #checks based on theatres/weeks
  if (length(wk)==0) 
  {error = 1} 
  else if (length(wk)==1) 
  {error = 1}
  else if (length(wk)==2) 
  {error = 1}
  else if (wk[1]==0) 
  {weekZ = 1}
  else {weekZ = 0}
  
  #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print (name)
  output = cbind(name, error, weekZ)
  
  return(output)
  
}

#--------------------------------------------------------
#             Cleaning Precheck List
#--------------------------------------------------------

#execute loop, convert from list to datafram
pclist <- lapply(csvlist, PreChecks)    ##### MAIN LOOP #####
pclist <- ldply (pclist, data.frame)

#Converting --- warning numerical values used
pclist$error <- as.numeric(as.character(pclist$error))
pclist$weekZ <- as.numeric(as.character(pclist$weekZ))
pclist<- mutate(pclist, flag=error + weekZ)
pclistn <- paste(pclist$name, '.csv',sep='')
#print(arrange(pclist, desc(flag)))

#List of error free items
oklist <-pclist[pclist$flag ==0, ]
oklistn <- paste(oklist$name, '.csv',sep='')
#print (oklist)

#List of zero week items
zlist <-pclist[pclist$weekZ ==1, ]
zlistn <- paste(zlist$name, '.csv',sep='')

#--------------------------------------------------------
#             Curve Generation Function
#--------------------------------------------------------

CurveGen <- function(csvlist, flag) {
  
  #reading in the file
  setwd("~/_Dev/wData")
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  week = as.numeric(gsub( ',', '', item$week))
  wGross = as.numeric(gsub( ',', '', item$wGross))
  trs = as.numeric(gsub( ',', '', item$trs))
  
  #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print (name)
  
  #creating empty weekly_drop and decay_curve 
  wDrop = c(1:length(wGross))
  dCurve = c(1:length(wGross))
  
  if (flag ==1)
  {
    #flag indicates a zero partial week
    #these are consolidated
    wGross[2] = wGross[2] + wGross[1]
    wGross[1] = wGross[2] #Ensures wDrop starts at 1
    
    #generating the curves
    for (i in 2:length(wGross)) 
    {
      wDrop[i] = wGross[i] / wGross[i-1]
      dCurve[i] = wDrop[i] * dCurve[i-1]
    }
    
    #fixes signpost error
    week[length(week)] = week[length(week) - 1] + 1
    
    #return value, removes duplicative row
    output = cbind(name, week, wGross, dCurve, trs)
    output = output[-1,] 
    setwd("~/_Dev/rData")
    write.csv(output, file = csvlist, row.names=FALSE)
    
    return(output)
  }
  

  #generating the curves
  for (i in 2:length(wGross)) {
    wDrop[i] = wGross[i] / wGross[i-1]
    dCurve[i] = wDrop[i] * dCurve[i-1]
  }
  
  #fixes signpost error
  week[length(week)] = week[length(week) - 1] + 1
  
  #return value
  output = cbind(name, week, wGross, dCurve, trs)
  
  setwd("~/_Dev/rData")
  write.csv(output, file = csvlist, row.names=FALSE)
  
  return(output)
  
}



#--------------------------------------------------------
#             List Combination
#--------------------------------------------------------

#Creating curves for zero items
zdata <- lapply(zlistn, CurveGen, flag=1) ##### MAIN LOOP #####
zdata <- ldply (zdata, data.frame)
zdata$week <- as.numeric(as.character(zdata$week))
zdata$wGross <- as.numeric(as.character(zdata$wGross))
zdata$dCurve <- as.numeric(as.character(zdata$dCurve))

#Creating curves for ok items
gdata <- lapply(oklistn, CurveGen, flag=0) ##### MAIN LOOP #####
gdata <- ldply (gdata, data.frame)
gdata$week <- as.numeric(as.character(gdata$week))
gdata$wGross <- as.numeric(as.character(gdata$wGross))
gdata$dCurve <- as.numeric(as.character(gdata$dCurve))

#Combining those sets
curveData <- rbind(zdata, gdata)
curveData$week <- curveData$week - 1 #indexing to 0
curveData<- tbl_df(curveData)
curveData<- unique(curveData)

#--------------------------------------------------------
#             Opening Week & Theatres
#--------------------------------------------------------

tlistn <- curveData$name
tlistn <- paste(tlistn,".csv",sep='')
tlistn <- unique(tlistn)
tlistn2 <- tlistn

OpenCalc <- function(list){
  setwd("~/_Dev/rData")
  item = read.csv(list, header = TRUE, stringsAsFactors = FALSE)
  wGross = as.numeric(gsub( ',', '', item$wGross))
  trs = as.numeric(gsub( ',', '', item$trs))
  
  id = gsub('.csv','',list)
  Open = wGross[1]
  trsA = trs[1]
  trsB = max(trs)
  #  Total = sum(wGross)
  output<- cbind(id,Open, trsA, trsB)
}

#Running Open Calc
tlistn <- lapply(tlistn,OpenCalc) ##### MAIN LOOP #####
tlistn <- ldply(tlistn, data.frame)

#Cleaning Output
tlistn$Open <- as.numeric(as.character(tlistn$Open))
tlistn$Open <- tlistn$Open / 1000000
tlistn$trsA <- as.numeric(as.character(tlistn$trsA))
tlistn$trsB <- as.numeric(as.character(tlistn$trsB))
#print(tlistn)


#--------------------------------------------------------
#             R2 Calculation
#--------------------------------------------------------

r2loop <- function(csvlist) {
  
  #reading in the file
  setwd("~/_Dev/rData")
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  dCurve = as.numeric(gsub( ',', '', item$dCurve))
  week = as.numeric(c(1:length(dCurve)))
  
  #linear model
  mlm = lm(log(dCurve) ~ week)
  #print (summary(mlm))
  
  #storing coefficients and r2 value
  coeffs = summary(mlm)$coefficients[,1]
  coeffs = t(coeffs)

  #obtaining R2 value
  r2 = summary(mlm)$r.squared
  
  #cleaning name for output
  id = gsub('.csv','',csvlist)
  
  #return value
  output = cbind(id, r2, coeffs)
  
}

#--------------------------------------------------------
#             R2 Cleaning
#--------------------------------------------------------

r2list <- lapply(tlistn2, r2loop) ##### MAIN LOOP #####
r2df <- ldply (r2list, data.frame)
factorconvert <- function(f){as.numeric(levels(f))[f]}
r2df[, 2:4] <- lapply(r2df[, 2:4], factorconvert)

#updating column names
colnames(r2df) <- c("id", "r2", "int", "shape")
r2df <- tbl_df(r2df)
#print(r2df)

#Joining with Total Gross Data
openData <-join(r2df, tlistn, by='id')
#print(head(openData))
openData <- distinct(openData)

#----------Joining Header and Weekly
mdf <-join(openData, mdf, by='id')
mdf$fRank[is.na(mdf$fRank)] <- 0
mdf$fRank <- as.factor(mdf$fRank)
#print(head(mdf))
mdf <- distinct(mdf)

#checks for duplicates
#print(sum(duplicated(mdf$id)))

#Removing old columns, updating names
mdf <- mdf[, !names(mdf) %in% c("int", "title", "nosp")]
names(mdf)[names(mdf)=="name"] <- "fName"

#--------------------------------------------------------
#             Output To File
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
#write.csv(mdf, file = "BOM_Data.csv", row.names=FALSE) #consolidate data
#write.csv(fdf, file = "FranchiseData.csv", row.names=FALSE) #franchise

#Remove all enviornment variables
#rm(list = ls())





