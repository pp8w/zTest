library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization
library(broom) #coefficient extraction
library(rjson)
library(RCurl)


#--------------------------------------------------------
#             CSV Generation Function
#--------------------------------------------------------


rtGen <- function (){
  
  orig <- mdf2016$Name
  links <- mdf2016$Name
  
  #Spaces
  links <- as.character(gsub('  ',' ', links))
  links <- as.character(gsub(' ','+', links))
  
  #Punctuation
  links <- as.character(gsub('\\.','', links)) #period
  links <- as.character(gsub('\\?','', links)) #question
  links <- as.character(gsub(':','%3A', links)) #colon
  links <- as.character(gsub('\\,','', links)) #comma
  links <- as.character(gsub('&amp;','%26', links)) #ampersand
  links <- as.character(gsub('/','%2F', links)) #slash
  links <- as.character(gsub('!','%2F', links)) # exclamation
  
  
  #OK List:  '   -    !    trailing +
  
  #Items with (Year)
  links <- as.character(gsub('\\(.*','', links))
  
  
  #Format for movies
  urlA ="http://www.omdbapi.com/?t="
  urlB = "&tomatoes=true"
  
  request = paste(urlA, links, urlB, sep="")
  #print(request)

  request[1] =  "http://www.omdbapi.com/?t=13+hours&tomatoes=true"
  request[5] =  "http://www.omdbapi.com/?t=ben+hur&y=2016&tomatoes=true"
  request[17] = "http://www.omdbapi.com/?t=ghostbusters&y=2016&tomatoes=true"
  request[26] = "http://www.omdbapi.com/?t=jungle+book&y=2016&tomatoes=true"
  request[35] = "http://www.omdbapi.com/?t=mother%27s+day&y=2016&tomatoes=true"
  request[37] = "http://www.omdbapi.com/?t=pete%27s+dragon&y=2016&tomatoes=true"

return(request)
}

setwd("~/_Dev/2016rt")
rtList2016 = rtGen()
CSVgen <- function (rtList2016) {
  for (i in 1:length(rtList2016)) {
    item <- getURL(rtList2016[i])
    item <- as.data.frame(fromJSON(item)) 
    
    output = paste(i,".csv",sep="")
    write.csv(item, output, row.names=FALSE)
  }
}

#CSVgen(rtList2016) #-----THIS IS THE BIG ONE------#



#--------------------------------------------------------
#             CSV Reading Function
#--------------------------------------------------------

numlist = c(1:length(rtList2016))
for (i in 1:length(numlist)) {numlist[i]= paste(i,".csv",sep="")}
#print (numlist)

CSVread <- function (numlist) {
  item = read.csv(numlist, header = TRUE, stringsAsFactors = FALSE)
  
  Title	=	as.character(	item$Title	)
  Year	=	as.numeric(	item$Year	)
  Director	=	as.character(	item$Director	)
  Writer	=	as.character(	item$Writer	)
  Actors	=	as.character(	item$Actors	)
  imdbID	=	as.character(	item$imdbID	)
  iRate	=	as.numeric(	item$imdbRating	)
  meta	=	as.numeric(	item$Metascore	)
  rtCS	=	as.numeric(	item$tomatoMeter	)
  rtCR	=	as.numeric(	item$tomatoRating	)
  rtUS	=	as.numeric(	item$tomatoUserMeter	)
  rtUR	=	as.numeric(	item$tomatoUserRating	)
  
  cJoin = cbind(numlist, Title, Year, Director, Writer, Actors, imdbID, iRate, meta, rtCS, rtCR, rtUS, rtUR)
  #print (cJoin)
}

#--------------------------------------------------------
#             Cleaning OMDB Data
#--------------------------------------------------------

rtOut2016 <- lapply(numlist, CSVread) #-----THIS IS A LOOP ------#
rtOut2016 <- ldply (rtOut2016, data.frame)


#MDF comes from weekly

combod2016 <- cbind(rtOut2016,mdf2016)
combod2016 <- combod2016[ , !names(combod2016) %in% c("numlist", "nosp")] #removes duplicative col
#combod2016 <- distinct(combod2016)

#----Changing columns to Character Types
combod2016$rtCS	=	as.numeric(as.character(combod2016$rtCS))
combod2016$rtCR	=	as.numeric(as.character(combod2016$rtCR))
combod2016$rtUS	=	as.numeric(as.character(combod2016$rtUS))
combod2016$rtUR	=	as.numeric(as.character(combod2016$rtUR))
combod2016$iRate	=	as.numeric(as.character(combod2016$iRate))
combod2016$meta	=	as.numeric(as.character(combod2016$meta))
combod2016$week	=	as.numeric(as.character(combod2016$week))



#One offs
combod2016 <- combod2016[ , !names(combod2016) %in% c("numlist", "nosp", "r2","int")] #removes duplicative col
#combod2016 <- combod2016[!is.na(combod2016$Title) , ] #removes rows with blank title (errors)
#combod2016 <- mutate(combod2016, tDiff = trsB- trsA) #adds column for theatre difference

#Sets blank fRanks to Zero
combod2016$fRank[is.na(combod2016$fRank)] <- 0 #Puts Zero 

#-----Key RT omissions

#combod2016$rtUR[grep("bourne5", combod2016$id)]= 3.5
#combod2016$rtUR[grep("conjuring2", combod2016$id)]= 4.0
#combod2016$rtUR[grep("purge3", combod2016$id)]= 3.3


#--------------------------------------------------------
#             Actor Columns
#--------------------------------------------------------


setwd("~/_Dev/_Lists")

#Reading in Actors
file <- read.csv('actorList.csv', header=TRUE)
aList <- as.character(file$Actors)
aList <- tolower(aList)

aData <- as.character(combod2016$Actors)
aData <- tolower(aData)

#Reading in Directors
file <- read.csv('directorList.csv', header=TRUE)
dList <- as.character(file$Director)
dList <- tolower(dList)

dData <- as.character(combod2016$Director)
dData <- tolower(dData)


#--------------------------------------------------------
#             Replace Function 
#--------------------------------------------------------

cRep <- function (List, Data){
  
  for (i in 1:length(List)){
    word = List[i]
    a <- grep(word, Data)
    Data[a] <- paste(Data[a],"#")
  }
  
  Data <- gsub( '\\w', '', Data)
  Data <- gsub( '#', '1', Data)
  Data <- gsub( '\\D', '', Data)
  Data <- gsub( '1111', '4', Data)
  Data <- gsub( '111', '3', Data)
  Data <- gsub( '11', '2', Data)
  Data <- as.numeric(Data)
  Data[is.na(Data)] <- 0
  
  return (Data)
}

#--------------------------------------------------------
#             Replace Results 
#--------------------------------------------------------


#Adjusting for foreign chars
dData <- gsub( 'alfonso cuarón', 'alfonso cuaron', dData)
dData <- gsub( 'alejandro g. iñárritu', 'alejandro g. inarritu', dData)

#Primary Function Call
aCol <- cRep(aList, aData)
dCol <- cRep(dList, dData)

#Print Results
print (sum(aCol))
print (sum(dCol))

combod2016 <-cbind(combod2016, aCol, dCol)
combod2016 <-distinct(combod2016)

#--------------------------------------------------------
#             Genre Consoliation
#--------------------------------------------------------

#Prefix Consolidation
combod2016$G <- as.character(gsub( 'Action.*', 'Action', combod2016$G))
combod2016$G <- as.character(gsub( 'Adventure.*', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Comedy.*', 'Comedy', combod2016$G))
combod2016$G <- as.character(gsub( 'Sci-Fi.*', 'Sci-Fi', combod2016$G))
combod2016$G <- as.character(gsub( 'Horror.*', 'Horror', combod2016$G))
combod2016$G <- as.character(gsub( 'Foreign.*', 'Foreign', combod2016$G))
combod2016$G <- as.character(gsub( 'Crime.*', 'Crime', combod2016$G))
combod2016$G <- as.character(gsub( 'Drama.*', 'Drama', combod2016$G))
combod2016$G <- as.character(gsub( 'Fantasy.*', 'Fantasy', combod2016$G))
combod2016$G <- as.character(gsub( 'Family.*', 'Family', combod2016$G))
combod2016$G <- as.character(gsub( 'War.*', 'War', combod2016$G))

#Suffix Consolidation
combod2016$G <- as.character(gsub( 'Period', '', combod2016$G))
combod2016$G <- as.character(gsub( 'Romantic', '', combod2016$G))
combod2016$G <- as.character(gsub( 'Sports', '', combod2016$G))
combod2016$G <- as.character(gsub( 'Historical', '', combod2016$G))

#set2016 Combinations
combod2016$G <- as.character(gsub( 'Musical', 'Concert', combod2016$G))
combod2016$G <- as.character(gsub( 'Music', '', combod2016$G))
combod2016$G <- as.character(gsub( 'War', 'Drama', combod2016$G))
combod2016$G <- as.character(gsub( 'Epic', 'Drama', combod2016$G))
combod2016$G <- as.character(gsub( 'Sci-Fi', 'Sci-Fan', combod2016$G))
combod2016$G <- as.character(gsub( 'Fantasy', 'Sci-Fan', combod2016$G))
combod2016$G <- as.character(gsub( 'WesternComedy', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Western', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Family', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Crime', 'Thriller', combod2016$G))

#Other
combod2016$G <- as.character(gsub( 'IMAX', 'Other', combod2016$G))
combod2016$G <- as.character(gsub( 'Concert', 'Other', combod2016$G))


#Convert to Factor
combod2016$fRank <- as.factor(combod2016$fRank)


#Creating trimmed set
dcon2016 <- combod2016[ , !names(combod2016) %in% c("Year", "Director", "Writers", "Actors", "Writer", "iRate", "meta", "rtCS", "rtCR", "rtUS", "Name", "X.Intercept.", "title")] #removes duplicative col
dcon2016 <- tbl_df(dcon2016)

#Ben Hur
ffix = grep("tt5582876", dcon2016$imdbID)
dcon2016$rtUR[ffix] = 3.6

#--------------------------------------------------------
#             Writing to File
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
#write.csv(rtOut2016, "rtData2016.csv", row.names = FALSE)
write.csv(dcon2016, "Data_Test.csv", row.names=FALSE)

