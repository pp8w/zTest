library(plyr) #ldply
library(dplyr) #table operations
library(tidyr) #table operations
library(ggplot2) #visualization
library(rjson) #R / JSON Integration
library(RCurl) #For Web
library(corrplot) #Used for correlation coefficients

#########################################################
#########################################################
#
#                   OMDB API Block                         
#
#########################################################
#########################################################


rtGen <- function (){
  links <- mdf$Name
  
  #Spaces
  links <- as.character(gsub('  ',' ', links)) #double space
  links <- as.character(gsub(' ','+', links)) # space with +
  
  #Punctuation 
  links <- as.character(gsub('\\.','', links)) #period
  links <- as.character(gsub('\\?','', links)) #question
  links <- as.character(gsub(':','%3A', links)) #colon
  links <- as.character(gsub('\\,','', links)) #comma
  links <- as.character(gsub('&amp;','%26', links)) #ampersand
  links <- as.character(gsub('/','%2F', links)) #slash
  links <- as.character(gsub('!','%2F', links)) # exclamation
  #OK List:  '   -    !    trailing space
  
  #Items with (Year)
  links <- as.character(gsub('\\(.*','', links))
  
  #----Formating request string
  urlA ="http://www.omdbapi.com/?t="
  urlB = "&tomatoes=true"
  
  request = paste(urlA, links, urlB, sep="")
  
  #These are manual fixes that had to be done
  request[23]	="http://www.omdbapi.com/?t=knight+and+day&tomatoes=true"
  request[54]	="http://www.omdbapi.com/?i=tt1707386&tomatoes=true"#Les Mis
  request[59]	="http://www.omdbapi.com/?t=love+%26+other+drugs&tomatoes=true"
  request[71]	="http://www.omdbapi.com/?t=Oldboy&y=2013&tomatoes=true"
  request[110]	="http://www.omdbapi.com/?t=21+%26+over&tomatoes=true"
  request[139]	="http://www.omdbapi.com/?t=annie&y=2014&tomatoes=true"
  request[154]	="http://www.omdbapi.com/?t=The+avengers&tomatoes=true"
  request[157]	="http://www.omdbapi.com/?t=bad+grandpa&tomatoes=true"
  request[207]	="http://www.omdbapi.com/?t=carrie&y=2013&tomatoes=true"
  request[219]	="http://www.omdbapi.com/?t=cinderella&y=2015&tomatoes=true"
  request[229]	="http://www.omdbapi.com/?t=conan&y=2011&tomatoes=true"
  request[259]	="http://www.omdbapi.com/?t=Death+at+a+Funeral&y=2010&tomatoes=true"
  request[309]	="http://www.omdbapi.com/?t=Fantastic+Four&y=2015&tomatoes=true"
  request[326]	="http://www.omdbapi.com/?t=Footloose&y=2011&tomatoes=true"
  request[345]	="http://www.omdbapi.com/?i=tt1132620&tomatoes=true"
  request[358]	="http://www.omdbapi.com/?t=greatest+movie+ever+sold&tomatoes=true"
  request[373]	="http://www.omdbapi.com/?t=hansel+gretel+witch+hunters&tomatoes=true"
  request[410]	="http://www.omdbapi.com/?t=The+Illusionist&y=2010&tomatoes=true"
  request[428]	="http://www.omdbapi.com/?t=jackass+3d&tomatoes=true"
  request[447]	="http://www.omdbapi.com/?t=The+Karate+Kid&y=2010&tomatoes=true"
  request[481]	="http://www.omdbapi.com/?t=the+lorax&tomatoes=true"
  request[492]	="http://www.omdbapi.com/?t=madea+witness+protection&tomatoes=true"
  request[509]	="http://www.omdbapi.com/?t=maze+runner+the+burn&tomatoes=true"
  request[515]	="http://www.omdbapi.com/?t=men+in+black+3&y=&tomatoes=true"
  request[552]	="http://www.omdbapi.com/?t=A+Nightmare+on+Elm+Street&y=2010&tomatoes=true"
  request[578]	="http://www.omdbapi.com/?t=pain+%26+gain&tomatoes=true"
  request[608]	="http://www.omdbapi.com/?i=tt2058673&tomatoes=true" #Point Break
  request[609]	="http://www.omdbapi.com/?i=tt1029360&tomatoes=true" #Poltergeist
  request[674]	="http://www.omdbapi.com/?t=sex+and+zen+extreme&tomatoes=true"
  request[702]	="http://www.omdbapi.com/?t=step+up+3d&tomatoes=true"
  request[708]	="http://www.omdbapi.com/?i=tt0999913&tomatoes=true" # Straw Dogs
  request[711]	="http://www.omdbapi.com/?t=survival+of+the+dead&tomatoes=true"
  request[729]	="http://www.omdbapi.com/?i=tt0905372&tomatoes=true" #TheThing
  request[742]	="http://www.omdbapi.com/?i=tt1386703&tomatoes=true" #Total Recall
  request[783]	="http://www.omdbapi.com/?t=peeples&tomatoes=true"
  request[790]	="http://www.omdbapi.com/?t=why+did+I+get+married+too&tomatoes=true"

  #545, 583, 616, 746 not found

  return(request)
   }


#--------------------------------------------------------
#             CSV Generation Function
#--------------------------------------------------------


setwd("~/_Dev/rtOutput")
rtList = rtGen()
CSVgen <- function (rtList) {
    for (i in 1:length(rtList)) {
      item <- getURL(rtList[i])
      item <- as.data.frame(fromJSON(item)) 
    
      output = paste(i,".csv",sep="")
      write.csv(item, output, row.names=FALSE)
    }
}

#CSVgen(rtList) #-----THIS IS THE BIG ONE------#



#--------------------------------------------------------
#             CSV Reading Function
#--------------------------------------------------------


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


numlist = c(1:length(rtList))
for (i in 1:length(numlist)) {numlist[i]= paste(i,".csv",sep="")}
#print (numlist)

setwd("~/_Dev/rtOutput")
rt <- lapply(numlist, CSVread) #-----THIS IS A LOOP ------#
rtOut <- ldply (rt, data.frame)


nosp = rtOut$Title
nosp = as.character(gsub( ' ', '', nosp))
nosp = as.character(gsub( ':', '', nosp))
rtOut$imdbID <- as.character(rtOut$imdbID)
rtOut <-cbind (rtOut, nosp)

#Binding OMDB and MDF
combod <- cbind(rtOut,mdf)

combod$rtCS	=	as.numeric(as.character(combod$rtCS))
combod$rtCR	=	as.numeric(as.character(combod$rtCR))
combod$rtUS	=	as.numeric(as.character(combod$rtUS))
combod$rtUR	=	as.numeric(as.character(combod$rtUR))
combod$iRate	=	as.numeric(as.character(combod$iRate))
combod$meta	=	as.numeric(as.character(combod$meta))
combod$Year	=	as.numeric(as.character(combod$Year))


#Remvoing some columns 
combod <- combod[ , !names(combod) %in% c("numlist", "nosp")] #removes duplicative col
combod <- combod[!is.na(combod$Title) , ] #removes rows with blank title (errors)


#########################################################
#########################################################
#
#                   ACTOR/DIRECTOR CODE BLOCK                         
#
#########################################################
#########################################################



setwd("~/_Dev/_Lists")
#Reading in Actors
file <- read.csv('actorList.csv', header=TRUE)
aList <- as.character(file$Actors)
aList <- tolower(aList)

aData <- as.character(combod$Actors)
aData <- tolower(aData)

#Reading in Directors
file <- read.csv('directorList.csv', header=TRUE)
dList <- as.character(file$Director)
dList <- tolower(dList)

dData <- as.character(combod$Director)
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

combod <-cbind(combod, aCol, dCol)
combod <-distinct(combod)


#########################################################
#########################################################
#
#                   GENRE SCRUB CODE BLOCK                         
#
#########################################################
#########################################################


#Prefix Consolidation
combod$G <- as.character(gsub( 'Action.*', 'Action', combod$G))
combod$G <- as.character(gsub( 'Adventure.*', 'Adventure', combod$G))
combod$G <- as.character(gsub( 'Comedy.*', 'Comedy', combod$G))
combod$G <- as.character(gsub( 'Sci-Fi.*', 'Sci-Fi', combod$G))
combod$G <- as.character(gsub( 'Horror.*', 'Horror', combod$G))
combod$G <- as.character(gsub( 'Foreign.*', 'Foreign', combod$G))
combod$G <- as.character(gsub( 'Crime.*', 'Crime', combod$G))
combod$G <- as.character(gsub( 'Drama.*', 'Drama', combod$G))
combod$G <- as.character(gsub( 'Fantasy.*', 'Fantasy', combod$G))
combod$G <- as.character(gsub( 'Family.*', 'Family', combod$G))
combod$G <- as.character(gsub( 'War.*', 'War', combod$G))

#Suffix Consolidation
combod$G <- as.character(gsub( 'Period', '', combod$G))
combod$G <- as.character(gsub( 'Romantic', '', combod$G))
combod$G <- as.character(gsub( 'Sports', '', combod$G))
combod$G <- as.character(gsub( 'Historical', '', combod$G))

#Set Combinations
combod$G <- as.character(gsub( 'Musical', 'Concert', combod$G))
combod$G <- as.character(gsub( 'Music', '', combod$G))
combod$G <- as.character(gsub( 'War', 'Drama', combod$G))
combod$G <- as.character(gsub( 'Sci-Fi', 'Sci-Fan', combod$G))
combod$G <- as.character(gsub( 'Fantasy', 'Sci-Fan', combod$G))
combod$G <- as.character(gsub( 'WesternComedy', 'Adventure', combod$G))
combod$G <- as.character(gsub( 'Western', 'Adventure', combod$G))
combod$G <- as.character(gsub( 'Family', 'Adventure', combod$G))
combod$G <- as.character(gsub( 'Crime', 'Thriller', combod$G))

#Other
combod$G <- as.character(gsub( 'IMAX', 'Other', combod$G))
combod$G <- as.character(gsub( 'Concert', 'Other', combod$G))



#Trimming columns
dcon <- combod[ , !names(combod) %in% c("Year", "r2", "imdbID", "Director", "Writers", "Actors", "Writer", "iRate", "meta", "rtCS", "rtCR", "rtUS", "Name", "name")] 
dcon <- tbl_df(dcon)

#--------------------------------------------------------
#             Writing to File
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
write.csv(dcon, "Data_Train.csv", row.names=FALSE)
#write.csv(rtOut, "rtData.csv", row.names = FALSE)


