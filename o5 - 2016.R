rtGen <- function (){rtList2016 = c( "http://www.omdbapi.com/?t=13+Hours:The+Secret+Soldiers+of+Benghazi&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+5th+Wave&tomatoes=true",
                                 "http://www.omdbapi.com/?t=A+Beautiful+Planet&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Absolutely+Fabulous:The+Movie&tomatoes=true",
                                 "http://www.omdbapi.com/?t=A+Hologram+for+the+King&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Alice+Through+the+Looking+Glass&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Divergent+Series:Allegiant&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Angry+Birds+Movie&tomatoes=true",
                                 "http://www.omdbapi.com/?t=War+Dogs&tomatoes=true",
                                 "http://www.omdbapi.com/?t=10+Cloverfield+Lane&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Barbershop:The+Next+Cut&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Ben-Hur+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+BFG&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Darkness&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Jason+Bourne&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Boy+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Cafe+Society&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Captain+Fantastic&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Central+Intelligence&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Young+Messiah&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Conjuring+2&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Criminal+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Suicide+Squad&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Deadpool&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Dirty+Grandpa&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Zootopia&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Don%27t+Breathe&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Don%27t+Breathe&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Eddie+the+Eagle&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Eye+in+the+Sky&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Fifty+Shades+of+Black&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Finest+Hours&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Florence+Foster+Jenkins&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Beautiful+Creatures+(2013)&y=2013&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Free+State+of+Jones&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Ghostbusters+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=God%27s+Not+Dead+2&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Gods+of+Egypt&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Brothers+Grimsby&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Hail+Caesar!&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Hardcore+Henry&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Hell+or+High+Water&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Hillary%27s+America:The+Secret+History+of+the+Democratic+Party&tomatoes=true",
                                 "http://www.omdbapi.com/?t=How+to+Be+Single&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Hunt+for+the+Wilderpeople&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Huntsman:Winter%27s+War&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Ice+Age:Collision+Course&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Independence+Day:Resurgence&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Secret+Life+of+Pets&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Jungle+Book+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Keanu&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Kubo+and+the+Two+Strings&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Kung+Fu+Panda+3&tomatoes=true",
                                 "http://www.omdbapi.com/?t=London+Has+Fallen&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Love+&amp;+Friendship&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Captain+America:Civil+War&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Me+Before+You&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Mechanic:Resurrection&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Meet+the+Blacks&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Boss&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Mike+and+Dave+Need+Wedding+Dates&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Miracles+from+Heaven&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Money+Monster&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Mother%27s+Day&tomatoes=true",
                                 "http://www.omdbapi.com/?t=My+Big+Fat+Greek+Wedding+2&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Nerve&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Lights+Out&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Nice+Guys&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Nine+Lives+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Norm+of+the+North&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Now+You+See+Me+2&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Pete%27s+Dragon+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Finding+Dory&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Pride+and+Prejudice+and+Zombies&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Purge:Election+Year&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Race+&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Ratchet+&amp;+Clank&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Ride+Along+2&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Risen&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Sausage+Party&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Star+Trek+Beyond&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Sultan&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Batman+v+Superman:Dawn+of+Justice&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Legend+of+Tarzan&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Choice&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Infiltrator&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Meddler&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Perfect+Match&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Shallows&tomatoes=true",
                                 "http://www.omdbapi.com/?t=The+Witch&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Teenage+Mutant+Ninja+Turtles:Out+of+the+Shadows&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Popstar:Never+Stop+Never+Stopping&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Triple+9&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Neighbors+2:Sorority+Rising&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Bad+Moms&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Whiskey+Tango+Foxtrot&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Warcraft&tomatoes=true",
                                 "http://www.omdbapi.com/?t=X-Men:Apocalypse&tomatoes=true",
                                 "http://www.omdbapi.com/?t=Zoolander+2&tomatoes=true"
)
return(rtList2016)
}

#-----required libraries, list was declared first for error checking simplification

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
for (i in 1:length(numlist)) {numlist[i]= paste("o",i,".csv",sep="")}
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

nosp = rtOut2016$Title
nosp = as.character(gsub( ' ', '', nosp))
nosp = as.character(gsub( ':', '', nosp))
rtOut2016$imdbID <- as.character(rtOut2016$imdbID)
rtOut2016 <-cbind (rtOut2016, nosp)

#--------------------------------------------------------
#             Joining Movie Index and RT
#--------------------------------------------------------

#MDF comes from weekly
combod2016 <- join(mdf2016, rtOut2016, by='nosp')
combod2016 <- combod2016[ , !names(combod2016) %in% c("numlist", "nosp")] #removes duplicative col
combod2016 <- distinct(combod2016)

#----Changing columns to Character Types
combod2016$rtCS	=	as.numeric(as.character(combod2016$rtCS))
combod2016$rtCR	=	as.numeric(as.character(combod2016$rtCR))
combod2016$rtUS	=	as.numeric(as.character(combod2016$rtUS))
combod2016$rtUR	=	as.numeric(as.character(combod2016$rtUR))
combod2016$iRate	=	as.numeric(as.character(combod2016$iRate))
combod2016$meta	=	as.numeric(as.character(combod2016$meta))
combod2016$week	=	as.numeric(as.character(combod2016$week))


#-----Franchise Updates
combod2016$franchise[is.na(combod2016$franchise)] <- 0 #replaces NA in franchise with 0
combod2016$franchise <- as.factor(combod2016$franchise) #makes franchise a factor


#One offs
combod2016 <- combod2016[ , !names(combod2016) %in% c("numlist", "nosp", "r2","int")] #removes duplicative col
combod2016 <- combod2016[!is.na(combod2016$Title) , ] #removes rows with blank title (errors)
combod2016 <- mutate(combod2016, tDiff = trsB- trsA) #adds column for theatre difference

#-----Key RT omissions

combod2016$rtUR[grep("bourne5", combod2016$id)]= 3.5
combod2016$rtUR[grep("conjuring2", combod2016$id)]= 4.0
combod2016$rtUR[grep("purge3", combod2016$id)]= 3.3


#--------------------------------------------------------
#             Actor Columns
#--------------------------------------------------------


setwd("~/_Dev/_Backup")

#Reading in Actors
file <- read.csv('actors.csv', header=TRUE)
aList <- as.character(file$Actors)
aList <- tolower(aList)

aData <- as.character(combod2016$Actors)
aData <- tolower(aData)

#Reading in Directors
file <- read.csv('directors.csv', header=TRUE)
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
combod2016$G <- as.character(gsub( 'Sci-Fi', 'Sci-Fan', combod2016$G))
combod2016$G <- as.character(gsub( 'Fantasy', 'Sci-Fan', combod2016$G))
combod2016$G <- as.character(gsub( 'WesternComedy', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Western', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Family', 'Adventure', combod2016$G))
combod2016$G <- as.character(gsub( 'Crime', 'Thriller', combod2016$G))

#Other
combod2016$G <- as.character(gsub( 'IMAX', 'Other', combod2016$G))
combod2016$G <- as.character(gsub( 'Concert', 'Other', combod2016$G))


dcon2016 <- combod2016[ , !names(combod) %in% c("Year", "Director", "Writers", "Actors", "Writer", "iRate", "meta", "rtCS", "rtCR", "rtUS", "rtUR")] #removes duplicative col
dcon2016 <- tbl_df(dcon2016)



#--------------------------------------------------------
#             Writing to File
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
write.csv(rtOut2016, "rtData2016.csv", row.names = FALSE)
#write.csv(mdfi, "MDFIndex.csv", row.names = FALSE)
write.csv(combod2016, "modelData2016.csv", row.names=FALSE)



