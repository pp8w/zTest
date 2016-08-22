library(rjson)

rtList = c( "http://www.omdbapi.com/?t=Tomorrowland&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Age+of+Adaline&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Project+Almanac&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Alvin+and+the+Chipmunks%3AThe+Road+Chip&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Ant-Man&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Avengers%3AAge+of+Ultron&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Second+Best+Exotic+Marigold+Hotel&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Big+Short&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Black+or+White&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Visit&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Gift&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Spectre&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Boy+Next+Door&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Brooklyn&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Chappie&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Cinderella&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Bridge+of+Spies&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Concussion&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=No+Escape&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Creed&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Crimson+Peak&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Aloha&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Unfriended&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Daddy's+Home&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+DUFF&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Entourage&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Everest&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Ex+Machina&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Fantastic+Four&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Furious+7&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Fifty+Shades+of+Grey&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Focus&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Mad+Max%3AFury+Road&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Get+Hard&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Goosebumps&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Home&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=In+the+Heart+of+the+Sea&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Hitman%3AAgent+47&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Hotel+Transylvania+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Hunger+Games%3AMockingjay+-+Part+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Insidious+Chapter+3&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Divergent+Series%3AInsurgent&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Intern&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Joy&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Jupiter+Ascending&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Jurassic+World&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Wedding+Ringer&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Krampus&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Last+Witch+Hunter&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Lazarus+Effect&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Love+the+Coopers&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Magic+Mike+XXL&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Max&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Maze+Runner&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=McFarland,+USA&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Mission%3AImpossible+-+Rogue+Nation&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Minions&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Sisters&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Vacation&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Gallows&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Paddington&y=2014&tomatoes=true",
            "http://www.omdbapi.com/?t=Pan&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Paper+Towns&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Paul+Blart%3AMall+Cop+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Peanuts+Movie&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Perfect+Guy&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Pitch+Perfect+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Good+Dinosaur&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Inside+Out&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Pixels&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Point+Break&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Poltergeist&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Revenant&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Ricki+and+the+Flash&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Run+All+Night&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=San+Andreas&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Martian&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Kingsman%3AThe+Secret+Service&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Sicario&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Sinister+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Southpaw&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+SpongeBob+Movie%3ASponge+Out+of+Water&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Spotlight&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Star+Wars%3AThe+Force+Awakens&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Straight+Outta+Compton&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Spy&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Taken+3&y=2014&tomatoes=true",
            "http://www.omdbapi.com/?t=Ted+2&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Terminator%3AGenisys&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Hateful+Eight&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Longest+Ride&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Trainwreck&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Man+From+U.N.C.L.E.&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=A+Walk+in+the+Woods&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=War+Room&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Black+Mass&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=Hot+Pursuit&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Woman+in+Black&y=2014&tomatoes=true",
            "http://www.omdbapi.com/?t=Woman+in+Gold&y=2015&tomatoes=true",
            "http://www.omdbapi.com/?t=The+Night+Before&y=2015&tomatoes=true"
)

#2014: paddington, taken , the woman in black

setwd("~/_Dev/rtOutput")
#----Creating CSVs

CSVgen <- function () {
    for (i in 1:length(rtList)) {
      item <- getURL(rtList[i])
      item <- as.data.frame(fromJSON(item)) 
    
      output = paste("o",i,".csv",sep="")
      write.csv(item, output, row.names=FALSE)
    }
}



numlist = c(1:length(rtList))
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
    tomatoMeter	=	as.numeric(	item$tomatoMeter	)
    tomatoRating	=	as.numeric(	item$tomatoRating	)
    tomatoUserMeter	=	as.numeric(	item$tomatoUserMeter	)
    tomatoUserRating	=	as.numeric(	item$tomatoUserRating	)
   
    cJoin = cbind(Title, Year, Director, Writer, Actors, imdbID, tomatoMeter, tomatoRating, tomatoUserMeter, tomatoUserRating)
    #print (cJoin)
  }

rt <- lapply(numlist, CSVread)
rtOut <- ldply (rt, data.frame)

write.csv(rtOut, "rtOut.csv", row.names=FALSE)

