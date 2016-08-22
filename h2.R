library(dplyr)
library(tidyr)
library(plyr) #conversion
library(ggplot2)
library(dummies) #dummy var creation

#----------Initial Variables

csvlist = c(
  "1952.csv",
  "ageofadaline.csv",
  "almanac.csv",
  "alvin4.csv",
  "antman.csv",
  "avengers2.csv",
  "bestexotic2.csv",
  "bigshort.csv",
  "blackorwhite.csv",
  "blumhouse2015.csv",
  "blumhousejuly2015.csv",
  "bond24.csv",
  "boynextdoor.csv",
  "brooklyn.csv",
  "chappie.csv",
  "cinderella2015.csv",
  "coldwar2015.csv",
  "concussion2015.csv",
  "coup.csv",
  "creed.csv",
  "crimsonpeak.csv",
  "crowe2014.csv",
  "cybernatural.csv",
  "daddyshome.csv",
  "duff.csv",
  "entourage.csv",
  "everest2015.csv",
  "exmachina.csv",
  "fantasticfour15.csv",
  "fast7.csv",
  "fiftyshadesofgrey.csv",
  "focus2015.csv",
  "furyroad.csv",
  "gethard.csv",
  "goosebumps.csv",
  "happysmekday.csv",
  "heartofthesea.csv",
  "hitman47.csv",
  "hoteltransylvania2.csv",
  "hungergames4.csv",
  "insidiouschapter3.csv",
  "insurgent.csv",
  "intern.csv",
  "joy.csv",
  "jupiterascending.csv",
  "jurassicpark4.csv",
  "kevinhart15.csv",
  "krampus.csv",
  "lastwitchhunter.csv",
  "lazarus.csv",
  "lovethecoopers.csv",
  "magicmike2.csv",
  "max2015.csv",
  "mazerunner2.csv",
  "mcfarland.csv",
  "mi5.csv",
  "minions.csv",
  "nest.csv",
  "newline15.csv",
  "newlinehorror2015.csv",
  "paddington.csv",
  "pan.csv",
  "papertowns.csv",
  "paulblart2.csv",
  "peanuts2015.csv",
  "perfectguy.csv",
  "pitchperfect2.csv",
  "pixar2013.csv",
  "pixar2014.csv",
  "pixels.csv",
  "pointbreak2015.csv",
  "poltergeist2015.csv",
  "revenant.csv",
  "rickiandtheflash.csv",
  "runallnight.csv",
  "sanandreas.csv",
  "scott2016.csv",
  "secretservice.csv",
  "sicario.csv",
  "sinister2.csv",
  "southpaw2015.csv",
  "spongebob2.csv",
  "spotlight.csv",
  "starwars7.csv",
  "straightouttacompton.csv",
  "susancooper.csv",
  "taken3.csv",
  "ted2.csv",
  "terminator2015.csv",
  "thehatefuleight.csv",
  "thelongestride.csv",
  "trainwreck15.csv",
  "uncle.csv",
  "walkinthewoods.csv",
  "warroom2015.csv",
  "whiteybulger15.csv",
  "witherspoonvergara.csv",
  "womaninblack2.csv",
  "womaningold.csv",
  "xmas2015.csv"
)

#----------File Loop
setwd("~/_Dev/hData")
fileloop <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = FALSE, stringsAsFactors = FALSE)

}

#-------Executing The Loop

mlist <- lapply(csvlist, fileloop)
mdf <- ldply (mlist, data.frame)

#updating column headers
colnames(mdf) = c('id', 'adj', 'Name', 'dGross', 'dist', 'rDate', 'G', 'Runtime',  'Rating', 'pBudget')
mdf <- mdf[ , !names(mdf) %in% c("adj","dist")]
mdf <- tbl_df(mdf)


#----- Cleaning the contents

#HTML replacements - removing html fragments
mdf <- as.data.frame(sapply(mdf,gsub,pattern="<b>|</b>|<br/>|\\$",replacement=""))
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

rtExp <- sapply(mdf$pBudget, function(x) eval(parse(text=x))) #expression to value
rtExp = rtExp / 1000000 # converting to millions
mdf$pBudget <-as.numeric(rtExp)


#Runtime - Changing from "[X hrs Y min]" to "[Z]", where Z is minutes
#Converting to expression then evaluating the expression
mdf$Runtime <- as.character(gsub( ' min.', '', mdf$Runtime))
mdf$Runtime <- as.character(gsub( ' hrs. ', '\\*60+', mdf$Runtime))
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


#Creating mdf2, new table with dummy columns for genre and rating
#Converting to table
mdf2 <- mdf
mdf2 <- dummy.data.frame(mdf2, name = "G", sep=".")
mdf2 <- dummy.data.frame(mdf2, name = "Rating", sep=".")
print(head(mdf2))


#Output
setwd("~/_Dev/")
write.csv(mdf2, file = "2015v.csv", row.names=FALSE)
#rm(list = ls())
