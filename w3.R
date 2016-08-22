library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2)

#----------Initial Variables


csvlistall = c(
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

fileloop <- function(csvlist) {
  
  #reading in the file
  setwd("~/_Dev/wData")
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  week = as.numeric(gsub( ',', '', item$week))
  wGross = as.numeric(gsub( ',', '', item$wGross))
  oc = as.numeric(gsub( ',', '', item$trs))
  
  #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print (name)
  
  #creating empty weekly_drop and decay_curve 
  wDrop = c(1:length(wGross))
  dCurve = c(1:length(wGross))
  
  #generating the curves
  for (i in 2:length(wGross)) {
  wDrop[i] = wGross[i] / wGross[i-1]
  dCurve[i] = wDrop[i] * dCurve[i-1]
  }
  
  #Oscar check
  if (oc[1] < oc[length(oc)]) {oscar = 1}
  else {oscar = 0}
  
  #return value
  output = cbind(name, week, dCurve, oscar)
  
  #setwd("~/_Dev/rData")
  #write.csv(output, file = csvlist, row.names=FALSE)

  
}

#-------Executing The Loop

#execute loop, convert from list to datafram
wlist <- lapply(csvlist, fileloop)


#------Formatting the Loop output

wlist <- ldply (wlist, data.frame)

#convert from factors to numerics
#then converting to tibble
factorconvert <- function(f){as.numeric(levels(f))[f]}
wlist[, 2:4] <- lapply(wlist[, 2:4], factorconvert)
wlist <- tbl_df(wlist)
print(summary(wlist))

#------Visualization

gp <- ggplot(wlist, aes(x=week, y=dCurve, col = name)) + geom_line() 
gp1 <- gp +theme(legend.position="bottom",legend.direction="vertical")
gp2 <- gp + theme(legend.position="none")

#print (gp)

#write.csv(wlist, file = "2015w.csv", row.names=FALSE)

#clear objects
#rm(list = ls())
