library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction

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

fileloop <- function(csvlist) {
  
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
  r2 = summary(mlm)$r.squared
 
   #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print(name)
  
  #return value
  output = cbind(name, r2, coeffs)

}

#-------Executing The Loop

r2list <- lapply(csvlist, fileloop)
r2df <- ldply (r2list, data.frame)
factorconvert <- function(f){as.numeric(levels(f))[f]}
r2df[, 2:4] <- lapply(r2df[, 2:4], factorconvert)
r2df <- tbl_df(r2df)

print(r2df)

setwd("~/_Dev/")
write.csv(r2df, file = "2015r2.csv", row.names=FALSE)
