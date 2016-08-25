library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2)

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


#bigshort, brooklyn, ex machina, 
#revenant, spotlight, sicario, woman in gold


#----------Exception Checks

PreChecks <- function(csvlist) {
  
  #reading in the file
  setwd("~/_Dev/wData")
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  wk = as.numeric(gsub( ',', '', item$week))
  oc = as.numeric(gsub( ',', '', item$trs))
  
  #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print (name)

  #Oscar check
  if (oc[1] < oc[length(oc)]) {oscar = 1}
  else {oscar = 0}
  
  if (wk[1] ==0) {weekZ = 1}
  else {weekZ = 0}
  
  #return value
  output = cbind(name, weekZ, oscar)
  
  return(output)
  
}

#-------Exception Check Result Cleaning

#execute loop, convert from list to datafram
pclist <- lapply(csvlist, PreChecks)
pclist <- ldply (pclist, data.frame)
factorconvert <- function(f){as.numeric(levels(f))[f]}
pclist[, 2:3] <- lapply(pclist[, 2:3], factorconvert)
pclist<- mutate(pclist, flag=weekZ + oscar)
#print(arrange(pclist, desc(flag)))

#Non Oscar, Non Zero
oklist <-pclist[pclist$flag ==0, ]
oklistn <- paste(oklist$name, '.csv',sep='')
#print (oklist)

#Zero List
zlist <-pclist[pclist$weekZ ==1, ]
#print(arrange(zlist, desc(oscar)))
zlist <-zlist[ !(zlist$name=="brooklyn"),]
zlist <-zlist[ !(zlist$name=="womaningold"),]
zlistn <- paste(zlist$name, '.csv',sep='')

#-------Curve Generating Function

CurveGen <- function(csvlist, flag) {
  
  #reading in the file
  setwd("~/_Dev/wData")
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  week = as.numeric(gsub( ',', '', item$week))
  wGross = as.numeric(gsub( ',', '', item$wGross))

  #cleaning name for output
  name = gsub('.csv','',csvlist)
  #print (name)
  
  
  #creating empty weekly_drop and decay_curve 
  wDrop = c(1:length(wGross))
  dCurve = c(1:length(wGross))
  
  if (flag ==1)
       {
          wGross[2] = wGross[2] + wGross[1]
          wGross[1] = wGross[2]
    
          #generating the curves
          for (i in 2:length(wGross)) 
            {
              wDrop[i] = wGross[i] / wGross[i-1]
              dCurve[i] = wDrop[i] * dCurve[i-1]
            }
          
          #return value
          output = cbind(name, week, wGross, dCurve)
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
  
  #return value
  output = cbind(name, week, wGross, dCurve)

  #setwd("~/_Dev/rData")
  #write.csv(output, file = csvlist, row.names=FALSE)
  
  return(output)
  
}




#-----------Results of Curve Gen

Nconvert <- function(f){as.numeric(levels(f))[f]}

#Creating curves for zero items
zdata <- lapply(zlistn, CurveGen, flag=1)
zdata <- ldply (zdata, data.frame)
zdata[, 2:4] <- lapply(zdata[, 2:4], Nconvert)

#Creating curves for ok items
gdata <- lapply(oklistn, CurveGen, flag=0)
gdata <- ldply (gdata, data.frame)
gdata[, 2:4] <- lapply(gdata[, 2:4], Nconvert)

#Combining those sets
jdata <- rbind(zdata, gdata)
jdata$week <- jdata$week - 1 #indexing to 0
jdata<- tbl_df(jdata)

#---------Total Box Office Calculator

tlistn <- jdata$name
tlistn <- paste(tlistn,".csv",sep='')
tlistn <- unique(tlistn)
print(tlistn)

TBOCalc <- function(list){
  setwd("~/_Dev/wData")
  item = read.csv(list, header = TRUE, stringsAsFactors = FALSE)
  wGross = as.numeric(gsub( ',', '', item$wGross))
  
  name = gsub('.csv','',list)
  Total = sum(wGross)
  
  output<- cbind(name,Total)
  print(output)
}

tlistn <- lapply(tlistn,TBOCalc)
tlistn <- ldply (tlistn, data.frame)
print(tlistn)

#------Visualization

gp <- ggplot(jdata, aes(x=week, y=dCurve, col = name)) + geom_line() 
gp1 <- gp +theme(legend.position="bottom",legend.direction="vertical")
gp2 <- gp + theme(legend.position="none")

print (gp2)