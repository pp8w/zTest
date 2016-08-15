mitem = read.csv("1952.csv", header = TRUE, stringsAsFactors = FALSE)
it2 = as.numeric(gsub( ',', '', mitem$cGross))
cMax = max(it2)

gRatio = it2 / cMax
dCurve = 1-gRatio

mitem <- cbind(mitem, gRatio, dCurve)





#----------Initial Variables

csvlist = c(
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
            "dc2016.csv",
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
            "xmas2015.csv")
setwd("~/_Dev/mData")
a = 1
dcdf = data.frame(a) 

#----------File Loop

fileloop <- function(csvlist, mitem) {
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  it2 = as.numeric(gsub( ',', '', item$cGross))
  cMax = max(it2)
  
  gRatio = it2 / cMax
  dCurve = 1-gRatio
  
  item <- cbind(item, gRatio, dCurve)
  print(csvlist)
  print(item)
  
  setwd("~/_Dev/rData")
 
  #print(output)
  write.csv(item, file = csvlist, row.names=FALSE)
  setwd("~/_Dev/mData")
  
}

#-------Executing The Loop

runloop <- lapply(fileloop, csvlist, mitem)
#print(master)







