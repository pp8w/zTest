library(dplyr)
library(tidyr)
library(plyr) #conversion
library(ggplot2)
library(dummies) #dummy var creation

#----------Initial Variables

csvlist = c(
  "xxxtrilogy.csv",
  "xmen.csv",
  "workandtheglory.csv",
  "wolverine.csv",
  "witchmountain.csv",
  "wimpykid.csv",
  "vhs.csv",
  "vacation.csv",
  "universalsoldier.csv",
  "underworld.csv",
  "twilight.csv",
  "transporter.csv",
  "transformers.csv",
  "toystory.csv",
  "thor.csv",
  "texaschainsawmassacre.csv",
  "terminator.csv",
  "teenagemutantninjaturtles.csv",
  "taken.csv",
  "superman.csv",
  "stepup.csv",
  "starwars.csv",
  "startrek.csv",
  "spykids.csv",
  "spiderman.csv",
  "smurfs.csv",
  "smokeyandthebandit.csv",
  "sinister.csv",
  "shrek.csv",
  "shiloh.csv",
  "scream.csv",
  "scoobydoo.csv",
  "scarymovie.csv",
  "saw.csv",
  "santaclause.csv",
  "rushhour.csv",
  "rugrats.csv",
  "rocky.csv",
  "robocop.csv",
  "riddick.csv",
  "returnofthelivingdead.csv",
  "residentevil.csv",
  "rambo.csv",
  "purge.csv",
  "psycho.csv",
  "predator.csv",
  "porkys.csv",
  "poltergeist.csv",
  "policeacademy.csv",
  "pokemon.csv",
  "planetoftheapes.csv",
  "pixarcars.csv",
  "pitchperfectseries.csv",
  "piratesofthecaribbean.csv",
  "pinkpanther.csv",
  "paranormalactivity.csv",
  "ongbak.csv",
  "omen.csv",
  "ohgod.csv",
  "oceans.csv",
  "nightmuseum.csv",
  "nightmareonelmstreet.csv",
  "neverendingstory.csv",
  "nakedgun.csv",
  "muppets.csv",
  "mummy.csv",
  "missionimpossible.csv",
  "missinginaction.csv",
  "millennium.csv",
  "mightyducks.csv",
  "middleearth.csv",
  "meninblackseries.csv",
  "meatballs.csv",
  "mazerunner.csv",
  "matrix.csv",
  "mariachi.csv",
  "majorleague.csv",
  "madmax.csv",
  "madea.csv",
  "madagascar.csv",
  "lovebug.csv",
  "lordoftherings.csv",
  "lookwhostalking.csv",
  "lethalweapon.csv",
  "lego.csv",
  "langdon.csv",
  "kungfupanda.csv",
  "kingsman.csv",
  "karatekid.csv",
  "jurassicpark.csv",
  "jaws.csv",
  "jamesbond.csv",
  "jackryan.csv",
  "jackass.csv",
  "ironmanfranchise.csv",
  "ironeagle.csv",
  "ipmanseries.csv",
  "insidious.csv",
  "indianajones.csv",
  "iceage.csv",
  "hungergames.csv",
  "httyd.csv",
  "houseparty.csv",
  "hoteltransylvania.csv",
  "homealone.csv",
  "hobbit.csv",
  "highlander.csv",
  "hellraiser.csv",
  "harrypotter.csv",
  "haroldandkumar.csv",
  "hannibal.csv",
  "hangover.csv",
  "halloween.csv",
  "godzillakong.csv",
  "godzilla.csv",
  "godfather.csv",
  "ghostbusters.csv",
  "frightnight.csv",
  "fridaythe13th.csv",
  "friday.csv",
  "freewilly.csv",
  "fockers.csv",
  "finaldestination.csv",
  "fiftyshades.csv",
  "fastandthefurious.csv",
  "fantasticfour.csv",
  "expendables.csv",
  "exorcist.csv",
  "evildead.csv",
  "evangelion.csv",
  "ernest.csv",
  "dumbdumber.csv",
  "divergent.csv",
  "dirtyharry.csv",
  "diehard.csv",
  "dhoom.csv",
  "despicableme.csv",
  "dc.csv",
  "darkknighttrilogy.csv",
  "crocodiledundee.csv",
  "conjuringfranchise.csv",
  "conan.csv",
  "chroniclesofnarnia.csv",
  "childsplay.csv",
  "carebears.csv",
  "captainamerica.csv",
  "bridgetjones.csv",
  "bourne.csv",
  "blade.csv",
  "bigmomma.csv",
  "beverlyhillscop.csv",
  "bestman.csv",
  "benji.csv",
  "before.csv",
  "batman.csv",
  "barbershop.csv",
  "badnewsbears.csv",
  "badboys.csv",
  "backtothefuture.csv",
  "avengersfranchise.csv",
  "avengers.csv",
  "austinpowers.csv",
  "atlasshrugg.csv",
  "arthur.csv",
  "amityville.csv",
  "americanpie.csv",
  "americanninja.csv",
  "alvinseries.csv",
  "alien.csv",
  "alexcross.csv",
  "3ninjas.csv"
)

#----------File Loop
setwd("~/_Dev/fData")
fileloop <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)

}

#-------Executing The Loop

flist <- lapply(csvlist, fileloop)
fdf <- ldply (flist, data.frame)



#----- Cleaning the contents

nosp= fdf$name
nosp = as.character(gsub( ' ', '', nosp))
onecol = c(1:length(nosp))
onecol = 1

fdf = cbind(fdf, nosp, onecol)
print (head(fdf))

#------- Output
setwd("~/_Dev/")
write.csv(fdf, file = "2015f.csv", row.names=FALSE)
#rm(list = ls())
