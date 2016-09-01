library(dplyr)
library(tidyr)
library(plyr) #conversion
library(ggplot2)


#--------------------------------------------------------
#             Initiliaztion - from weekly
#--------------------------------------------------------

csvlist = jdata$name
csvlist <- paste(csvlist,".csv",sep='')
csvlist <- unique(csvlist)

#--------------------------------------------------------
#             Header Reading Function
#--------------------------------------------------------

setwd("~/_Dev/hData")
headerloop <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = FALSE, stringsAsFactors = FALSE)

}

#--------------------------------------------------------
#             Executing Header Function
#--------------------------------------------------------

mlist <- lapply(csvlist, headerloop) ##### MAIN LOOP #####
mdf <- ldply (mlist, data.frame)

#updating column headers
colnames(mdf) = c('id', 'adj', 'Name', 'dGross', 
                  'dist', 'rDate', 'G', 'Runtime',  'Rating', 'pBudget')
mdf <- mdf[ , !names(mdf) %in% c("adj","dist")]
mdf <- tbl_df(mdf)

mdft <- mdf

#--------------------------------------------------------
#             Cleaning Header Results
#--------------------------------------------------------

#HTML replacements - removing html fragments
mdf <- as.data.frame(sapply(mdf,gsub,pattern="<b>|</b>|<br/>|<i>|</i>|\\$",replacement=""))
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

pExp <- sapply(mdf$pBudget, function(x) eval(parse(text=x))) #expression to value
pExp = pExp / 1000000 # converting to millions
pExp[pExp==0] <- NA #replacing 0 with NA for log
mdf <- cbind(mdf, pExp) #binding to frame
mdf <- mdf[ , !names(mdf) %in% c("pBudget")] #removing old column


#Runtime - Changing from "[X hrs Y min]" to "[Z]", where Z is minutes
#Converting to expression then evaluating the expression
mdf$Runtime <- as.character(gsub( ' min.', '', mdf$Runtime))
mdf$Runtime <- as.character(gsub( ' hrs. ', '\\*60+', mdf$Runtime))
mdf$Runtime <- as.character(gsub( 'N/A', '0', mdf$Runtime))

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


#Preparing For Franchises
nosp = mdf$Name
nosp = gsub(' ','', nosp)
nosp = gsub(':','', nosp)
#nospcol = gsub('\\(.*','', nospcol)
#print(nospcol)
mdf <- cbind(mdf,nosp)



  

#--------------------------------------------------------
#             Franchise Function
#--------------------------------------------------------

Fgen <- function() {
csvflist = c(
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
return (csvflist)
}

csvflist = Fgen()

#----------File Loop
setwd("~/_Dev/fData")
franchiseloop <- function(csvlist) {
  
  #reading in the file
  item = read.csv(csvlist, header = TRUE, stringsAsFactors = FALSE)
  
  name = gsub('.csv','',csvlist)
  
  return(cbind(name,item))
}

#-------Executing The Loop

flist <- lapply(csvflist, franchiseloop) ##### MAIN LOOP #####
fdf <- ldply (flist, data.frame)


#--------------------------------------------------------
#             Cleaning Franchise Results
#--------------------------------------------------------

nosp= fdf$name.1
nosp = as.character(gsub( ' ', '', nosp))
nosp = as.character(gsub( ':', '', nosp))
franchise = c(1:length(nosp))
franchise = 1

fdf = cbind(fdf, nosp, franchise)
#print (head(fdf))

#----------Joining Franchise and Header

mdf <-join(mdf, fdf, by='nosp')
mdf <- mdf[ , !names(mdf) %in% c("name.1","date")]
mdf$franchise <- as.numeric(mdf$franchise)
mdf <- distinct(mdf)
#print(filter(mdf, franchise==1))

#r2tgdata comes from weekly code
mdf <-join(r2tgdata, mdf, by='id')
mdf$franchise[is.na(mdf$franchise)] <- 0
mdf$franchise <- as.factor(mdf$franchise)
print(head(mdf))
mdf <- distinct(mdf)


#--------------------------------------------------------
#             Output
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
write.csv(mdf, file = "HeaderData.csv", row.names=FALSE) #franchise and header
write.csv(fdf, file = "FranchiseData.csv", row.names=FALSE) #franchise
#rm(list = ls())
