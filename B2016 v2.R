library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization
library(broom) #coefficient extraction

#########################################################
#########################################################
#
#                   WEEKLY CODE BLOCK                         
#
#########################################################
#########################################################


#--------------------------------------------------------
#             Initial Variables
#--------------------------------------------------------


LGen <- function (){
  csvlist2016 = c(
    "13hoursthesecretsoldiersofbenghazi.csv",
    "5thwave.csv",
    "abeautifulplanet.csv",
    "absolutelyfabulousthemovie.csv",
    "ahologramfortheking.csv",
    "alice2.csv",
    "allegiant.csv",
    "angrybirds.csv",
    "armsandthedudes.csv",
    "badrobot2016.csv",
    "barbershop3.csv",
    "benhur2016.csv",
    "bfg.csv",
    "bhtilt2016.csv",
    "bourne5.csv",
    "boy2016.csv",
    "cafesociety.csv",
    "captainfantastic.csv",
    "centralintelligence.csv",
    "christthelord.csv",
    "conjuring2.csv",
    "criminal2015.csv",
    "dc2016.csv",
    "deadpool2016.csv",
    "dirtygrandpa.csv",
    "disney2016.csv",
    "dontbreathe.csv",
    "dontbreathe.csv",
    "eddietheeagle.csv",
    "eyeinthesky.csv",
    "fiftyshadesofblack.csv",
    "finesthours.csv",
    "florencefosterjenkins.csv",
    "forest.csv",
    "freestateofjones.csv",
    "ghostbusters2016.csv",
    "godsnotdead2.csv",
    "godsofegypt.csv",
    "grimsby.csv",
    "hailcaesar.csv",
    "hardcorehenry.csv",
    "hello.csv",
    "hellorhighwater.csv",
    "hillarysamerica.csv",
    "howtobesingle.csv",
    "huntforwilderpeople.csv",
    "huntsman.csv",
    "iceage5.csv",
    "id42.csv",
    "illumination2015.csv",
    "junglebook2015.csv",
    "keanu.csv",
    "kuboandthetwostrings.csv",
    "kungfupanda3.csv",
    "londonhasfallen.csv",
    "lovefriendship.csv",
    "marvel2016.csv",
    "mebeforeyou.csv",
    "mechanic2.csv",
    "meettheblacks.csv",
    "michelledarnell.csv",
    "mikeanddave.csv",
    "miraclesfromheaven.csv",
    "moneymonster.csv",
    "mothersday.csv",
    "mybigfatgreekwedding2.csv",
    "nerve.csv",
    "newline0116.csv",
    "niceguys.csv",
    "ninelives2016.csv",
    "normofthenorth.csv",
    "nowyouseeme2.csv",
    "petesdragon2016.csv",
    "pixar2015.csv",
    "prideprejudicezombies.csv",
    "purge3.csv",
    "race2016.csv",
    "ratchetandclank.csv",
    "ridealong2.csv",
    "risen.csv",
    "sausageparty.csv",
    "startrek2016.csv",
    "sultan.csv",
    "superman2015.csv",
    "tarzan2016.csv",
    "thechoice.csv",
    "theinfiltrator.csv",
    "themeddler.csv",
    "theperfectmatch.csv",
    "theshallows.csv",
    "thewitch.csv",
    "tmnt2016.csv",
    "topsecretuntitledlonelyislandmovie.csv",
    "triplenine.csv",
    "universalcomedy2016.csv",
    "untitledlucasmoore.csv",
    "untitledtinafeycomedy.csv",
    "warcraft.csv",
    "x-men2016.csv",
    "zoolander2.csv"
  )
  return (csvlist2016)
}

csvlist2016 = LGen()
setwd("~/_Dev/2016w")


#--------------------------------------------------------
#             Error Checking Function
#--------------------------------------------------------

PreChecks <- function(csvlist2016) {
  
  #reading in the file
  item = read.csv(csvlist2016, header = TRUE, stringsAsFactors = FALSE)
  wk = as.numeric(gsub( ',', '', item$week))
  #print(csvlist2016)
  
  #default values
  error = 0
  weekZ = 0
  
  #checks based on theatres/weeks
  if (length(wk)==0) 
  {error = 1} 
  else if (length(wk)==1) 
  {error = 1}
  else if (length(wk)==2) 
  {error = 1}
  else if (wk[1]==0) 
  {weekZ = 1}
  else {weekZ = 0}
  
  #cleaning name for output
  name = gsub('.csv','',csvlist2016)
  #print (name)
  output = cbind(name, error, weekZ)
  
  return(output)
  
}

#--------------------------------------------------------
#             Cleaning Precheck List
#--------------------------------------------------------

#execute loop, convert from list to datafram
pclist2016 <- lapply(csvlist2016, PreChecks)    ##### MAIN LOOP #####
pclist2016 <- ldply (pclist2016, data.frame)

#Converting --- warning numerical values used
pclist2016$error <- as.numeric(as.character(pclist2016$error))
pclist2016$weekZ <- as.numeric(as.character(pclist2016$weekZ))
pclist2016<- mutate(pclist2016, flag=error + weekZ)
pclist2016n <- paste(pclist2016$name, '.csv',sep='')
#print(arrange(pclist2016, desc(flag)))

#List of error free items
oklist2016 <-pclist2016[pclist2016$flag ==0, ]
oklist2016n <- paste(oklist2016$name, '.csv',sep='')
#print (oklist2016)

#List of zero week items
zlist2016 <-pclist2016[pclist2016$weekZ ==1, ]
zlist2016n <- paste(zlist2016$name, '.csv',sep='')

#--------------------------------------------------------
#             Curve Generation Function
#--------------------------------------------------------

CurveGen <- function(csvlist2016, flag) {
  
  #reading in the file
  setwd("~/_Dev/2016w")
  item = read.csv(csvlist2016, header = TRUE, stringsAsFactors = FALSE)
  week = as.numeric(gsub( ',', '', item$week))
  wGross = as.numeric(gsub( ',', '', item$wGross))
  trs = as.numeric(gsub( ',', '', item$trs))
  
  
  #cleaning name for output
  name = gsub('.csv','',csvlist2016)
  #print (name)
  
  
  #creating empty weekly_drop and decay_curve 
  wDrop = c(1:length(wGross))
  dCurve = c(1:length(wGross))
  
  if (flag ==1)
  {
    #flag indicates a zero partial week
    #these are consolidated
    wGross[2] = wGross[2] + wGross[1]
    wGross[1] = wGross[2]
    
    #generating the curves
    for (i in 2:length(wGross)) 
    {
      wDrop[i] = wGross[i] / wGross[i-1]
      dCurve[i] = wDrop[i] * dCurve[i-1]
    }
    
    #fixes signpost error
    week[length(week)] = week[length(week) - 1] + 1
    
    #return value, removes duplicative row
    
    output = cbind(name, week, wGross, dCurve, trs)
    output = output[-1,] 
    setwd("~/_Dev/2016r")
    write.csv(output, file = csvlist2016, row.names=FALSE)
    
    return(output)
  }
  
  
  #generating the curves
  for (i in 2:length(wGross)) {
    wDrop[i] = wGross[i] / wGross[i-1]
    dCurve[i] = wDrop[i] * dCurve[i-1]
  }
  
  #fixes signpost error
  week[length(week)] = week[length(week) - 1] + 1
  
  #return value
  output = cbind(name, week, wGross, dCurve, trs)
  
  setwd("~/_Dev/2016r")
  write.csv(output, file = csvlist2016, row.names=FALSE)
  
  return(output)
  
}



#--------------------------------------------------------
#             List Combination
#--------------------------------------------------------


#Creating curves for zero items
#zdata <- lapply(zlist2016n, CurveGen, flag=1) ##### MAIN LOOP #####
#zdata <- ldply (zdata, data.frame)
#zdata$week <- as.numeric(as.character(zdata$week))
#zdata$wGross <- as.numeric(as.character(zdata$wGross))
#zdata$dCurve <- as.numeric(as.character(zdata$dCurve))

#Creating curves for ok items
curveData2016 <- lapply(oklist2016n, CurveGen, flag=0) ##### MAIN LOOP #####
curveData2016 <- ldply (curveData2016, data.frame)
curveData2016$week <- as.numeric(as.character(curveData2016$week))
curveData2016$wGross <- as.numeric(as.character(curveData2016$wGross))
curveData2016$dCurve <- as.numeric(as.character(curveData2016$dCurve))

#Combining those sets
#jdata <- rbind(zdata, curveData2016)
#jdata$week <- jdata$week - 1 #indexing to 0
#jdata<- tbl_df(jdata)
#jdata<- unique(jdata)

#--------------------------------------------------------
#             Opening Week & Theatres
#--------------------------------------------------------

tlist2016n <- curveData2016$name
tlist2016n <- paste(tlist2016n,".csv",sep='')
tlist2016n <- unique(tlist2016n)
tlist2016n2 <- tlist2016n

OpenCalc <- function(list){
  setwd("~/_Dev/2016r")
  item = read.csv(list, header = TRUE, stringsAsFactors = FALSE)
  wGross = as.numeric(gsub( ',', '', item$wGross))
  trs = as.numeric(gsub( ',', '', item$trs))
  
  id = gsub('.csv','',list)
  Open = wGross[1]
  trsA = trs[1]
  trsB = max(trs)
  #  Total = sum(wGross)
  output<- cbind(id,Open, trsA, trsB)
}

#Running Open Calc
tlist2016n <- lapply(tlist2016n,OpenCalc) ##### MAIN LOOP #####
tlist2016n <- ldply(tlist2016n, data.frame)

#Cleaning Output
tlist2016n$Open <- as.numeric(as.character(tlist2016n$Open))
tlist2016n$Open <- tlist2016n$Open / 1000000
tlist2016n$trsA <- as.numeric(as.character(tlist2016n$trsA))
tlist2016n$trsB <- as.numeric(as.character(tlist2016n$trsB))
#print(tlist2016n)




#--------------------------------------------------------
#             R2 Calculation
#--------------------------------------------------------

r2loop <- function(csvlist2016) {
  
  #reading in the file
  setwd("~/_Dev/2016r")
  item = read.csv(csvlist2016, header = TRUE, stringsAsFactors = FALSE)
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
  id = gsub('.csv','',csvlist2016)
  #print(name)
  
  #return value
  output = cbind(id, r2, coeffs)
  
}

#--------------------------------------------------------
#             R2 Cleaning
#--------------------------------------------------------

r2list2016 <- lapply(tlist2016n2, r2loop) ##### MAIN LOOP #####
r2list2016 <- ldply (r2list2016, data.frame)

r2list2016 <- tbl_df(r2list2016)
#colnames(r2df2016) <- c("id", "r2", "int", "shape")


#updating column names
#print(r2df2016)

#Joining with Total Gross Data
openData2016 <-join(r2list2016, tlist2016n, by='id')
print(head(openData2016))
openData2016 <- distinct(openData2016)


#--------------------------------------------------------
#             Output To File
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
#write.csv(openData2016, file = "2016Open.csv", row.names=FALSE) #r2 & tg info
#write.csv(curveData2016, file = "2016Curves.csv", row.names=FALSE) #weekly
#rm(list = ls())

#########################################################
#########################################################
#
#                   HEADER CODE BLOCK                         
#
#########################################################
#########################################################


#--------------------------------------------------------
#             Initiliaztion - from weekly
#--------------------------------------------------------

csvlist2016 = curveData2016$name
csvlist2016 <- paste(csvlist2016,".csv",sep='')
csvlist2016 <- unique(csvlist2016)

#--------------------------------------------------------
#             Header Reading Function
#--------------------------------------------------------

setwd("~/_Dev/2016h")
headerloop <- function(csvlist2016) {
  
  #reading in the file
  item = read.csv(csvlist2016, header = FALSE, stringsAsFactors = FALSE)
  
}

#--------------------------------------------------------
#             Executing Header Function
#--------------------------------------------------------

mdf2016 <- lapply(csvlist2016, headerloop) ##### MAIN LOOP #####
mdf2016 <- ldply (mdf2016, data.frame)

#updating column headers
colnames(mdf2016) = c('id', 'adj', 'Name', 'dGross', 'dist', 'rDate', 'G', 'Runtime',  'Rating', 'pBudget')
mdf2016 <- mdf2016[ , !names(mdf2016) %in% c("adj","dist")]
mdf2016 <- tbl_df(mdf2016)

#--------------------------------------------------------
#             Cleaning Header Results
#--------------------------------------------------------

#HTML replacements - removing html fragments
mdf2016 <- as.data.frame(sapply(mdf2016,gsub,pattern="<b>|</b>|<br/>|<i>|</i>|\\$",replacement=""))
mdf2016 <- as.data.frame(sapply(mdf2016,gsub,pattern=".*date=",replacement="")) #date specific
mdf2016 <- as.data.frame(sapply(mdf2016,gsub,pattern="&amp;p=.htm\">.*",replacement="")) #date specific
#print(head(mdf2016))


#Gross- changing from character to numerics
mdf2016$dGross <- as.character(gsub( ' \\(Estimate\\)', '', mdf2016$dGross))
mdf2016$dGross <- as.numeric(as.character(gsub( ',', '', mdf2016$dGross)))
mdf2016$dGross <- mdf2016$dGross / 1000000 # converting to millions


#Budget - removing millions text then converting to numeric
mdf2016$pBudget <- as.character(gsub( ' million', '\\*1000000', mdf2016$pBudget))
mdf2016$pBudget <- as.character(gsub( 'N/A', '0', mdf2016$pBudget)) #Sets NA to 0
mdf2016$pBudget <- as.character(gsub( ',', '', mdf2016$pBudget))

pExp <- sapply(mdf2016$pBudget, function(x) eval(parse(text=x))) #expression to value
pExp = pExp / 1000000 # converting to millions
pExp[pExp==0] <- NA #replacing 0 with NA for log
mdf2016 <- cbind(mdf2016, pExp) #binding to frame
mdf2016 <- mdf2016[ , !names(mdf2016) %in% c("pBudget")] #removing old column


#Runtime - Changing from "[X hrs Y min]" to "[Z]", where Z is minutes
#Converting to expression then evaluating the expression
mdf2016$Runtime <- as.character(gsub( ' min.', '', mdf2016$Runtime))
mdf2016$Runtime <- as.character(gsub( ' hrs. ', '\\*60+', mdf2016$Runtime))
mdf2016$Runtime <- as.character(gsub( 'N/A', '0', mdf2016$Runtime))

rtExp2016 <- sapply(mdf2016$Runtime, function(x) eval(parse(text=x))) #expression to value
mdf2016$Runtime <- as.numeric(rtExp2016)


#Simple column conversions
mdf2016$id <- as.character(mdf2016$id)
mdf2016$Name <- as.character(mdf2016$Name)
mdf2016$rDate <- as.Date(mdf2016$rDate)


#Mild replacements
mdf2016$Rating <- as.character(gsub( '-', '', mdf2016$Rating))
mdf2016$G <- as.character(gsub( ' ', '', mdf2016$G))
mdf2016$G <- as.character(gsub( '/', '', mdf2016$G))

#print(head(mdf2016))

#Preparing For Franchises
nosp = mdf2016$Name
nosp = gsub(' ','', nosp)
nosp = gsub(':','', nosp)
#nospcol = gsub('\\(.*','', nospcol)
#print(nospcol)
mdf2016 <- cbind(mdf2016,nosp)

#--------------------------------------------------------
#             Joining Franchise and Header
#--------------------------------------------------------


mdf2016 <-join(mdf2016, fdf, by='nosp')
mdf2016 <- mdf2016[ , !names(mdf2016) %in% c("name.1","date")]
mdf2016$fRank <- as.numeric(mdf2016$fRank)
mdf2016 <- distinct(mdf2016)
#print(filter(mdf2016, franchise==1))

#openData comes from weekly code
mdf2016 <-join(openData2016, mdf2016, by='id')
mdf2016 <- distinct(mdf2016)

mdf2016 <- subset(mdf2016, pExp >0)


names(mdf2016)[names(mdf2016)=="name"] <- "fName"
print(head(mdf2016))


#--------------------------------------------------------
#             Output
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
#write.csv(mdf2016, file = "BOM_2016.csv", row.names=FALSE) #franchise and header
#write.csv(fdf, file = "FranchiseData.csv", row.names=FALSE) #franchise
#rm(list = ls())

