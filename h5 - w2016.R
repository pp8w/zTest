library(plyr) #conversion
library(dplyr)
library(tidyr)
library(ggplot2)


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
mdf2016$franchise <- as.numeric(mdf2016$franchise)
mdf2016 <- distinct(mdf2016)
#print(filter(mdf2016, franchise==1))

#openData comes from weekly code
mdf2016 <-join(openData2016, mdf2016, by='id')
mdf2016$franchise[is.na(mdf2016$franchise)] <- 0
mdf2016$franchise <- as.factor(mdf2016$franchise)
#print(head(mdf2016))
mdf2016 <- distinct(mdf2016)


#--------------------------------------------------------
#             Output
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
write.csv(mdf2016, file = "Header2016.csv", row.names=FALSE) #franchise and header
#write.csv(fdf, file = "FranchiseData.csv", row.names=FALSE) #franchise
#rm(list = ls())
