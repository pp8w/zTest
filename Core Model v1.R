library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization
library(corrplot)


#########################################################
#########################################################
#
#                   CORE MODELLING                         
#
#########################################################
#########################################################

#--------------------------------------------------------
#           Obtaining Train and Test
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
Tcon = read.csv("Data_Train.csv", header = TRUE, stringsAsFactors = FALSE)
Tcon2016 = read.csv("Data_Test.csv", header = TRUE, stringsAsFactors = FALSE)

#Setting Factor Levels
Tcon$G <- as.factor(Tcon$G)
Tcon$Rating <- as.factor(Tcon$Rating)
Tcon$fRank <- as.factor(Tcon$fRank)
Tcon$dCol <- as.factor(Tcon$dCol)
Tcon$rDate <- as.Date(Tcon$rDate)

#Setting Factor Levels
Tcon2016$G <- as.factor(Tcon2016$G)
Tcon2016$Rating <- as.factor(Tcon2016$Rating)
Tcon2016$fRank <- as.factor(Tcon2016$fRank)
Tcon2016$dCol <- as.factor(Tcon2016$dCol)
Tcon2016$rDate <- as.Date(Tcon2016$rDate)

#--------------------------------------------------------
#           Variable Initialization
#--------------------------------------------------------

dcon <-subset(Tcon, pExp>0 & rtUR >0 & trsB >0)
dcon <- tbl_df(Tcon)
dcon <- mutate(Tcon, predOpen = Open, predShape = shape, predTotal = dGross) #clones columns

#dcon2016 <-subset(Tcon2016, pExp>0 & rtUR >0 & trsB >0 & rDate < "2016-06-01")
#dcon2016 <-subset(Tcon2016, pExp>0 & rtUR >0 & trsB >0 & rDate < "2016-07-15")
dcon2016 <-subset(Tcon2016, pExp>0 & rtUR >0 & trsB >0 & rDate < "2016-08-01")
#dcon2016 <-subset(dcon2016, Title != "Deadpool")

dcon2016 <- tbl_df(dcon2016)


#--------------------------------------------------------
#           Shape Prediction
#--------------------------------------------------------

#Shape Stepwise Prediction
min.model <- lm(shape ~ rtUR, data=dcon)
big.model <- formula(lm(shape ~ rtUR + Open + trsA + trsB + G + Runtime + Rating + pExp + fRank + aCol + dCol ,dcon))
#shape.forward <- step(min.model, direction = 'forward', scope=big.model)

shape.model <- lm(shape ~ rtUR + G + Rating + trsA + Open + aCol + trsB + pExp, data = dcon)
#print(summary(shape.model)) #.333

#--------------------------------------------------------
#           Open Prediction
#--------------------------------------------------------

#---Stepwise For Open
open.min <- lm(Open ~ pExp, data=dcon)
open.max <- formula(lm(Open ~  pExp + rtUR + trsA + trsB + G + Runtime + Rating + fRank + aCol + dCol ,dcon))
#open.forward <- step(open.min, direction = 'forward', scope=open.max)

open.model<- lm(Open ~ pExp + fRank + trsA + rtUR + G + aCol + trsB, data = dcon)
#print(summary(open.model)) #.749

#--------------------------------------------------------
#           TOTAL - ONE STAGE - OPEN
#--------------------------------------------------------

#---Stepwise For Open
small.model <- lm(dGross ~ Open, data = dcon)
#print(summary(small.model)) #.88

#print("R2 Value via Open (ONE STAGE):")
#print(round(summary(small.model)$r.squared, digits =3))

#--------------------------------------------------------
#           TOTAL - ONE STAGE - FULL
#--------------------------------------------------------

#---Stepwise For Open
total.min <- lm(dGross ~ pExp, data=dcon)
total.max <- formula(lm(dGross ~  pExp + rtUR + trsA + trsB + G + Runtime + Rating + fRank + aCol + dCol ,dcon))
#total.forward <- step(total.min, direction = 'forward', scope=total.max)

total.model <- lm(dGross ~ pExp + fRank + rtUR + trsB + aCol + G + trsA, data = dcon)
#print(summary(total.model)) #.732

predGross1s <- predict(total.model, newdata = dcon2016)

#R2 Calculation
SSE <- sum((predGross1s - dcon2016$dGross)^2)
SST <- sum((mean(dcon$dGross) - dcon2016$dGross)^2)

#R2 Display
tr <- c(1:2)
tr[1] = round(summary(total.model)$r.squared, digits =3)
tr[2] = round(1 - (SSE/SST), digits =3)
print("Total R2 Value (ONE STAGE):")
print(tr)


#########################################################
#########################################################
#
#                   Prediction                        
#
#########################################################
#########################################################

#--------------------------------------------------------
#           Open Prediction
#--------------------------------------------------------

predOpen <- predict(open.model, newdata = dcon2016)
#print(summary(model36))

#R2 Calculation
SSE <- sum((predOpen - dcon2016$Open)^2)
SST <- sum((mean(dcon$Open) - dcon2016$Open)^2)

#R2 Display
openr <- c(1:2)
openr[1] = round(summary(open.model)$r.squared, digits =3)
openr[2] = round(1 - (SSE/SST), digits =3)
#print("------------------------------------")
#print("Opening R2 Value:")
#print(openr)

#Binding predicted opens


#--------------------------------------------------------
#           Shape Prediction
#--------------------------------------------------------

#Shape Model then Predict
predShape <- predict(shape.model, newdata = dcon2016)

#R2 Calculation
SSE <- sum((predShape - dcon2016$week)^2)
SST <- sum((mean(dcon$shape) - dcon2016$week)^2)

#R2 Display
shaper <- c(1:2)
shaper[1] = round(summary(shape.model)$r.squared, digits =3)
shaper[2] = round(1 - (SSE/SST), digits =3)

#print("Shape R2 estimates: ")
#print(shaper)
#print(round(summary(model55)$r.squared, digits =3))


#print (select(fresult, id, week, shape))

#--------------------------------------------------------
#           Total Prediction
#--------------------------------------------------------

dcon2016$predOpen <- predOpen
dcon2016$predShape <- predShape

modelt2 <- lm(predTotal ~ predOpen * exp(predShape), data = dcon)
predTotal <- predict(modelt2, newdata = dcon2016)
#print(summary(modelt2))

#R2 Calculation
SSE <- sum((predTotal - dcon2016$dGross)^2)
SST <- sum((mean(dcon$dGross) - dcon2016$dGross)^2)

totalR <- c(1:2)
totalR[1] = round(summary(modelt2)$r.squared, digits =3)
totalR[2] = round(1 - (SSE/SST), digits =3)
print("Total R2 Value (TWO STAGE):")
print(totalR)

dcon2016$predTotal <- predTotal

#########################################################
#########################################################
#
#                   Exploration                         
#
#########################################################
#########################################################


#--------------------------------------------------------
#                    One Stage
#--------------------------------------------------------

OnePred <- dcon2016[, names(dcon2016) %in% c("Title", "dGross", "G", "fRank")]
OnePred$predGross <- predGross1s


#New Columns
OnePred <- mutate(OnePred,  ds2 = (((predGross-dGross)/dGross)^2)^(1/2))
OnePred <- mutate(OnePred,  weights = dGross/sum(dGross))
OnePred <- mutate(OnePred,  adjError = ds2*weights)

#Formatting
OnePred$dGross = round(OnePred$dGross, digits = 2)
OnePred$predGross = round(OnePred$predGross, digits = 2)
OnePred$ds2 = round(OnePred$ds2, digits = 2)

#Key Stats
opStats <- c(1:3)
opStats[1] <- round(sum(OnePred$predGross)/sum(OnePred$dGross) , digits = 3) 
opStats[2] <- round(mean(OnePred$ds2) , digits = 3) 
opStats[3] <- round(sum(OnePred$adjError) , digits = 3) 
print("------------------------------------")
print("One Pred: Total / Error / Adj Error")
print(opStats)

#-----Franchise
oneF <- subset(OnePred, fRank!=0)

oneF$weights <- oneF$dGross/sum(oneF$dGross)
oneF$adjError <- oneF$ds2*oneF$weights

oFStats <- c(1:3)
oFStats[1] <- round(sum(oneF$predGross)/sum(oneF$dGross) , digits = 3) 
oFStats[2] <- round(mean(oneF$ds2) , digits = 3) 
oFStats[3] <- round(sum(oneF$adjError) , digits = 3) 
print("Franchises: Total / Error / Adj Error")
print(oFStats)

#-----Non Franchise
oneNF <- subset(OnePred, fRank==0)

oneNF$weights <- oneNF$dGross/sum(oneNF$dGross)
oneNF$adjError <- oneNF$ds2*oneNF$weights

oNStats <- c(1:3)
oNStats[1] <- round(sum(oneNF$predGross)/sum(oneNF$dGross) , digits = 3) 
oNStats[2] <- round(mean(oneNF$ds2) , digits = 3) 
oNStats[3] <- round(sum(oneNF$adjError) , digits = 3) 
print("Non Franchises: Total / Error / Adj Error")
print(oNStats)

#--------------------------------------------------------
#                    Two Stage
#--------------------------------------------------------

TwoPred <- dcon2016[, names(dcon2016) %in% c("Title", "dGross", "G", "predTotal","fRank")]

#New Columns
TwoPred <- mutate(TwoPred, ds2 = (((predTotal-dGross)/dGross)^2)^(1/2))
TwoPred <- mutate(TwoPred,  weights = dGross/sum(dGross))
TwoPred <- mutate(TwoPred,  adjError = ds2*weights)

#Formatting
TwoPred$dGross = round(TwoPred$dGross, digits = 2)
TwoPred$predTotal = round(TwoPred$predTotal, digits = 2)
TwoPred$ds2 = round(TwoPred$ds2, digits = 2)

#Key Stats
tpStats <- c(1:3)
tpStats[1] <- round(sum(TwoPred$predTotal)/sum(TwoPred$dGross) , digits = 3) 
tpStats[2] <- round(mean(TwoPred$ds2) , digits = 3) 
tpStats[3] <- round(sum(TwoPred$adjError) , digits = 3) 
print("------------------------------------")
print("Two Pred: Total / Error / Adj Error")
print(tpStats)

#-----#Franchises
twoF <- subset(TwoPred, fRank!=0)

twoF$weights <- twoF$dGross/sum(twoF$dGross)
twoF$adjError <- twoF$ds2*twoF$weights

tFStats <- c(1:3)
tFStats[1] <- round(sum(twoF$predTotal)/sum(twoF$dGross) , digits = 3) 
tFStats[2] <- round(mean(twoF$ds2) , digits = 3) 
tFStats[3] <- round(sum(twoF$adjError) , digits = 3) 
print("Franchises: Total / Error / Adj Error")
print(tFStats)

#-----Non Franchise
twoNF <- subset(TwoPred, fRank==0)

twoNF$weights <- twoNF$dGross/sum(twoNF$dGross)
twoNF$adjError <- twoNF$ds2*twoNF$weights

tNFStats <- c(1:3)
tNFStats[1] <- round(sum(twoNF$predTotal)/sum(twoNF$dGross) , digits = 3) 
tNFStats[2] <- round(mean(twoNF$ds2) , digits = 3) 
tNFStats[3] <- round(sum(twoNF$adjError) , digits = 3) 
print("Non Franchises: Total / Error / Adj Error")
print(tNFStats)

#--------------------------------------------------------
#                    Shape
#--------------------------------------------------------

eshape <- dcon2016[, names(dcon2016) %in% c("Title", "dGross", "G", "week",  "predShape")]
eshape <- mutate(eshape, diff = week - predShape, dscore = (predShape-week)/week, ds2 = (dscore^2)^(1/2))

eshape$dGross = round(eshape$dGross, digits = 1)
eshape$diff = round(eshape$diff, digits = 3)
eshape$dscore = round(eshape$dscore, digits = 2)
eshape$ds2 = round(eshape$ds2, digits = 2)

#--------------------------------------------------------
#                    Visualization
#--------------------------------------------------------

x1 <- xlim(0,1)
x11 <- xlim(-1,1)

#d10 <- ggplot(eshape, aes(y=dGross, x= diff, col = G)) + geom_point() + ggtitle("Diff by Gross")
d11 <- ggplot(eshape, aes(y=dGross, x= ds2, col = G)) + geom_point() + ggtitle("Percent Error")

#d12 <- ggplot(eshape, aes(y=dGross, x= ds2, col = G)) + geom_point() + ggtitle("DS2 by Gross")
#d13 <- ggplot(eshape, aes(y=dGross, x= dpct, col = G)) + geom_point() + ggtitle("DS2 by Gross")

#print(d10) #Difference by Gross
#print(d11) #Dscore by Gross
#print(d11 + x1) #Dscore by gross


d20 <- ggplot(eshape, aes(ds2)) + 
  geom_histogram(binwidth = .20, position = "stack") +
  ggtitle("DS2 by Shape") + 
  stat_bin(binwidth=.20, geom="text",vjust=-.2, aes(label=..count..))

d21 <- ggplot(eshape, aes(ds2, fill = G)) + 
  geom_histogram(binwidth = .20, position = "stack") + 
  ggtitle("DS2 by Shape") 

#print(d20)
#print(d21)


#--------------------------------------------------------
#                    Output
#--------------------------------------------------------


write.csv(dcon2016, "Test_Results.csv", row.names = FALSE)
#write.csv(eshape, "eshape.csv", row.names = FALSE)
