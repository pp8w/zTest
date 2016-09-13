library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization


#--------------------------------------------------------
#                    Variable Initialization
#--------------------------------------------------------

df <-subset(combod, pExp>0 & rtUR >0 )
df <- mutate(df, predOpen = Open, predTotal = dGross) #clones columns
df2016 <-subset(combod2016, pExp>0 & rtUR >0 & rDate < "2016-06-01")

#--------------------------------------------------------
#                    Test and Validation Set
#--------------------------------------------------------


library(caTools)
set.seed(46)

split = sample.split(df$Open, SplitRatio = 0.80) 
Train = subset(df, split == TRUE) #618
Test = subset(df, split == FALSE) #155


#----- Open
model35 <- lm(Open ~ pExp + franchise + G + rtUR + tDiff +  aCol + dCol, data = Train)
predOpenX <- predict(model35, newdata = Test)
predOpenY <- predict(model35, newdata = Train)

#print(summary(model35))

SSE <- sum((predOpenX - Test$Open)^2)
SST <- sum((mean(Test$Open) - Test$Open)^2)

ttsetr <- c(1:2)
ttsetr[1] = round(summary(model35)$r.squared, digits =3)
ttsetr[2] = round(1 - (SSE/SST), digits =3)
print("Training/Test Opening R2 Value:")
print(ttsetr)

#-----Shape
model34 <- lm(shape ~ franchise + G + rtUR + aCol, data = Train)
predShapeX <- predict(model34, newdata = Test)
predShapeY <- predict(model34, newdata = Train)

#print(summary(model35))

SSE <- sum((predShapeX - Test$shape)^2)
SST <- sum((mean(Test$shape) - Test$shape)^2)

tvsetr <- c(1:2)
tvsetr[1] = round(summary(model34)$r.squared, digits =3)
tvsetr[2] = round(1 - (SSE/SST), digits =3)
print("Training/Test Shape R2 Value:")
print(tvsetr)

#print(head(Test))


#-------Total
Test <- cbind(Test, predOpenX, predShapeX)
Train <- cbind(Train, predOpenY, predShapeY)


model31 <- lm(dGross ~ predOpenY * exp(predShapeY), data = Train)
model32 <- lm(dGross ~ predOpenX * exp(predShapeX), data = Test)

vtotalr <- c(1:2)
vtotalr[1] = round(summary(model31)$r.squared, digits =3)
vtotalr[2] = round(summary(model32)$r.squared, digits =3)

print("Total Prediction Validation:")
print(vtotalr)
print("-----------------------")

#--------------------------------------------------------
#                    Unseen Data 1.0 - Group
#--------------------------------------------------------

model36 <- lm(predOpen ~ pExp + franchise + G + rtUR + tDiff +  aCol + dCol, data = df)
predOpen <- predict(model36, newdata = df2016)
#print(summary(model36))

uSSE <- sum((predOpen - df2016$Open)^2)
uSST <- sum((mean(df2016$Open) - df2016$Open)^2)

openr <- c(1:2)
openr[1] = round(summary(model36)$r.squared, digits =3)
openr[2] = round(1 - (uSSE/uSST), digits =3)
print("Opening R2 Value:")
print(openr)

#Binding predicted opens
nfresult <- cbind(df2016, predOpen)
nfresult <- mutate(nfresult, diff = Open - predOpen)

#print (select(nfresult, Title, Open, predOpen, diff))
#print(summary(predictTrain))

#--------------------------------------------------------
#                    Unseen Data 2.0 - Shape
#--------------------------------------------------------

#model55 <- lm(shape ~  log(predOpen) + log(pExp) + franchise + G + rtUR + tDiff + aCol + dCol, data = df)
model55 <- lm(shape ~ franchise + G + rtUR + aCol , data = df)
shape <- predict(model55, newdata = nfresult)


uSSE <- sum((shape - df2016$week)^2)
uSST <- sum((mean(df2016$week) - df2016$week)^2)

shaper <- c(1:2)
shaper[1] = round(summary(model55)$r.squared, digits =3)
shaper[2] = round(1 - (uSSE/uSST), digits =3)


print("Shape R2 estimates: ")
print(shaper)
#print(round(summary(model55)$r.squared, digits =3))

nfresult <- cbind(nfresult, shape)
#print (select(fresult, id, week, shape))

#----- Examination Scale

shapeq <- cbind(shape, df2016$week)
shapeq <- tbl_df(shapeq)
colnames(shapeq) <- c("pred", "act")
shapeq <- cbind(shapeq, nfresult$id)
#shapeq$pred <- shapeq$pred*-10
#shapeq$act <- shapeq$act*-10

shapeq <- mutate(shapeq, diff= ((pred/act)-1))
#print(arrange(shapeq, desc(act)))


#---- Examination Graphing
xcol <- c(1:21)
ccol1 <- 0 #Predicted
ccol2 <- 1 #Actual

predshape <- cbind(xcol, shape, ccol1)
actualshape <- cbind(xcol, df2016$week, ccol2)
compareshape<- as.data.frame(rbind(predshape, actualshape))
compareshape$ccol1 <- as.factor(compareshape$ccol1)

#-----Graphing Shape
#s1 <- ggplot(compareshape, aes(y=shape, x= xcol, col=ccol1)) + geom_point() + ggtitle("Shape Estimates")
#print(s1)

#--------------------------------------------------------
#                    Total Data - Full
#--------------------------------------------------------

modelt2 <- lm(predTotal ~ predOpen * exp(shape), data = df)
predTotal <- predict(modelt2, newdata = nfresult)
#print(summary(modelt2))


uSSE <- sum((predTotal - nfresult$dGross)^2)
uSST <- sum((mean(nfresult$dGross) - nfresult$dGross)^2)

totalR <- c(1:2)
totalR[1] = round(summary(modelt2)$r.squared, digits =3)
totalR[2] = round(1 - (uSSE/uSST), digits =3)
print("Total R2 Value (with Shape):")
print(totalR)

nfresult <- cbind(nfresult, predTotal)

#--------------------------------------------------------
#                    Total Data - Just Open
#--------------------------------------------------------

modelt <- lm(predTotal ~ predOpen, data = df)
predTotal <- predict(modelt, newdata = df2016)
#print(summary(modelt2))


uSSE <- sum((predTotal - df2016$dGross)^2)
uSST <- sum((mean(df2016$dGross) - df2016$dGross)^2)

totalR <- c(1:2)
totalR[1] = round(summary(modelt)$r.squared, digits =3)
totalR[2] = round(1 - (uSSE/uSST), digits =3)
print("Total R2 Value (with Open):")
print(totalR)

#--------------------------------------------------------
#                   Misc Prints
#--------------------------------------------------------

nfresult <- mutate(nfresult, score = (dGross/predTotal - 1)*100)
#print (select(nfresult, Title, dGross, predTotal, score))

totalq <- cbind(nfresult$Title, nfresult$dGross, nfresult$predTotal, nfresult$score)
colnames(totalq) <- c("Title", "dGross", "predTotal", "score")
totalq <- as.data.frame(totalq)

#print(arrange(totalq, desc(score)))
#print(arrange(shapeq, desc(diff)))

