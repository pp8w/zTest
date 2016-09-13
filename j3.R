library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction
library(corrplot)

setwd("~/_Dev/")
df <-tbl_df(combod)


#-------Subsetting
#getting subset from combod in o
#df <- subset(df, df$Open > 10)
#print(df)



#--------------------------------------------------------
#                    TOTAL PREDICTION
#--------------------------------------------------------

#-----Predicting Total based on opening
modelt1 <- lm(dGross ~ Open, data = df)
#print(summary(model2))


#-----Predicting based on Shape and Log
modelt2 <- lm(dGross ~ Open * exp(shape), data = df)
#print(summary(model4))

#-----Storing results in vector
rsqvec <- c(1,2)
rsqvec[1] = round(summary(modelt1)$r.squared, digits =3)
rsqvec[2] = round(summary(modelt2)$r.squared, digits =3)

print("Total Prediction:")
print(rsqvec)

#--------------------------------------------------------
#                    OPENING PREDICTION
#--------------------------------------------------------

ovec <-c(1:2)
#-----------Predicting Open based on budget, Rating, Franchise, Genre
model31 <- lm(Open ~ pExp + franchise + G + rtCS + rtCR + rtUS + rtUR + 
                tDiff + iRate + meta + aCol + dCol, data = df)
model35 <- lm(Open ~ pExp + franchise + G + rtUR + tDiff +  aCol + dCol, data = df)

#print(summary(model3))


ovec[1] = round(summary(model31)$r.squared, digits =3)
ovec[2] = round(summary(model32)$r.squared, digits =3)
ovec[3] = round(summary(model33)$r.squared, digits =3)
ovec[4] = round(summary(model34)$r.squared, digits =3) #UR
ovec[5] = round(summary(model35)$r.squared, digits =3) #Meta & UR

#print(summary(model31))
#print(summary(model32))
print("Opening Prediction:")
print(ovec)

#-----------Predicting Open based on budget, Franchise, Genre, User
#model5 <- lm(Open ~ pBudget + franchise + G + rtUS + rtUR, data = df)
#print(summary(model5))

#--------------------------------------------------------
#                    SHAPE
#--------------------------------------------------------

sqvec <- c(1:3)

#-----------Predicting Shape based on budget, Franchise, Genre, User
model51 <- lm(shape ~  Open + pExp + franchise + G + rtCS + rtCR + rtUS + rtUR + tDiff, data = df)
model52 <- lm(shape ~  log(Open)*log(pExp) + franchise + G + meta + tDiff, data = df)
model53 <- lm(shape ~  log(Open)*log(pExp) + franchise + G + iRate + tDiff, data = df)
model54 <- lm(shape ~  log(Open)*log(pExp) + franchise + G + rtUS + tDiff, data = df)
model55 <- lm(exp(shape) ~  log(Open) + log(pExp) + franchise + G + rtUR + tDiff + aCol + dCol, data = df)

sqvec[1] = round(summary(model51)$r.squared, digits =3)
sqvec[2] = round(summary(model52)$r.squared, digits =3)
sqvec[3] = round(summary(model53)$r.squared, digits =3)
sqvec[4] = round(summary(model54)$r.squared, digits =3) #US
sqvec[5] = round(summary(model55)$r.squared, digits =3) #UR

#print(summary(model54))
#print(summary(model55))
print("Shape Prediction:")
print(sqvec)

#--------------------------------------------------------
#             Output To File
#--------------------------------------------------------

#rm(list = ls())
