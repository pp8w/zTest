library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction


setwd("~/_Dev/")

#getting subset from combod in o2
df <- mdf[ , !names(mdf) %in% c("id","Actors", "Writer","Director", "imdbID", "Year", "nosp")]
df$franchise[is.na(df$franchise)] <- 0
df <-tbl_df(df)


#print(df)

#--------------------------------------------------------
#                    TOTAL PREDICTION
#--------------------------------------------------------


#--------Predicting Total based on opening
model2 <- lm(df$dGross ~ df$Open)
print(summary(model2))
#Adjusted R-squared:  0.8322

#-----------Predicting Total based on production budget
#First model - based on production budget
model1 <- lm(df$dGross ~ df$pBudget)
print(summary(model1))
#Adjusted R-squared:  0.5689 

#----------Predicting based on Shape and Log
model4 <- lm(log(dGross) ~ log(Open) + shape , data = df)
print(summary(model4))
#Adjusted R-squared:  0.8961


#--------------------------------------------------------
#                    OPENING PREDICTION
#--------------------------------------------------------

#-----------Predicting Open based on budget, Rating, Franchise, Genre
model3 <- lm(Open ~ pBudget + Rating + franchise + G, data = df)
print(summary(model3))
#Adjusted R-squared:  0.6 vs .48 with only pbudget


#-----------Predicting Open based on budget, Franchise, Genre, User
#model5 <- lm(Open ~ pBudget + franchise + G + rtUS + rtUR, data = df)
#print(summary(model5))
#Adjusted R-squared:  0.7824



