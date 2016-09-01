library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction


setwd("~/_Dev/")
df <-tbl_df(combod)

#getting subset from combod in o
df <- subset(df, df$trsA > 1000)

#print(df)

#--------------------------------------------------------
#                    TOTAL PREDICTION
#--------------------------------------------------------

#----Storage
rsqvec <- c(1,2)

#--------Predicting Total based on opening
model2 <- lm(dGross ~ Open, data = df)
print(summary(model2))


#----------Predicting based on Shape and Log
model4 <- lm(dGross ~ Open * exp(shape) , data = df)
print(summary(model4))

rsqvec[1] = round(summary(model2)$r.squared, digits =3)
rsqvec[2] = round(summary(model4)$r.squared, digits =3)

print(rsqvec)

#--------------------------------------------------------
#                    OPENING PREDICTION
#--------------------------------------------------------

#-----------Predicting Open based on budget, Rating, Franchise, Genre
model3 <- lm(Open ~ pExp + franchise + G  , data = df)
#print(summary(model3))


#-----------Predicting Open based on budget, Franchise, Genre, User
#model5 <- lm(Open ~ pBudget + franchise + G + rtUS + rtUR, data = df)
#print(summary(model5))

#--------------------------------------------------------
#                    SHAPE
#--------------------------------------------------------


#-----------Predicting Open based on budget, Franchise, Genre, User
model5 <- lm(shape ~  log(Open)/log(pExp) + franchise + G , data = df)

#print(summary(model5))

#--------------------------------------------------------
#             Visualization
#--------------------------------------------------------

#------ Add Ons
l = theme(legend.position="none")
l2 = theme(legend.position="bottom",legend.direction="vertical")
x = theme(axis.text.x = element_blank())


#-------Scatter Plots by dGross
d1 <- ggplot(df, aes(y=dGross, x= Open, col=franchise)) + 
  geom_point() + ggtitle("Gross by Open")
d3 <- ggplot(df, aes(y=dGross, x= G, col=G)) + geom_point() + 
  ggtitle("Gross by Genre")
d4 <- ggplot(df, aes(y=dGross, x= franchise, col=franchise)) + 
  geom_point() + ggtitle("Gross by Franchise")
d5 <- ggplot(df, aes(y=dGross, x= Rating, col=franchise)) + 
  geom_point() + ggtitle("Gross by Rating")

print(d1) #Gross > Budget
#print(d3 + x) #Gross > Genre
#print(d4 + x) #Gross > Franchise
#print(d5) #Gross > Rating

#-------Scatter Plots by Open
o1 <- ggplot(df, aes(y=Open, x= pExp, col=franchise)) + 
  geom_point() + ggtitle("Open by Budget")
o3 <- ggplot(df, aes(y=Open, x= G, col=franchise)) + 
  geom_point() + ggtitle("Open by Genre")
o4 <- ggplot(df, aes(y=Open, x= franchise, col=franchise)) + 
  geom_point() + ggtitle("Open by Franchise")
o5 <- ggplot(df, aes(y=Open, x= Rating, col=franchise)) + 
  geom_point() + ggtitle("Open by Rating")

print(o1) #Open > Budget
#print(o3 + x) #Open > Genre
#print(o4 + x) #Open > Franchise
#print(o5) #Open > Rating


#-------Histograms by Open and Budget
h1 <- ggplot(df, aes(Open, fill = G)) + 
  geom_histogram(binwidth = 50, position = "stack")
h2 <- ggplot(df, aes(Open, fill = G)) + 
  geom_histogram(binwidth = 100, position = "fill")
h3 <- ggplot(df, aes(pExp, fill = G)) + 
  geom_histogram(binwidth = 50, position = "stack")
h4 <- ggplot(df, aes(pExp, fill = G)) + 
  geom_histogram(binwidth = 100, position = "fill")
h5 <- ggplot(df, aes(trsA, fill = franchise)) + 
  geom_histogram(binwidth = 1000, position = "stack")
h6 <- ggplot(df, aes(trsA, fill = franchise)) + 
  geom_histogram(binwidth = 1000, position = "fill")

#print (h1) #Open stack
#print (h2) #Open fill
#print (h3) #Budget stack
#print (h4) #Budget fill
#print (h5) #trsA stack
#print (h6) #trsA fill

#--------------------------------------------------------
#             Output To File
#--------------------------------------------------------

#rm(list = ls())
