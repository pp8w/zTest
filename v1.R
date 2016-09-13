library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction
library(corrplot)


#--------------------------------------------------------
#             Diagnoasitcs
#--------------------------------------------------------

#----- Coefficient Checks
df_cor <- df[, sapply(df, is.numeric)]
modelcor <- cor(df_cor, use= "complete.obs")
#print(modelcor)
corrplot(modelcor, type="lower")


#------ Add Ons
l = theme(legend.position="none")
l2 = theme(legend.position="bottom",legend.direction="vertical")
x = theme(axis.text.x = element_blank())


#--------------------------------------------------------
#             Total 
#--------------------------------------------------------


#-------Scatter Plots by dGross
d1 <- ggplot(df, aes(y=dGross, x= Open, col=franchise)) + 
  geom_point() + ggtitle("Gross by Open")
d2 <- ggplot(df, aes(y=dGross, x= Open, col=Actors)) + 
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

#--------------------------------------------------------
#             Opening Weeking 
#--------------------------------------------------------


#-------Scatter Plots by Open
o1 <- ggplot(df2, aes(y=Open, x= pExp, col=G)) + 
  geom_point() + ggtitle("Open by Budget")
o2 <- ggplot(df, aes(y=Open, x= pExp, col=franchise)) + 
  geom_point() + ggtitle("Open by Budget")
o3 <- ggplot(df, aes(y=Open, x= G, col=franchise)) + 
  geom_point() + ggtitle("Open by Genre")
o4 <- ggplot(df, aes(y=Open, x= franchise, col=franchise)) + 
  geom_point() + ggtitle("Open by Franchise")
o5 <- ggplot(df, aes(y=Open, x= Rating, col=franchise)) + 
  geom_point() + ggtitle("Open by Rating")
o6 <- ggplot(df, aes(y=Open, x= pExp, col=Actors)) + 
  geom_point() + ggtitle("Open by Budget")

#print(o1) #Open > Budget
#print(o2 + l) #Open > Budget
#print(o3 + x) #Open > Genre
#print(o4 + x) #Open > Franchise
#print(o5) #Open > Rating
#print(o6 + l) #Open > Rating

#--------------------------------------------------------
#             Histograms 
#--------------------------------------------------------

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
