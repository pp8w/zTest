library(dplyr)
library(tidyr)
library(plyr) #ldply
library(ggplot2) #visualization
library(broom) #coefficient extraction
library(corrplot)


#--------------------------------------------------------
#             Diagnostics
#--------------------------------------------------------

#----- Coefficient Checks
dcon_cor <- combod[, sapply(combod, is.numeric)]

#Excludes Year, R2, dGross
modelcor <- cor(dcon_cor[, -c(1,8,13)], use= "complete.obs")
#print(modelcor)
corrplot(modelcor, type="lower")

#adding franchise column
dcon$franchise <- as.numeric(dcon$fName)
dcon$franchise <- dcon$franchise/dcon$franchise
dcon$franchise[is.na(dcon$franchise)] <- 0

dcon$franchise <- as.factor(dcon$franchise)
dcon$aCol <- as.factor(dcon$aCol)
dcon$dCol <- as.factor(dcon$dCol)



#------ Add Ons
l = theme(legend.position="none")
l2 = theme(legend.position="bottom",legend.direction="vertical")
x = theme(axis.text.x = element_blank())


#--------------------------------------------------------
#             Total 
#--------------------------------------------------------


#-------Scatter Plots by dGross
d1 <- ggplot(dcon, aes(y=dGross, x= Open, col=franchise)) + 
  geom_point() + ggtitle("Gross by Open") 

d2 <- ggplot(dcon, aes(y=dGross, x= pExp, col=franchise)) + 
  geom_point() + ggtitle("Gross by pExp")

d3 <- ggplot(dcon, aes(y=dGross, x= shape, col=franchise)) + 
  geom_point() + ggtitle("Gross by shape")

d4 <- ggplot(dcon, aes(y=dGross, x= trsA, col=franchise)) + 
  geom_point() + ggtitle("Gross by trsA")

d5 <- ggplot(dcon, aes(y=dGross, x= trsB, col=franchise)) + 
  geom_point() + ggtitle("Gross by trsB")

d7 <- ggplot(dcon, aes(y=dGross, x= rtUR, col=franchise)) + 
  geom_point() + ggtitle("Gross by rtUR") + facet_grid(. ~ franchise)

d8 <- ggplot(dcon, aes(y=dGross, x= Runtime, col=franchise)) + 
  geom_point() + ggtitle("Gross by Runtime")

d9 <- ggplot(dcon, aes(y=dGross, x= franchise, col=franchise)) + 
  geom_point() + ggtitle("Gross by franchise")

d10 <- ggplot(dcon, aes(y=dGross, x= aCol, col=franchise)) + 
  geom_point() + ggtitle("Gross by aCol") + facet_grid(. ~ franchise)

d11 <- ggplot(dcon, aes(y=dGross, x= dCol, col=franchise)) + 
  geom_point() + ggtitle("Gross by dCol") + facet_grid(. ~ franchise)

d12 <- ggplot(dcon, aes(y=dGross, x= Rating, col=franchise)) + 
  geom_point() + ggtitle("Gross by Rating") + facet_grid(. ~ franchise)

print(d1)
print(d2)
print(d3)
print(d4)
print(d5)
print(d7)
print(d8)
print(d9)
print(d10)
print(d11)
print(d12)

#--------------------------------------------------------
#             Shape 
#--------------------------------------------------------

#-------Scatter Plots by dGross
d1 <- ggplot(dcon, aes(y=shape, x= Open, col=franchise)) + 
  geom_point() + ggtitle("Shape by Open")

d2 <- ggplot(dcon, aes(y=shape, x= pExp, col=franchise)) + 
  geom_point() + ggtitle("Shape by pExp")

d3 <- ggplot(dcon, aes(y=shape, x= dGross, col=franchise)) + 
  geom_point() + ggtitle("Shape by dGross")

d4 <- ggplot(dcon, aes(y=shape, x= trsA, col=franchise)) + 
  geom_point() + ggtitle("Shape by trsA")

d5 <- ggplot(dcon, aes(y=shape, x= trsB, col=franchise)) + 
  geom_point() + ggtitle("Shape by trsB")

d7 <- ggplot(dcon, aes(y=shape, x= rtUR, col=franchise)) + 
  geom_point() + ggtitle("Shape by rtUR")

d8 <- ggplot(dcon, aes(y=shape, x= Runtime, col=franchise)) + 
  geom_point() + ggtitle("Shape by Runtime")

d9 <- ggplot(dcon, aes(y=shape, x= franchise, col=franchise)) + 
  geom_point() + ggtitle("Shape by franchise")

d10 <- ggplot(dcon, aes(y=shape, x= aCol, col=franchise)) + 
  geom_point() + ggtitle("Shape by aCol")

d11 <- ggplot(dcon, aes(y=shape, x= dCol, col=franchise)) + 
  geom_point() + ggtitle("Shape by dCol")

d12 <- ggplot(dcon, aes(y=shape, x= Rating, col=franchise)) + 
  geom_point() + ggtitle("Shape by Rating")

'
print(d1)
print(d2)
print(d3)
print(d4)
print(d5)
#print(d6)
print(d7)
print(d8)
print(d9)
print(d10)
print(d11)
print(d12)
'

#--------------------------------------------------------
#             Histograms 
#--------------------------------------------------------

#-------Histograms by Open and Budget
h1 <- ggplot(dcon, aes(dGross, fill = franchise)) + 
  geom_histogram(binwidth = 50, position = "fill") 

h2 <- ggplot(dcon, aes(pExp, fill = franchise)) + 
  geom_histogram(binwidth = 50, position = "fill") 

h3 <- ggplot(dcon, aes(shape, fill = G)) + 
  geom_histogram(binwidth = 0.1, position = "stack") 

h4 <- ggplot(dcon, aes(trsA, fill = franchise)) + 
  geom_histogram(binwidth = 500, position = "stack") + facet_grid(. ~ franchise)

h5 <- ggplot(dcon, aes(trsB, fill = franchise)) + 
  geom_histogram(binwidth = 500, position = "stack") + facet_grid(. ~ franchise)

h6 <- ggplot(dcon, aes(rtUR, fill = franchise)) + 
  geom_histogram(binwidth = 0.5, position = "dodge") + facet_grid(. ~ franchise)

h7 <- ggplot(dcon, aes(franchise, fill = G)) + 
  geom_histogram(binwidth = 1, position = "stack")

h8 <- ggplot(dcon, aes(aCol, fill = G)) + 
  geom_histogram(binwidth = 1, position = "stack") + facet_grid(. ~ franchise)

'
print (h1) #Open stack
print (h2) #Budget fill
print (h3) #shape stack
print (h4) #trsA fill
print (h5) #trsB stack
print (h6) #rtUR fill
#print (h7) #franchise stack
#print (h8) #aCol fill
'

#--------------------------------------------------------
#             Output To File
#--------------------------------------------------------
