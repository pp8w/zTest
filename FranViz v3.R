library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization
library(scales)


#########################################################
#########################################################
#
#                   FRANCHISE VISUALIZATION                         
#
#########################################################
#########################################################

#--------------------------------------------------------
#           Obtaining FRANCHISES
#--------------------------------------------------------

setwd("~/_Dev/_Backup")
fdfsum <- read.csv("franchiseSummary.csv", header = TRUE, stringsAsFactors = FALSE)
fdfsum <- tbl_df(fdfsum)

fdflist <- read.csv("franchiseDetail.csv", header = TRUE, stringsAsFactors = FALSE)
fdflist <- fdflist[ , !names(fdflist) %in% c("nosp")]
fdflist <- tbl_df(fdflist)

fl5 <- subset(fdflist, fRank ==5)
fl4 <- subset(fdflist, fRank ==4)
fl3 <- subset(fdflist, fRank ==3)
fl2 <- subset(fdflist, fRank ==2)
fl1 <- subset(fdflist, fRank ==1)

fs5 <- subset(fdfsum, fRank ==5)
fs4 <- subset(fdfsum, fRank ==4)
fs3 <- subset(fdfsum, fRank ==3)
fs2 <- subset(fdfsum, fRank ==2)
fs1 <- subset(fdfsum, fRank ==1)

fRank <- c(1:5)
frsums <- c(sum(fs1$gross),sum(fs2$gross),sum(fs3$gross),sum(fs4$gross),sum(fs5$gross))
frnums <- c(sum(fs1$num),sum(fs2$num),sum(fs3$num),sum(fs4$num),sum(fs5$num))
fRank <- cbind(fRank, frsums, frnums)
fRank <- as.data.frame(fRank)
#print(frlist)

#--------------------------------------------------------
#           Overall Set
#--------------------------------------------------------

l = theme(legend.position="none") + theme(axis.ticks  = element_blank())
l2 = theme(legend.position="bottom",legend.direction="horizontal") + theme(axis.ticks  = element_blank())
x = theme(axis.text.x = element_blank()) + theme(axis.ticks  = element_blank())

#-------Histograms by Open and Budget
h10 <- ggplot(fdflist, aes(gross, fill=I("green"), col=I("black"), alpha = .20)) + 
  geom_histogram(binwidth = 100, position = "stack") + 
  stat_bin(binwidth= 100, geom="text",vjust=-.2, aes(label=..count..)) + 
  ggtitle("Gross by Individual Movie (100M Bins)")

h20 <- ggplot(fdflist, aes(fRank, fill=I("green"), col=I("black"), alpha = .20)) + 
  geom_histogram(binwidth = 1, position = "stack") + 
  stat_bin(binwidth= 1, geom="text",vjust=-.2, aes(label=..count..)) + 
  ggtitle("Movies per Franchise Rank")

h30 <- ggplot(fRank, aes(y=frsums, x=fRank, fill=I("green"), col=I("black"), alpha = .20)) + 
  geom_bar(stat="identity") + geom_text(aes(label=format(frsums, digits =1)), vjust= -.2) +
  ggtitle("Gross By Franchise Rank") 



h21 <- ggplot(fdfsum, aes(fRank, fill=I("green"), col=I("black"), alpha = .20)) + 
  geom_histogram(binwidth = 1, position = "stack") + 
  stat_bin(binwidth= 1, geom="text",vjust=-.2, aes(label=..count..)) + 
  ggtitle("Franchises by Rank") 

h22 <- ggplot(fdfsum, aes(num, fill=I("green"), col=I("black"), alpha = .20)) + 
  geom_histogram(binwidth = 1, position = "stack") + 
  stat_bin(binwidth= 1, geom="text",vjust=-.2, aes(label=..count..)) + 
  ggtitle("Franchise Size") 
#print(h22+l) #Franchise Size - Bar

#Individual Movies
print(h10+l) #Gross by Rank - Bar
print(h20+l) #Gross by Rank - Bar

#By Franchise Movies
print(h30+l) #Gross by Rank - Bar
print(h21+l) #Franchise Ranks - Bar

#-----All Movies

h31 <- ggplot(fdfsum, aes(y=gross, x=fRank, col=name)) + 
  geom_point() + 
  ggtitle("All Franchises")

d51b <- ggplot(fdfsum, aes(y=gross, x=name, fill=name)) + 
  geom_bar(stat="identity") + 
  ggtitle("All Franchises") +
  facet_grid(fRank~.) + facet_wrap(~fRank)

d52 <- ggplot(fdfsum, aes(y=gross, x=name, fill=name)) + 
  geom_bar(stat="identity") + 
  ggtitle("All Franchises") +
  facet_grid(fRank~.) + facet_wrap(~fRank, scales = "free_y")



#All Franchises
#print(h31+l+x) #Gross by Rank - Bar
print(d51b+x+l) #Gross by Franchise - Bar
print(d52+x+l) #Gross by Franchise - Bar


#--------------------------------------------------------
#           5 Star
#--------------------------------------------------------

d50 <- ggplot(fl5, aes(y=gross, x=name, col=name)) + geom_point() + 
       ggtitle("Individual Gross (5 Star)") 
      

#print(d50+x+l2) #Individual 5 Star Performance - Dot

d51 <- ggplot(fs5, aes(y=gross, x=name, fill=name)) + geom_bar(stat="identity") + 
       ggtitle("Gross per Franchise (5 Star)") +
       geom_text(aes(label=format(gross, digits =1)), vjust= -.2)

print(d51+x+l2) #Gross by Franchise - Bar


#--------------------------------------------------------
#           4 Star
#--------------------------------------------------------

d50 <- ggplot(fl4, aes(y=gross, x=name, col=name)) + geom_point() + 
  ggtitle("Individual Gross (4 Star)") 


#print(d50+x+l2) #Individual 5 Star Performance - Dot

d51 <- ggplot(fs4, aes(y=gross, x=name, fill=name)) + geom_bar(stat="identity") + 
  ggtitle("Gross per Franchise (4 Star)") +
  geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =3) 

print(d51+x+l2) #Gross by Franchise - Bar

#--------------------------------------------------------
#           3 Star
#--------------------------------------------------------

d50 <- ggplot(fl3, aes(y=gross, x=name, col=name)) + geom_point() + 
  ggtitle("Individual Gross (3 Star)") 


#print(d50+x+l2) #Individual 3 Star Performance - Dot

d51 <- ggplot(fs3, aes(y=gross, x=name, fill=name)) + geom_bar(stat="identity") + 
  ggtitle("Gross per Franchise (3 Star)") +
  geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =3) 

print(d51+x+l2) #Gross by Franchise - Bar

#--------------------------------------------------------
#           2 Star
#--------------------------------------------------------

d50 <- ggplot(fl2, aes(y=gross, x=name, col=name)) + geom_point() + 
  ggtitle("Individual Gross (2 Star)") 


#print(d50+x+l2) #Individual 2 Star Performance - Dot

d51 <- ggplot(fs2, aes(y=gross, x=name, fill=name)) + geom_bar(stat="identity") + 
  ggtitle("Gross per Franchise (2 Star)") +
  geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =3) 

print(d51+x+l2) #Gross by Franchise - Bar

#--------------------------------------------------------
#           1 Star
#--------------------------------------------------------

d50 <- ggplot(fl1, aes(y=gross, x=name, col=name)) + geom_point() + 
  ggtitle("Individual Gross (1 Star)") 


#print(d50+x+l2) #Individual 1 Star Performance - Dot

d51 <- ggplot(fs1, aes(y=gross, x=name, fill=name)) + geom_bar(stat="identity") + 
  ggtitle("Gross per Franchise (1 Star)") +
  geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =3) 

print(d51+x+l2) #Gross by Franchise - Bar
