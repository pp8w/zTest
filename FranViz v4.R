library(plyr) #ldply
library(dplyr)
library(tidyr)
library(ggplot2) #visualization


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

fl5 <- subset(fdflist, fRank == 5)
fl4 <- subset(fdflist, fRank == 4)
fl3 <- subset(fdflist, fRank == 3)
fl2 <- subset(fdflist, fRank == 2)
fl1 <- subset(fdflist, fRank == 1)

fs5 <- subset(fdfsum, fRank == 5)
fs4 <- subset(fdfsum, fRank == 4)
fs3 <- subset(fdfsum, fRank == 3)
fs2 <- subset(fdfsum, fRank == 2)
fs1 <- subset(fdfsum, fRank == 1)

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


#--------------------------------------------------------
#           Individual Histograms
#--------------------------------------------------------

h10 <- ggplot(fdflist, aes(gross, 
              fill=I("green"), col=I("black"), alpha = .20)) + 
              geom_histogram(binwidth = 100, position = "stack") + 
              stat_bin(binwidth= 100, geom="text",vjust=-.2, aes(label=..count..)) + 
              ggtitle("Gross by Individual Movie (100M Bins)")

h20 <- ggplot(fdfsum, aes(fRank, 
              fill=I("green"), col=I("black"), alpha = .20)) + 
              geom_histogram(binwidth = 1, position = "stack") + 
              stat_bin(binwidth= 1, geom="text",vjust=-.2, aes(label=..count..)) + 
              ggtitle("Movies per Franchise Rank")


print(h10+l) #Gross by Rank - Bar
print(h20+l) #Gross by Rank - Bar

#--------------------------------------------------------
#           Franchise Histograms
#--------------------------------------------------------


h30 <- ggplot(fRank, aes(y=frsums, x=fRank, 
              fill=fRank, col=I("black"), alpha = .20)) + 
              geom_bar(stat="identity", position = "stack") + 
              geom_text(aes(label=format(frsums, digits =1)), vjust= -.2) +
              ggtitle("Gross By Franchise Rank") 

h21 <- ggplot(fRank, aes(y=frnums, x=fRank, 
              fill=fRank, col=I("black"), alpha = .20)) + 
              geom_text(aes(label=format(frnums, digits =1)), vjust= -.2) +
              geom_bar(stat="identity") + 
              ggtitle("Franchises by Size") 

print(h30+l) #Gross by Rank - Bar
print(h21+l) #Franchise Ranks - Bar

#--------------------------------------------------------
#           All Movies
#--------------------------------------------------------

d51b <- ggplot(fdfsum, aes(y=gross, x=name, fill=name)) + 
               geom_bar(stat="identity") + 
               ggtitle("All Franchises") +
               facet_grid(fRank~.) + facet_wrap(~fRank) +
               theme(axis.title.x = element_blank(), legend.title = element_blank())

d52 <- ggplot(fdfsum, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("All Franchises") +
              facet_grid(fRank~.) + facet_wrap(~fRank, scales = "free_y") + 
              theme(axis.title.x = element_blank(), legend.title = element_blank())
                   
              

#print(d51b+x+l) #Gross by Franchise - Bar
#print(d52+x+l) #Gross by Franchise - Bar

#--------------------------------------------------------
#           5 Star
#--------------------------------------------------------

d51 <- ggplot(fs5, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("Gross per Franchise (5 Star)") +
              geom_text(aes(label=format(gross, digits =1)), vjust= -.2) +
              theme(axis.title.x = element_blank(), legend.title = element_blank())

#print(d51+x+l2) #Gross by Franchise - Bar

#--------------------------------------------------------
#           Four Star
#--------------------------------------------------------

d51 <- ggplot(fs4, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("Gross per Franchise (4 Star)") +
              geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =2) +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

#print(d51+x+l2) #Four Star

#--------------------------------------------------------
#           Three Star
#--------------------------------------------------------

d51 <- ggplot(fs3, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("Gross per Franchise (3 Star)") +
              geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =2)+
  theme(axis.title.x = element_blank(), legend.title = element_blank())
            

#print(d51+x+l2) #Three Star

#--------------------------------------------------------
#           Two Star
#--------------------------------------------------------

d51 <- ggplot(fs2, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("Gross per Franchise (2 Star)") +
              geom_text(aes(label=format(gross, digits =1)), vjust= -.2, size =2) +
              theme(legend.text = element_text(size = 8)) +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

#print(d51+x+l2) #Two Star

#--------------------------------------------------------
#           One Star
#--------------------------------------------------------

d51 <- ggplot(fs1, aes(y=gross, x=name, fill=name)) + 
              geom_bar(stat="identity") + 
              ggtitle("Gross per Franchise (1 Star)") +
              geom_text(aes(label=format(gross, digits =0)), vjust= -.2, size =2) +
              theme(legend.text = element_text(size = 8))+
  theme(axis.title.x = element_blank(), legend.title = element_blank())

#print(d51+x+l2) #One Star
