#Unique names
gNames = combod2016$G
gTitle = as.character(combod2016$Title)
gN = 1

#Binding Columns
gset2016 = cbind(gTitle, gNames, gN)
colnames(gset2016) = c("Title", "G","n")
gset2016 <- tbl_df(gset2016)
gset2016$n <- as.numeric(gset2016$n)


#Prefix Consolidation
gset2016$G <- as.character(gsub( 'Action.*', 'Action', gset2016$G))
gset2016$G <- as.character(gsub( 'Adventure.*', 'Adventure', gset2016$G))
gset2016$G <- as.character(gsub( 'Comedy.*', 'Comedy', gset2016$G))
gset2016$G <- as.character(gsub( 'Sci-Fi.*', 'Sci-Fi', gset2016$G))
gset2016$G <- as.character(gsub( 'Horror.*', 'Horror', gset2016$G))
gset2016$G <- as.character(gsub( 'Foreign.*', 'Foreign', gset2016$G))
gset2016$G <- as.character(gsub( 'Crime.*', 'Crime', gset2016$G))
gset2016$G <- as.character(gsub( 'Drama.*', 'Drama', gset2016$G))
gset2016$G <- as.character(gsub( 'Fantasy.*', 'Fantasy', gset2016$G))
gset2016$G <- as.character(gsub( 'Family.*', 'Family', gset2016$G))
gset2016$G <- as.character(gsub( 'War.*', 'War', gset2016$G))

#Suffix Consolidation
gset2016$G <- as.character(gsub( 'Period', '', gset2016$G))
gset2016$G <- as.character(gsub( 'Romantic', '', gset2016$G))
gset2016$G <- as.character(gsub( 'Sports', '', gset2016$G))
gset2016$G <- as.character(gsub( 'Historical', '', gset2016$G))

#set2016 Combinations
gset2016$G <- as.character(gsub( 'Musical', 'Concert', gset2016$G))
gset2016$G <- as.character(gsub( 'Music', '', gset2016$G))
gset2016$G <- as.character(gsub( 'War', 'Drama', gset2016$G))
gset2016$G <- as.character(gsub( 'Sci-Fi', 'Sci-Fan', gset2016$G))
gset2016$G <- as.character(gsub( 'Fantasy', 'Sci-Fan', gset2016$G))
gset2016$G <- as.character(gsub( 'WesternComedy', 'Adventure', gset2016$G))
gset2016$G <- as.character(gsub( 'Western', 'Adventure', gset2016$G))
gset2016$G <- as.character(gsub( 'Family', 'Adventure', gset2016$G))
gset2016$G <- as.character(gsub( 'Crime', 'Thriller', gset2016$G))

#Other
gset2016$G <- as.character(gsub( 'IMAX', 'Other', gset2016$G))
gset2016$G <- as.character(gsub( 'Concert', 'Other', gset2016$G))


#Sum If Equivalent
print(aggregate(gset2016$n, by = list(Category = gset2016$G),  FUN = sum))

