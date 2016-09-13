#Unique names
gNames = combod$G
gTitle = as.character(combod$Title)
gN = 1

#Binding Columns
gSet = cbind(gTitle, gNames, gN)
colnames(gSet) = c("Title", "G","n")
gSet <- tbl_df(gSet)
gSet$n <- as.numeric(gSet$n)


#Prefix Consolidation
gSet$G <- as.character(gsub( 'Action.*', 'Action', gSet$G))
gSet$G <- as.character(gsub( 'Adventure.*', 'Adventure', gSet$G))
gSet$G <- as.character(gsub( 'Comedy.*', 'Comedy', gSet$G))
gSet$G <- as.character(gsub( 'Sci-Fi.*', 'Sci-Fi', gSet$G))
gSet$G <- as.character(gsub( 'Horror.*', 'Horror', gSet$G))
gSet$G <- as.character(gsub( 'Foreign.*', 'Foreign', gSet$G))
gSet$G <- as.character(gsub( 'Crime.*', 'Crime', gSet$G))
gSet$G <- as.character(gsub( 'Drama.*', 'Drama', gSet$G))
gSet$G <- as.character(gsub( 'Fantasy.*', 'Fantasy', gSet$G))
gSet$G <- as.character(gsub( 'Family.*', 'Family', gSet$G))
gSet$G <- as.character(gsub( 'War.*', 'War', gSet$G))

#Suffix Consolidation
gSet$G <- as.character(gsub( 'Period', '', gSet$G))
gSet$G <- as.character(gsub( 'Romantic', '', gSet$G))
gSet$G <- as.character(gsub( 'Sports', '', gSet$G))
gSet$G <- as.character(gsub( 'Historical', '', gSet$G))

#Set Combinations
gSet$G <- as.character(gsub( 'Musical', 'Concert', gSet$G))
gSet$G <- as.character(gsub( 'Music', '', gSet$G))
gSet$G <- as.character(gsub( 'War', 'Drama', gSet$G))
gSet$G <- as.character(gsub( 'Sci-Fi', 'Sci-Fan', gSet$G))
gSet$G <- as.character(gsub( 'Fantasy', 'Sci-Fan', gSet$G))
gSet$G <- as.character(gsub( 'WesternComedy', 'Adventure', gSet$G))
gSet$G <- as.character(gsub( 'Western', 'Adventure', gSet$G))
gSet$G <- as.character(gsub( 'Family', 'Adventure', gSet$G))
gSet$G <- as.character(gsub( 'Crime', 'Thriller', gSet$G))

#Other
gSet$G <- as.character(gsub( 'IMAX', 'Other', gSet$G))
gSet$G <- as.character(gsub( 'Concert', 'Other', gSet$G))


#Sum If Equivalent
print(aggregate(gSet$n, by = list(Category = gSet$G),  FUN = sum))

