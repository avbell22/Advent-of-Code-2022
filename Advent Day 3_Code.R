#Advent of Code Day 3

#Part 1

#Data Cleaning 

#Read in data that was copied to text file 
day3 <- read.csv("/Users/avbell/Documents/MSA Program/Advent Code/AdventDay3_DATA.rtf")

#Get rid of the introductory lines from the RTF read in
#for some reason, the second variable is needed to complete code on line 9
day3$counter <- 1:nrow(day3)
day3 <- day3[-c(1:7),]


#Rename the single read in column to compartment list
colnames(day3)[1] <- "compartment_list"

#needed to change this because some of the RTF settings ended up on this line
day3$compartment_list[1] <- "dWlhclDHdFvDCCDfFq"

#get rid of te final line of the data (because it is only a })
day3 <- head(day3, -1)

#Get rid of the backslashes
day3$compartment_list <- gsub("\\", " ", day3$compartment_list, ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)






#Try to figure out the sample data first
sample <- c("vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw")
df <- data.frame(sample)


#See how the substring function works on 1 row of code before for loop
substring(df$sample[2], seq(1, nchar(df$sample[2]), nchar(df$sample[2])/2), seq(nchar(df$sample[2])/2, nchar(df$sample[2]), nchar(df$sample[2])/2))                   



#Create a for loop to run through the sample data created above 

#Split the variable compartment_list into pack1 and pack2
myLetters <- letters[1:26]
myLetterscap <- toupper(letters[1:26])
mapping <- c(myLetters, myLetterscap)
for (row in 1:nrow(df)){
  #print(row)
  print(nchar(df$sample[row]))
  print(nchar(df$sample[row])/2)
  #print(df$sample[row])
  set <- substring(df$sample[row], seq(1, nchar(df$sample[row]), nchar(df$sample[row])/2), 
                             seq(nchar(df$sample[row])/2, nchar(df$sample[row]), nchar(df$sample[row])/2))
  print(set)
  
  #This takes the pieces of each half of the original compartment and splits them into first and second rucksack
  df$part1[row] <- set[1]
  df$part2[row] <- set[2]
  
  
  #Now take each of those pieces and split them into the individual character values to compare in an intersection
  split <-el(strsplit(df$part1[row],''))
  split2 <- el(strsplit(df$part2[row],''))
  df$overlap[row] <- intersect(as.vector(split), as.vector(split2))
  df$score[row] <- match(df$overlap[row], mapping)
}

sum(df$score)

#Scratch work to figure functions to use (most of these came from stak overflow)
# split<-el(strsplit(df$part1[1],''))
# split2 <- el(strsplit(df$part2[1],''))
# print(split)
# 
# ?intersect
# as.vector(df$part1[1])
# intersect(as.vector(split), as.vector(split2))


# myLetters <- letters[1:26]
# match("a", myLetters)
# myLetterscap <- toupper(letters[1:26])
# match("A", myLetterscap)
# mapping <- c(myLetters, myLetterscap)
# print(mapping)
# match("A", mapping)




#Use the for loop created above for the full data set


#Split the variable compartment_list into pack1 and pack2
myLetters <- letters[1:26]
myLetterscap <- toupper(letters[1:26])
mapping <- c(myLetters, myLetterscap)
for (row in 1:nrow(day3)){
  #print(row)
  print(nchar(day3$compartment_list[row]))
  print(nchar(day3$compartment_list[row])/2)
  #print(day3$compartment_list[row])
  set <- substring(day3$compartment_list[row], seq(1, nchar(day3$compartment_list[row]), nchar(day3$compartment_list[row])/2), 
                   seq(nchar(day3$compartment_list[row])/2, nchar(day3$compartment_list[row]), nchar(day3$compartment_list[row])/2))
  print(set)
  
  #This takes the pieces of each half of the original compartment and splits them into first and second rucksack
  day3$part1[row] <- set[1]
  day3$part2[row] <- set[2]
  
  
  #Now take each of those pieces and split them into the individual character values to compare in an intersection
  split <-el(strsplit(day3$part1[row],''))
  split2 <- el(strsplit(day3$part2[row],''))
  day3$overlap[row] <- intersect(as.vector(split), as.vector(split2))
  day3$score[row] <- match(day3$overlap[row], mapping)
}


sum(day3$score)



















#Part 2

#Try to get the sample to work first
sampledf <- df[c(1)]
#This lists the number 3 for each grouping of 3 in the sample
sampledf$cut <- rep(1:(nrow(sampledf)/3), each = 3)
#This lists 1,2,3 for every 3 subsequent observations
sampledf$id <- rep(1:3, each=1, length.out=6)

long <- reshape(sampledf, 
             timevar = "id",
             idvar = c("cut"),
             direction = "wide")
###IMPORTANT, when going in to create the wide dataset for the full set, NEED to get rid of all other variables except the id and cut
  #I created and the rucksack strings (compartment_list)


#Now that we have the proper outline, need to make a for loop to run through them
#bring back in the mappings used above for consistency
myLetters <- letters[1:26]
myLetterscap <- toupper(letters[1:26])
mapping <- c(myLetters, myLetterscap)

for (row in 1:nrow(long)){
  #Take each of the 3 packs and split them into the individual character values to compare in an intersection
  split <- el(strsplit(long$sample.1[row],''))
  split2 <- el(strsplit(long$sample.2[row],''))
  split3 <- el(strsplit(long$sample.3[row],''))
  long$overlap[row] <- intersect(as.vector(split3), intersect(as.vector(split), as.vector(split2)))
  long$score[row] <- match(long$overlap[row], mapping)
}

sum(long$score)
#This is the same as the example, so good to move on to next step










####Code to understand how the reshape function works from long to wide and wide to long
  ####found on: https://stats.oarc.ucla.edu/r/faq/how-can-i-reshape-my-data-in-r/
# hsb2 <- read.table('https://stats.idre.ucla.edu/stat/r/faq/hsb2.csv', header=T, sep=",")
# hsb2[1:10,]
# 
# 
# l <- reshape(hsb2, 
#              varying = c("read", "write", "math", "science", "socst"), 
#              v.names = "score",
#              timevar = "subj", 
#              times = c("read", "write", "math", "science", "socst"), 
#              new.row.names = 1:1000,
#              direction = "long")
# 
# l.sort <- l[order(l$id),]
# l.sort[1:10,]
# 
# 
# ## Long to wide
# 
# w <- reshape(l.sort, 
#              timevar = "subj",
#              idvar = c("id", "female", "race", "ses", "schtyp", "prog"),
#              direction = "wide")
# 
# w[1:10,]







#Try to get the full sample to work now
day3_p2 <- day3[c(1)]
day3_p2$cut <- rep(1:(nrow(day3_p2)/3), each = 3)
day3_p2$id <- rep(1:3, each=1, length.out=300)
#compartment_listdf$counter <- 1:nrow(compartment_listdf)

long_day3 <- reshape(day3_p2, 
                          timevar = "id",
                          idvar = c("cut"),
                          direction = "wide")


#Now that we have the proper outline, need to make a for loop to run through them
#bring back in the mappings used above for consistency
myLetters <- letters[1:26]
myLetterscap <- toupper(letters[1:26])
mapping <- c(myLetters, myLetterscap)

for (row in 1:nrow(long_day3)){
  #Take each of the 3 packs and split them into the individual character values to compare in an intersection
  split <- el(strsplit(long_day3$compartment_list.1[row],''))
  split2 <- el(strsplit(long_day3$compartment_list.2[row],''))
  split3 <- el(strsplit(long_day3$compartment_list.3[row],''))
  long_day3$overlap[row] <- intersect(as.vector(split3), intersect(as.vector(split), as.vector(split2)))
  long_day3$score[row] <- match(long_day3$overlap[row], mapping)
}
#There were warning about how I used the intersect function, but I am going to check and see if the output is still correct






#Used for reference to figure out how to find grouping of 3 for my data
#day3_p2$cut <- rep(1:(nrow(day3_p2)/3), each = 3)
#day3_p2 <- day3_p2[sapply(split(1:nrow(day3_p2), cut), c, NA), ]
#found these lines of code on stack overflow, didnt need the second because it added NAs to the dataset based on the cut variable
















