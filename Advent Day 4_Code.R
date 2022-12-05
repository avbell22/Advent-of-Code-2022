#Advent of Code Day 4

#Data Cleaning
#Read in data that was copied to text file 
day4 <- read.csv("/Users/avbell/Documents/MSA Program/Advent Code/AdventDay4_DATA.rtf")

#Get rid of the introductory lines from the RTF read in
#for some reason, the second variable is needed to complete code on line 9
day4$counter <- 1:nrow(day4)
day4 <- day4[-c(1:7),]


#Rename the single read in column to compartment list
colnames(day4)[1] <- "range"

#needed to change this because some of the RTF settings ended up on this line
day4$range[1] <- "7-96"

#get rid of the } in the final line of the data
day4$range[2000] <- "26-90"

#Get rid of the backslashes
day4$range <- gsub("\\", " ", day4$range, ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)





#Create a sample to work with
df <- c( "2-4", "6-8", "2-3", "4-5", "5-7", "7-9", "2-8", "3-7", "6-6", "4-6", "2-6", "4-8")
df <- data.frame(df)


#Now we have each line being the range for 1 elf, but we need to convert from wide to long where every 2 is a grouping of elves to compare
  #will try for the sample I have above first, copying the code that I used in yesterday's Advent puzzle to convert it from long to wide
#Try to get the sample to work first
sampledf <- df[c(1)]
#This lists the number 3 for each grouping of 3 in the sample
sampledf$cut <- rep(1:(nrow(sampledf)/2), each = 2)
#This lists 1,2,3 for every 3 subsequent observations
sampledf$id <- rep(1:2, each=1, length.out=12)

long <- reshape(sampledf, 
                timevar = "id",
                idvar = c("cut"),
                direction = "wide")


#Now I need to split each of the new variables into 2: the start iterator for the sequence and the stop iterator for both elf 1 and elf 2 for comparison
long1 <- data.frame(do.call('rbind', strsplit(as.character(long$df.1),'-',fixed=TRUE)))

long2 <- data.frame(do.call('rbind', strsplit(as.character(long$df.2),'-',fixed=TRUE)))

long_final <- data.frame(long1, long2)

#Now I have each column for the start and stop, and just want to rename the column names for clarity
colnames(long_final)[1] <- "start_elf1"
colnames(long_final)[2] <- "stop_elf1"
colnames(long_final)[3] <- "start_elf2"
colnames(long_final)[4] <- "stop_elf2"


#Now create a for loop to iterate through a create a sequence from the start and stop variables (based on scratch work below)
for (rows in 1:nrow(long_final)){
  #print(rows)
  
  #Create the list of each of the ranges to compare
  elf1_range <- seq(long_final$start_elf1[rows], long_final$stop_elf1[rows], by = 1)
  #print(elf1_range)
  
  elf2_range <- seq(long_final$start_elf2[rows], long_final$stop_elf2[rows], by = 1)
  #print(elf2_range)
  
  #Now take the sequences and see if one is fully contained in another 
    #(test BOTH ways, if elf1 is contained with elf2 OR is elf2 is contained within elf1)
  
  #These will create boolean lists where it is true if the values are contained within each other
  elf2_in_elf1 <- elf2_range %in% elf1_range
  print(elf2_in_elf1)
  elf1_in_elf2 <- elf1_range %in% elf2_range
  print( elf1_in_elf2)
  
  #Now find the lengths of the boolean lists, specifically looking for where the boolean is true
    #if the boolean list length is equivalent to the length of the initial range, it is contained
  elf2_in_elf1_bool <- length(elf2_in_elf1[elf2_in_elf1 == TRUE])
  print(elf2_in_elf1_bool)
  
  elf1_in_elf2_bool <- length(elf1_in_elf2[elf1_in_elf2 == TRUE])
  print(elf1_in_elf2_bool)
  
  if (elf2_in_elf1_bool == length(elf2_range)){
    #print("yes")
    long_final$indicator[rows] <- 1
  } else if (elf1_in_elf2_bool == length(elf1_range)){
    #print("yes")
    long_final$indicator[rows] <- 1
  } else {
    long_final$indicator[rows] <- 0
    #print("no")
  } 
  
}



# #All of this is scratch work
# bin = seq(1, 5, by = 1)
# print(bin)
# 
# bin1 = seq(3, 5, by = 1)
# print(bin1)
# 
# first = bin %in% bin1
# 
# length(first[first == TRUE])
# count(bin %in% bin1)
# 
# length(bin1 %in% bin)



#Now create a for loop to iterate through a create a sequence from the start and stop variables (based on scratch work below)
for (rows in 1:nrow(day4_long_final)){
  #print(rows)
  
  #Create the list of each of the ranges to compare
  elf1_range <- seq(day4_long_final$start_elf1[rows], day4_long_final$stop_elf1[rows], by = 1)
  #print(elf1_range)
  
  elf2_range <- seq(day4_long_final$start_elf2[rows], day4_long_final$stop_elf2[rows], by = 1)
  #print(elf2_range)
  
  #Now take the sequences and see if one is fully contained in another 
  #(test BOTH ways, if elf1 is contained with elf2 OR is elf2 is contained within elf1)
  
  #These will create boolean lists where it is true if the values are contained within each other
  elf2_in_elf1 <- elf2_range %in% elf1_range
  #print(elf2_in_elf1)
  elf1_in_elf2 <- elf1_range %in% elf2_range
  #print( elf1_in_elf2)
  
  #Now find the lengths of the boolean lists, specifically looking for where the boolean is true
  #if the boolean list length is equivalent to the length of the initial range, it is contained
  elf2_in_elf1_bool <- length(elf2_in_elf1[elf2_in_elf1 == TRUE])
  #print(elf2_in_elf1_bool)
  
  elf1_in_elf2_bool <- length(elf1_in_elf2[elf1_in_elf2 == TRUE])
  #print(elf1_in_elf2_bool)
  
  if (elf2_in_elf1_bool == length(elf2_range)){
    #print("yes")
    day4_long_final$indicator[rows] <- 1
  } else if (elf1_in_elf2_bool == length(elf1_range)){
    #print("yes")
    day4_long_final$indicator[rows] <- 1
  } else {
    day4_long_final$indicator[rows] <- 0
    #print("no")
  } 
  
}


sum(day4_long_final$indicator)











#Part 2

#In the previous section, we were looking for the entire sequence contained, now we are looking for any kind of overlap
  #NOTE on the example with the sample data, I did not change the dataset names, so if this is not run piece by piece, it will overwrite

#first work on the same sample from above, and using the same general outline from for loop above
  #now, if there is 1 true in the boolean statement, then it will be considered an indicator
for (rows in 1:nrow(long_final)){
  #print(rows)
  
  #Create the list of each of the ranges to compare
  elf1_range <- seq(long_final$start_elf1[rows], long_final$stop_elf1[rows], by = 1)
  #print(elf1_range)
  
  elf2_range <- seq(long_final$start_elf2[rows], long_final$stop_elf2[rows], by = 1)
  #print(elf2_range)
  
  #Now take the sequences and see if one is fully contained in another 
  #(test BOTH ways, if elf1 is contained with elf2 OR is elf2 is contained within elf1)
  
  #These will create boolean lists where it is true if the values are contained within each other
  elf2_in_elf1 <- elf2_range %in% elf1_range
  #print(elf2_in_elf1)
  elf1_in_elf2 <- elf1_range %in% elf2_range
  #print( elf1_in_elf2)
  
  #Now find the lengths of the boolean lists, specifically looking for where the boolean is true
  #if the boolean list length is equivalent to the length of the initial range, it is contained
  elf2_in_elf1_bool <- as.numeric(length(elf2_in_elf1[elf2_in_elf1 == TRUE]))
  print(elf2_in_elf1_bool)
  
  elf1_in_elf2_bool <- as.numeric(length(elf1_in_elf2[elf1_in_elf2 == TRUE]))
  print(elf1_in_elf2_bool)
  
  if (elf2_in_elf1_bool >= 1){
    #print("yes")
    long_final$indicator[rows] <- 1
  } else if (elf1_in_elf2_bool >= 1){
    #print("yes")
    long_final$indicator[rows] <- 1
  } else {
    long_final$indicator[rows] <- 0
    #print("no")
  }
  
}


#Now add this to the full dataset
#changing the dataset name we are working with so we don't overwrite on our full data like we did with the sample example
day4_long_final1 <- day4_long_final


for (rows in 1:nrow(day4_long_final1)){
  #print(rows)
  
  #Create the list of each of the ranges to compare
  elf1_range <- seq(day4_long_final1$start_elf1[rows], day4_long_final1$stop_elf1[rows], by = 1)
  #print(elf1_range)
  
  elf2_range <- seq(day4_long_final1$start_elf2[rows], day4_long_final1$stop_elf2[rows], by = 1)
  #print(elf2_range)
  
  #Now take the sequences and see if one is fully contained in another 
  #(test BOTH ways, if elf1 is contained with elf2 OR is elf2 is contained within elf1)
  
  #These will create boolean lists where it is true if the values are contained within each other
  elf2_in_elf1 <- elf2_range %in% elf1_range
  #print(elf2_in_elf1)
  elf1_in_elf2 <- elf1_range %in% elf2_range
  #print( elf1_in_elf2)
  
  #Now find the lengths of the boolean lists, specifically looking for where the boolean is true
  #if the boolean list length is equivalent to the length of the initial range, it is contained
  elf2_in_elf1_bool <- as.numeric(length(elf2_in_elf1[elf2_in_elf1 == TRUE]))
  print(elf2_in_elf1_bool)
  
  elf1_in_elf2_bool <- as.numeric(length(elf1_in_elf2[elf1_in_elf2 == TRUE]))
  print(elf1_in_elf2_bool)
  
  if (elf2_in_elf1_bool >= 1){
    #print("yes")
    day4_long_final1$indicator[rows] <- 1
  } else if (elf1_in_elf2_bool >= 1){
    #print("yes")
    day4_long_final1$indicator[rows] <- 1
  } else {
    day4_long_final1$indicator[rows] <- 0
    #print("no")
  }
  
}

sum(day4_long_final1$indicator)













