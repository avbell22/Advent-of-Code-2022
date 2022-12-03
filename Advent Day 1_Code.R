#Day 1 Advent Code Part 1

#Read in Data
elf_food <- read.csv("/Users/avbell/Downloads/Advent_Code_Day_1.rtf")

#Get rid of the introductory lines from the RTF read in
elf_food$counter <- 1:nrow(elf_food)
elf_food <- elf_food[-c(1:8),]

#Rename the food number column
colnames(elf_food)[1] <- "Food_num"



#Create an indicator where there is an empty row
elf_food$zero <- ifelse(elf_food$Food_num == "\\", 1, 0)


#USe the indicator created above to make groupings for each of the elves that we can use to sum
elf_food$group <- c()
n <- 0
for (row in 1:nrow(elf_food)){
  elf_food$category[row] <- n
  print(elf_food$category)
   if (elf_food$zero[row] == 1){
     elf_food$group <- row
     n <- n + 1
     print(n)
   }
}


#Get rid of the trailing backslashes and change the variable to numeric to sum
elf_food$numeric_food <- gsub("\\", " ", elf_food$Food_num, ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)
elf_food$numeric_food <- as.numeric(elf_food$numeric_food)

#Get rid of the NAs and make them 0s (cannot sum if they are 0s)
elf_food$numeric_food[is.na(elf_food$numeric_food)] <- 0


#Create final dataset
elf_food_final <- elf_food %>% group_by(category) %>% summarise(sum_food = sum(numeric_food))

#Sort the final set in the r pane, maybe come back and do this in code format
(head(sort(elf_food_final$sum_food,decreasing=TRUE),n=1))
#final number for top elf was 69883
  
  
  

#For Part 2 will sort and find the highest 3 elves and a sum of their calories
#This code will provide us with the top 3 elves and a sum of the calories of their snacks
sum(head(sort(elf_food_final$sum_food,decreasing=TRUE),n=3))





