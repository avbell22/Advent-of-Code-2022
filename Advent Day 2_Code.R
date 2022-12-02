#Advent of Code Day 2
#Part 1

#Data Cleaning 

#Read in data that was copied to text file 
day2 <- read.csv("/Users/avbell/Documents/MSA Program/Advent Code/AdventDay2_DATA.rtf")

#Get rid of the introductory lines from the RTF read in
  #for some reason, the second variable is needed to complete code on line 9
day2$counter <- 1:nrow(day2)
day2 <- day2[-c(1:7),]

#Rename the single read in column to day 2 and then split it into 2 variables
colnames(day2)[1] <- "day_2"
day2_new <- data.frame(do.call("rbind", strsplit(as.character(day2$day_2), " ", fixed = TRUE)))


#Rename the new variables just created
colnames(day2_new)[1] <- "opp_move" 
colnames(day2_new)[2] <-  "your_move" 


#Get rid of the trailing backslash on the your_move variable
day2_new$your_move <- gsub("\\", " ", day2_new$your_move, ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)






#Start on sample for loop


#Make sample set to use for creating for loop
first_column <- c("A", "B", "C")
second_column <- c("Y", "X", "Z")

df <- data.frame(second_column, first_column)
#In the case of my sample I just need to switch first_column with second_column, but will get more involved for full data below





#For loop to assign scores

x <- 1 #rock also A
y <- 2 #paper also B
z <- 3 #scissors also C
#The wins loss and draw rules from classical rock, paper, scissors applied below
win <- 6
lose <- 0
draw <- 3
for (row in 1:nrow(df)) {
  print(row)

  #First looking at hands where we played a rock, and then where opp played rock, paper, and scissors
  if (df$second_column[row] == "X"){
    df$your_play[row] <- "rock"
    if (df$first_column[row] == "A"){
      df$score[row] <- x + draw
      df$off_play[row] <- "rock"
    }
    else if (df$first_column[row] == "B"){
      df$score[row] <- x + lose
      df$off_play[row] <- "paper"
    }
    else if (df$first_column[row] == "C"){
      df$score[row] <- x + win
      df$off_play[row] <- "scissors"
    }
  } 
  
  
  #Second looking at hands where we played a paper, and then where opp played rock, paper, and scissors
  else if (df$second_column[row] == "Y"){
      df$your_play[row] <- "paper"
      if (df$first_column[row] == "A"){
        df$score[row] <- y + win
        df$off_play[row] <- "rock"
      }
      else if (df$first_column[row] == "B"){
        df$score[row] <- y + draw
        df$off_play[row] <- "paper"
      }
      else if (df$first_column[row] == "C"){
        df$score[row] <- y + lose
        df$off_play[row] <- "scissors"
        
      }
  }
  
  #Third looking at hands where we played a scissors, and then where opp played rock, paper, and scissors
  else if (df$second_column[row] == "Z"){
    df$your_play[row] <- "scissors"
    if (df$first_column[row] == "A"){
      df$score[row] <- z + lose
      df$off_play[row] <- "rock"
    }
    else if (df$first_column[row] == "B"){
      df$score[row] <- z + win
      df$off_play[row] <- "paper"
    }
    else if (df$first_column[row] == "C"){
      df$score[row] <- z + draw
      df$off_play[row] <- "scissors"
    }
  }
    
}

sum(df$score)









#Had to change the columns to begin with your move (by the logic I chose to use in my for loop)
day2_new <- day2_new[ , c("your_move", "opp_move")]

#When trying to run, I noticed there was trailing blank spaces on my your_move variable, so this needs to be fixed
day2_new$your_move <- trimws(day2_new$your_move)

#On the last ob, it had a } that was messing up my code
day2_new$your_move <- gsub("}", " ", day2_new$your_move, ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)


#Now reformat code loop from above to work for larger dataset
x <- 1 #rock also A
y <- 2 #paper also B
z <- 3 #scissors also C
#The wins loss and draw rules from classical rock, paper, scissors applied below
win <- 6
lose <- 0
draw <- 3
for (row in 1:nrow(day2_new)) {
  print(row)
  #First looking at hands where we played a rock, and then where opp played rock, paper, and scissors
  if (day2_new$your_move[row] == "X"){
    day2_new$your_play[row] <- "rock"
    if (day2_new$opp_move[row] == "A"){
      day2_new$score[row] <- x + draw
      day2_new$off_play[row] <- "rock"
    }
    else if (day2_new$opp_move[row] == "B"){
      day2_new$score[row] <- x + lose
      day2_new$off_play[row] <- "paper"
    }
    else if (day2_new$opp_move[row] == "C"){
      day2_new$score[row] <- x + win
      day2_new$off_play[row] <- "scissors"
    }
  } 
  
  
  #Second looking at hands where we played a paper, and then where opp played rock, paper, and scissors
  else if (day2_new$your_move[row] == "Y"){
    day2_new$your_play[row] <- "paper"
    if (day2_new$opp_move[row] == "A"){
      day2_new$score[row] <- y + win
      day2_new$off_play[row] <- "rock"
    }
    else if (day2_new$opp_move[row] == "B"){
      day2_new$score[row] <- y + draw
      day2_new$off_play[row] <- "paper"
    }
    else if (day2_new$opp_move[row] == "C"){
      day2_new$score[row] <- y + lose
      day2_new$off_play[row] <- "scissors"
      
    }
  }
  
  #Third looking at hands where we played a scissors, and then where opp played rock, paper, and scissors
  else if (day2_new$your_move[row] == "Z"){
    day2_new$your_play[row] <- "scissors"
    if (day2_new$opp_move[row] == "A"){
      day2_new$score[row] <- z + lose
      day2_new$off_play[row] <- "rock"
    }
    else if (day2_new$opp_move[row] == "B"){
      day2_new$score[row] <- z + win
      day2_new$off_play[row] <- "paper"
    }
    else if (day2_new$opp_move[row] == "C"){
      day2_new$score[row] <- z + draw
      day2_new$off_play[row] <- "scissors"
    }
  }
  
}



#For some reason, the last line of my code is offsetting all of my values by one character
  #I double checked and it is not doing this for other lines mimicking the same thing, so to combat, I am manually rescoring
  #observation 2500 to 9
day2_new$score[2500] <- 9

sum(day2_new$score)









#Part 2
day2_p2 = subset(day2_new, select = c('your_move' , 'opp_move'))

colnames(day2_p2)[1] <-  "game_end" 
# x = lose
# y = draw
# z = win



#Now reformat code loop from above to work for larger dataset

#We now know this column (the one containing x,y,z) is for lose draw or win
x <- 0 #lose
y <- 3 #draw
z <- 6 #win
#Values assigned for rock paper and scissors by the elves
rock <- 1 
paper <- 2 
scissors <- 3

for (row in 1:nrow(day2_p2)) {
  print(row)
  #First looking at hands where opp played a rock, and then where we either need to lose, draw, or win
  if (day2_p2$game_end[row] == "X"){
    day2_p2$game_end_c[row] <- "lose"
    if (day2_p2$opp_move[row] == "A"){
      day2_p2$opp_play[row] <- "rock"
      day2_p2$your_play[row] <- "scissors"
      day2_p2$score[row] <- x + scissors
    }
    else if (day2_p2$opp_move[row] == "B"){
      day2_p2$opp_play[row] <- "paper"
      day2_p2$your_play[row] <- "rock"
      day2_p2$score[row] <- x + rock
    }
    else if (day2_p2$opp_move[row] == "C"){
      day2_p2$opp_play[row] <- "scissors"
      day2_p2$your_play[row] <- "paper"
      day2_p2$score[row] <- x + paper
    }
  } 
  
  
  #Second looking at hands where opp played a paper, aand then where we either need to lose, draw, or win
  else if (day2_p2$game_end[row] == "Y"){
    day2_p2$game_end_c[row] <- "draw"
    if (day2_p2$opp_move[row] == "A"){
      day2_p2$opp_play[row] <- "rock"
      day2_p2$your_play[row] <- "rock"
      day2_p2$score[row] <- y + rock
    }
    else if (day2_p2$opp_move[row] == "B"){
      day2_p2$opp_play[row] <- "paper"
      day2_p2$your_play[row] <- "paper"
      day2_p2$score[row] <- y + paper
    }
    else if (day2_p2$opp_move[row] == "C"){
      day2_p2$opp_play[row] <- "scissors"
      day2_p2$your_play[row] <- "scissors"
      day2_p2$score[row] <- y + scissors
    }
  }
  
  #Third looking at hands where opp played a scissors, and then where we either need to lose, draw, or win
  else if (day2_p2$game_end[row] == "Z"){
    day2_p2$game_end_c[row] <- "win"
    if (day2_p2$opp_move[row] == "A"){
      day2_p2$opp_play[row] <- "rock"
      day2_p2$your_play[row] <- "paper"
      day2_p2$score[row] <- z + paper
      
    }
    else if (day2_p2$opp_move[row] == "B"){
      day2_p2$opp_play[row] <- "paper"
      day2_p2$your_play[row] <- "scissors"
      day2_p2$score[row] <- z + scissors
    }
    else if (day2_p2$opp_move[row] == "C"){
      day2_p2$opp_play[row] <- "scissors"
      day2_p2$your_play[row] <- "rock"
      day2_p2$score[row] <- z + rock
      
    }
  }
  
}


#Once again the last line of code is not matriculating like it needs to, so I will manually edit it
  #the game end is a win, and the opponents move is a paper, this means I will need to be scissors (3) and the win is (6)
day2_p2$score[2500] <- 9

sum(day2_p2$score)




