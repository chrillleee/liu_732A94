library(markmyassignment)
lab_path <-
"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)
name <- "Christian Jonsson"
liuid <- "chjon338"

# Assignment set:
# Lab2: Advanced R programming, computer lab 2
# The assignment contain the following (10) tasks:
# - sheldon game
# - my moving median
# - for mult table
# - find cumsum
# - while mult table
# - repeat find cumsum
# - repeat my moving median
# - in environment
# - cov
# - moment


# created a win matrix since this will be smaller than the NxN matrix if i created all alternatives.
# If the opponents index is found in the win matrix that means the player has won. If no match is found that means it is a draw 
sheldon_game <-function(player1, player2){
    stopifnot(is.character(player1),is.character(player2))

    choices <- c("rock", "paper", "scissors", "lizard", "spock") 
    player1Index <- which(player1 == choices)
    player2Index <- which(player2 == choices)

    if (any(getWinAlternatives(player1Index) %in% player2Index)){
        return("Player 1 wins!")
    } else if(any(getWinAlternatives(player2Index) %in% player1Index)) {
        return("Player 2 wins!")
    } else{
        return("draw!")
    }
}

getWinAlternatives <-function(choiseIndex){
    winMatrix <- c(3,4)
    winMatrix <- rbind(winMatrix,c(1,5))
    winMatrix <- rbind(winMatrix,c(2,4))
    winMatrix <- rbind(winMatrix,c(2,5))
    winMatrix <- rbind(winMatrix,c(1,3))
    return(winMatrix[choiseIndex,])
}

# Step 1. Obtain the correct window to be evaluated, note +1 since one-indexing in R
# Step 2. Take mean of window
# Step 3. Append output to get the vector
# Step 4. Add 1 to index until hit the ceiling
my_moving_median <-function(x,n,...){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(n)))
    output <- c()
     for(index in (n+1):length(x)){
        window <- x[(index-n):index]
        output = append(output,median(window,... ))
     }
    return(output)
}

# Step 1: since the rows of the matrix are created by multiplying the numbers with a scalar this will be static
# Step 2: "append" the row with rbind starting with an empty matrix
for_mult_table <-function(from,to){
    stopifnot(is.numeric(from) && length(from)==1,is.numeric(to) && length(to)==1)
    staticRow = from:to
    outputMatrix = c()
    for(rowScalar in from:to){
        outputMatrix = rbind(outputMatrix,staticRow*rowScalar)
    }
    return(outputMatrix)
}

# We have 2 conditions that will break the loop, the value of the sum dependent on the input argument,
# and the length of the input vector. Note the And and the "smaller than" sign.
# Step 1 get all the inputs, and set cumsum to zero. Save stopIteration to avoid having to recalculate it every interation
# Step 2 add value to cumulative sum
# Step 3 increase index/iteration
find_cumsum <-function(x,find_sum){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(find_sum)))
    
    stopIteration = length(x)
    iteration = 1
    cumsum = 0
    while(iteration <= stopIteration & cumsum < find_sum){
        cumsum = cumsum + x[iteration]
        iteration = iteration + 1
    }
    return(cumsum)

}

# Since one demand was to have a nested while i bruteforced it 
# Step 1: Create static Row (nested while), and reset values 
# Step 2: multiply with current "row"-value and save to output (top while)
# Step 3: Increase step 
while_mult_table <-function(from, to){
    stopifnot(is.numeric(from) && length(from)==1,is.numeric(to) && length(to)==1)
    outputMatrix = c()
    iteration_row = from
    iteration_column = from
    while(iteration_row <= to){
        staticRow = c()
        iteration_column = from
        while(iteration_column <= to){
            staticRow = append(staticRow,iteration_column)
            iteration_column = iteration_column + 1
        }
        outputMatrix = rbind(outputMatrix,staticRow*iteration_row)
        iteration_row = iteration_row + 1
    }
    return(outputMatrix)
}

# same as find_cumsum but move the conditions from the while to the if and negate the conditions
# i.e. swap smaller to greater than, and change from and to or  
repeat_find_cumsum <-function(x, find_sum){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(find_sum)))
    
    stopIteration = length(x)
    iteration = 1
    cumsum = 0
    repeat{
        if(iteration > stopIteration | cumsum > find_sum){
            break
        }
        cumsum = cumsum + x[iteration]
        iteration = iteration + 1
    }
    return(cumsum)
}

# same as my_moving_median but move the conditions from the while to the if and negate the conditions
# had to declare the starting point to above the loop
repeat_my_moving_median<-function(x,n, ...){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(n)))
    output <- c()
    index = n+1
    stopIndex = length(x)
     repeat{
        if(index > stopIndex){break}
        window <- x[(index-n):index]
        output = append(output,median(window,...))
        index = index + 1
     }
    return(output)
}

# just syntax
in_environment<-function(env){
    return(ls(env))
}

# create the function beforehand and then use simple apply since we wanted a vector as output
cov <-function(X){
    stopifnot(is.data.frame(X))
    func <- (function(y) { sd(y)/mean(y) })
    return(sapply(X, func))
}

# Tricky part was to understand the wikipedia. same as Above but simpler and was able to return the funcition straigt away
moment <- function(i){
    stopifnot(is.numeric(i))
    return(function(data) {mean((data - mean(data))^i)})
}
mark_my_assignment()