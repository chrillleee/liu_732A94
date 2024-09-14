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

my_moving_median <-function(x,n,na.rm = FALSE){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(n)))
    output <- c()
     for(index in (n+1):length(x)){
        window <- x[(index-n):index]
        output = append(output,median(window,na.rm ))
     }
    return(output)
}

for_mult_table <-function(from,to){
    stopifnot(is.numeric(from) && length(from)==1,is.numeric(to) && length(to)==1)
    staticRow = from:to
    outputMatrix = c()
    for(rowScalar in from:to){
        outputMatrix = rbind(outputMatrix,staticRow*rowScalar)
    }
    return(outputMatrix)
}

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

repeat_my_moving_median<-function(x,n, na.rm = FALSE){
    stopifnot((is.numeric(x) && is.vector(x)),(is.numeric(n)))
    output <- c()
    index = n+1
    stopIndex = length(x)
     repeat{
        if(index > stopIndex){break}
        window <- x[(index-n):index]
        output = append(output,median(window,na.rm))
        index = index + 1
     }
    return(output)
}

in_environment<-function(env){
    return(ls(env))
}

cov <-function(X){
    stopifnot(is.data.frame(X))
    func <- (function(y) { sd(y)/mean(y) })
    return(sapply(X, func))
}

moment <- function(i){
    stopifnot(is.numeric(i))
    return(function(data) {mean((data - mean(data))^i)})
}
mark_my_assignment()