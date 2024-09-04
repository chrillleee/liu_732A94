library(markmyassignment)
lab_path <-
"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

# Assignment set:
# Lab1: Advanced R programming, computer lab 1
# The assignment contain the following (16) tasks:
# - my num vector
# - filter my vector
# - dot prod
# - approx e
# - my magic matrix
# - calculate elements
# - row to zero
# - add elements to matrix
# - my magic list
# - change info
# - add note
# - sum numeric parts
# - my data.frame
# - sort head
# - add median variable
# - analyze columns

my_num_vector <- function() {
    float_v <- c(log10(11), cos(pi/5), exp(pi/3), (1173%%7)/19)
    return(float_v)
}

filter_my_vector <- function(x, leq){
  output <- c()

  for(num in x){
    output <- append(output, ifelse(leq <= num, NA, num))
  }

  return(output)
}

 dot_prod <- function(a, b){
  return(a %*% b)
 }

 approx_e <- function(N){
    #approx N=11 gives  2.7183
    iteration <- 0
    output <- 0
    while(iteration<=N){
        output = output + 1/factorial(iteration)
        iteration <- iteration + 1
    }   
    return(output)
 }

  my_magic_matrix <- function(){
    output <- c()
    output <- rbind(output, c(4,9,2))
    output <- rbind(output, c(3,5,7))
    output <- rbind(output, c(8,1,6))
    return(output)
  }

  calculate_elements <-function(A){
    return(length(A))
  }

  row_to_zero <- function(A, i){
    A[i, ] <- A[i, ] * 0
    return(A)
  }

add_elements_to_matrix <- function(A, x, i, j){
    A[i,j] <- A[i,j] + x
    return(A)
}

my_magic_list <-function(){
  magic_list <- list(info = "my own list",
  my_num_vector(),
  my_magic_matrix())
return(magic_list)
}

change_info <-function(x, text){
    x[1]<-text
    return(x)
}

add_note <-function(x, note){
    x["note"] <- note
    return(x)
}