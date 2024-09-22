library(markmyassignment)
lab_path <-
"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
name <- "Christian Jonsson"
liuid <- "chjon338"

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

# comment: only syntax
my_num_vector <- function() {
    float_v <- c(log10(11), cos(pi/5), exp(pi/3), (1173%%7)/19)
    return(float_v)
}

# comment: loop through the values in the vector and append the output vector and evaluate the number using the ternary operator  
filter_my_vector <- function(x, leq){
  output <- c()

  for(num in x){
    output <- append(output, ifelse(leq <= num, NA, num))
  }

  return(output)
}

# Here I had an issue with the commented out function, i checked the values from the unit test and my function outputted the same. You should check your tolerance on the unittest.
 dot_prod <- function(a, b){
  #return(a %*% b)
  return(sum(a * b))

 }

# could have used a for loop but think the while makes it cleaner to state the purpose of the N
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

  # used rbind to make it clearer when reading the code to "see" the matrix
  my_magic_matrix <- function(){
    output <- c()
    output <- rbind(output, c(4,9,2))
    output <- rbind(output, c(3,5,7))
    output <- rbind(output, c(8,1,6))
    return(output)
  }

# used length
  calculate_elements <-function(A){
    return(length(A))
  }

  # use matrix indexing to obtain the row vector of correct size, and insert on the right location 
  row_to_zero <- function(A, i){
    A[i, ] <- A[i, ] * 0
    return(A)
  }

# use the provided indices to insert the value
add_elements_to_matrix <- function(A, x, i, j){
    A[i,j] <- A[i,j] + x
    return(A)
}

# use the list function and the previously declared functions to create my list
my_magic_list <-function(){
  magic_list <- list(info = "my own list",
  my_num_vector(),
  my_magic_matrix())
return(magic_list)
}

# since i know x contains the element "info" i can obtain the index by name and change it
change_info <-function(x, text){
    x[["info"]]<- text
    return(x)
}

# add a new element by name indexing
add_note <-function(x, note){
    x["note"] <- note
    return(x)
}

# Check if the element is numeric. Then assumes it is a numeric vector and sums it. This is a silly approach since i could have a list in a list and would result in missed values.
# Could use unlist() and the do some evaluations but after some discussions with the lab assistant this solution was deemed sufficient.
sum_numeric_parts <- function(x){
  output <- 0
  for(num in x){
    if(is.numeric(num)){
      output = output + sum(num)    
    }
  }
  return(output)
}

# syntax
my_data.frame <- function(){
  data.frame(
  id = c(1,2,3),
  name = c("John","Lisa","Azra"),
  income = c(7.30,0.00,15.21),
  rich = c(FALSE,FALSE,TRUE)
)
} 

# breaked up the problem into 2, sort the column with the matching name in decending order and obtaining the indices. Then use the first n elements in the index vector to create a sorted matrix.
sort_head <-function(df, var.name, n){
  indexes <- order(df[[var.name]],decreasing = TRUE)
  return(df[indexes[1:n],])
}

# 1) obtain median, 2) loop through the values and evaluate the conditions, 3) append the string into the string vector, 4) add the vector to the data frame
add_median_variable<-function(df, j){
  median_val<- median(df[,j])
  output_str <- c()
  tmp_str = ""
  for(num in df[,j]){
    if(num > median_val){
      tmp_str = "Greater"
    } else if (num < median_val) {
      tmp_str = "Smaller"
    }else{
      tmp_str = "Median"
    }
    output_str <- append(output_str,tmp_str)
  }
  df <- cbind(df, compared_to_median = output_str)
  return(df)
}

# 1) extract the right columns from the matrix, 2) obtain the names from the columns, 3) insert the data in the list 4) name the lists elements
# Created a helper function to make it easier and cleaner 
analyze_columns <-function(df, j){ 
  matrix_eval <- df[,j]
  theNames <- colnames(matrix_eval)
  output <- list(
    get_mmstd(matrix_eval[,1]),
    get_mmstd(matrix_eval[,2]),
    cor(matrix_eval))

  names(output) <- c(theNames,"correlation_matrix")
  return(output)
}

get_mmstd <-function(vector){
  output <-c(
   mean(vector),
   median(vector),
   sd(vector))
  names(output) <- c("mean","median","sd")
return(output)
}

mark_my_assignment()
