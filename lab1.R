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
  #return(a %*% b)
  return(sum(a * b))

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
    x[["info"]]<- text
    return(x)
}

add_note <-function(x, note){
    x["note"] <- note
    return(x)
}

sum_numeric_parts <- function(x){
  output <- 0
  for(num in x){
    if(is.numeric(num)){
      output = output + sum(num)    
    }
  }
  return(output)
}

my_data.frame <- function(){
  data.frame(
  id = c(1,2,3),
  name = c("John","Lisa","Azra"),
  income = c(7.30,0.00,15.21),
  rich = c(FALSE,FALSE,TRUE)
)
} 

sort_head <-function(df, var.name, n){
  indexes <- order(df[[var.name]],decreasing = TRUE)
  return(df[indexes[1:n],])
}

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
