# Code and Coffee - February 22nd, 2020
## Making Your Own Functions

# load packages
library(tidyverse)

### BASIC THEORY ###
# 1. Basic function without any arguments
  fun1 <- function(){
    return("Basic function")
  }
  ## run function
  fun1()
  ## save output of function in an object
  obj1 <- fun1()
  obj1

# 2. Basic function with an argument
  fun2 <- function(arg){
    return(arg + 5)
  }
  ## run function with different arguments
  fun2(0)
  fun2(5)
# 2.1. Create a local object within the function
  fun2.1 <- function(arg){
    local_obj <- arg + 5
    return(local_obj)
  }
  ## run function. note - local obj does not appear in the global environment
  fun2.1(0)
# 2.2. Create a global object within the function
  fun2.2 <- function(arg){
    global_obj <<- arg  - 5 # special arrow
    local_obj <- arg + 5
    return(local_obj)
  }
  ## run function. note - global object appears in the global environment
  fun2.2(0)
# 2.3. Basic function with multiple arguments
  fun2.3 <- function(arg1, arg2){
    return(arg1 - arg2)
  }
  ## run function
  fun2.3(5, 10)
  
# 3. Adding if else statements
  fun3 <- function(arg){
    if(arg > 0){
      message <- "Positive"
    }else if(arg < 0){
      message <- "Negative"
    }else if(arg == 0){
      message <- "Zero"
    }
      return(message)
  }
  ## run function
  fun3(5)
  fun3(-5)
  fun3(0)
# 3.1. Alternative method that does the same thing
  fun3.1 <- function(arg){
    if(arg > 0){
      return("Positive")
    }else if(arg < 0){
      return("Negative")
    }else if(arg == 0){
      return("Zero")
    }
  }
  ## run function
  fun3.1(5)
  fun3.1(-5)
  
# 3.2. Store results in a global object
# create an empty vector
  results_vector <- NA
# function
  fun3.2 <- function(arg){
    if(arg > 0){
      message <- "Positive"
    }else if(arg < 0){
      message <- "Negative"
    }else if(arg == 0){
      message <- "Zero"
    }
      return(messa)
  }
  
  
# 4. Looping function using different methods and storing results
# 4.1. For loop
# 4.1.1. Storing in a vector
  # create vector of random numbers
  random_vector <- runif(25, -100, 100)
  # create empty vector
  results_vector <- c()
  # run function within foor loop
  for(i in random_vector){
    results_vector <<- c(results_vector, fun3(i))
  }
  # look at results
    results_vector
# 4.1.2. Storing in a dataframe
  # create dataframe of random numbers
  results_df <- data.frame(arg = runif(25, -100,100), result = rep(NA, 25))
  # run function within for loop 
  for(i in results_df$arg){
    results_df <<- results_df %>% 
      mutate(result = ifelse(arg == i, fun3(i), result))
  }
  # look at results
  results_df
# 4.2. (m)apply function
# 4.2.1. Storing in a vector
  results_vector_2 <- mapply(fun3, arg = random_vector)
# 4.2.2. Storing in a dataframe
  # create empty dataframe
  results_df_2 <- data.frame(arg = numeric(), result = character())
  # modify function to return a global dataframe
  fun3_df <- function(arg){
    if(arg > 0){
      message <- "Positive"
    }else if(arg < 0){
      message <- "Negative"
    }else if(arg == 0){
      message <- "Zero"
    }
    df_row <- data.frame(arg = arg, result = message)
    results_df_2 <<- rbind(results_df_2, df_row)
  }
  # run function using mapply
  mapply(fun3_df, random_vector)
  # look at dataframe
  results_df_2
# 4.3. map function (from tidyverse)
  # map gives a list as an output
  map(random_vector, fun3)
# 4.3.1. Storing in a vector 
  results_vector_3 <- map(random_vector, fun3) %>% unlist()
# 4.3.2. Storing in a dataframe
  # create empty dataframe
  results_df_3 <- data.frame(arg = numeric(), result = character())
  # modify function to return a different global dataframe
  fun3_df2 <- function(arg){
    if(arg > 0){
      message <- "Positive"
    }else if(arg < 0){
      message <- "Negative"
    }else if(arg == 0){
      message <- "Zero"
    }
    df_row <- data.frame(arg = arg, result = message)
    results_df_3 <<- rbind(results_df_3, df_row)
  }
  results_df_3 <- map(random_vector, fun3_df2)[25]

