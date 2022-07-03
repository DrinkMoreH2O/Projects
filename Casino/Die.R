##Imports
library('tidyverse')
library(ggplot2)


#######Creating the die


roll <- function(dice = 2){
  die <- 1:6
  
  dice <- sample(die, 2, replace = TRUE)
  return(sum(dice))
}

#######Creating the weighted die


roll <- function(dice = 2){
  die <- 1:6
  
  dice <- sample(die, dice, replace = TRUE, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  return(sum(dice))
}

rolls <- replicate(10000, roll())

qplot(rolls, binwidth = 1)

