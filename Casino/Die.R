##Imports
library('tidyverse')
library(ggplot2)


#######Creating the die


roll <- function(dice = 2){
  die <- 1:6
  
  dice <- sample(die, 2, replace = TRUE)
  return(sum(dice))
}


x <- c(1, 2, 2, 3, 4, 7, 8)
y <- x^2

qplot(x,y)
qplot(x, bins = 10)
