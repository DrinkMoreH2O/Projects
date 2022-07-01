
############# Activity 1 ###################
# Function to set up the grid

createGrid <- function(grid_size=c(10,10), percent_vacc=0.10){
  #Set an array full of 1s of the specified grid size
  y <- array(1, grid_size)
  pop_size <- length(y)
  
  #Randomly select pop to vaccinate
  vacc_peeps <- sample(pop_size, (pop_size * percent_vacc), replace = FALSE)
  
  #Distribute vax
  for(i in vacc_peeps){
    y[i] <- 2
  }
  return(y)
}

# Test code
# Should create a 6x4 grid with 18 2s and six 1s randomly distributed
print(createGrid(c(6,4), 0.75))



########### Activity 2 ########################
# Function to check whether two arrays are identical
print("Activity 2")

areIdentical <- function(array_1, array_2){
  
  flag <- TRUE
  
  if(nrow(array_1) != nrow(array_2)){
    flag <- FALSE
    return(flag)
  }
  if(ncol(array_1) != ncol(array_2)){
    flag <- FALSE
    return(flag)
  }
  counter <- 0
  
  for(i in array_1){
    counter <- counter + 1
    if(i != array_2[counter]){
      flag = FALSE
      }
  }
  return(flag)
  
  
}
#

# Test code
testGrid1 <- array(data = c(numeric(23), 1), dim = c(6, 4))
testGrid2 <- testGrid1
print(areIdentical(testGrid1, testGrid2)) # Should print TRUE
testGrid2 <- array(data = c(numeric(10), 1, numeric(13)), dim = c(6, 4))
print(areIdentical(testGrid1, testGrid2)) # Should print FALSE
testGrid3 <- array(data=1:6, dim = c(3,2))
testGrid4 <- array(data=1:6, dim = c(2,3))
print(areIdentical(testGrid3, testGrid4)) # Should print FALSE
testGrid5 <- array(data=c(3,2,4,4,5,6), dim = c(2,3))
print(areIdentical(testGrid5, testGrid4)) # Should print FALSE



############# Activity 3 ######################
# Function to find the coordinates of all neighbors
# of a given spot in an array
print("Activity 3")

findNeighbors <- function(grid, pos_vector){
  
  x = pos_vector[1]
  y = pos_vector[2]
  
  neighbors <- list()
  
  counter <- 1
  
  
  if(!(x - 1 < 1)){
    neighbors[[counter]] <- c(x - 1, y)
    counter <- counter + 1
  }
  if(!(x + 1 > nrow(grid))){
    neighbors[[counter]] <- c(x + 1, y)
    counter <- counter + 1
  }
  if(!(y - 1 < 1)){
    neighbors[[counter]] <- c(x, y - 1)
    counter <- counter + 1
  }
  if(!(y + 1 > ncol(grid))){
    neighbors[[counter]] <- c(x, y + 1)
    counter <- counter + 1
  }
  ###I added invisible tag to prevent console spam
  return(invisible(neighbors))
}

# Test code
print(findNeighbors(testGrid1, c(2, 2)))
# Should print the following:
# 1 2
# 2 3
# 3 2
# 2 1
# Not necessarily in that order
print(findNeighbors(testGrid1, c(6, 3)))
# Should print the following:
# 6 2
# 5 3
# 6 4
# Not necessarily in that order


################ Activity 4 ################
# Put a 0 in a 35x50 grid that you make with createGrid().

print("Activity 4")


infected_pop <- createGrid(c(35,50))


infected_pop[sample(length(infected_pop),1)] <- 0




############## Activity 5 #################
# Function to decide whether an individual should be infected
print("Activity 5")



getsInfected <- function(grid, pos_vec){
  flag = FALSE
  
  if(grid[pos_vec[1],pos_vec[2]] == 0 | grid[pos_vec[1],pos_vec[2]] == 2 ){
    
  }else{
    neighbor_list <- findNeighbors(grid, pos_vec)
    for(i in neighbor_list){
      if(grid[i[1],i[2]] == 0){
        flag <- TRUE
        break
      }
    }
  }
  
  
  return(flag)
}

# Test code
testGrid3 <- array(data = c(numeric(23) + 1, 0), dim = c(6, 4))
print(getsInfected(testGrid3, c(5, 4))) # Should print TRUE
print(getsInfected(testGrid3, c(1, 1))) # Should print FALSE


############### Activity 6 ################
# Code to calculate the fraction of infected individuals at the end of the simulation
# This doesn't have to be a function.
# Either way, store the fraction in a variable called fractionInfected.
print("Activity 6")

infectedFraction <- function(grid){

  counter <- 0
  for(i in grid){
    if(i == 0){
      counter <- counter + 1
    }
  }
  return(counter/length(grid))
}


fractionInfected <- infectedFraction(testGrid4)





testGrid4 <- array(data = (c(numeric(9), numeric(15) + 1)), dim = c(6, 4))
# Write code to calculate the fraction of infected individuals (0s) in testGrid4
# and store that fraction in fractionInfected.



# Test code
print(fractionInfected) # Should print 0.375


############### Activity 7 #################
# Putting it all together
# Now write code to run the simulation once.
# Use the base model and 10% vaccination.
# Make sure you store fractionInfected at the end so that it prints to the console.
# To plot, look at activity 7b.  [For activity 8-17 you may want to comment it out]
print("Activity 7")


p_prime <- createGrid(c(35,50),0.1)

p_prime[sample(length(p_prime),1)] <- 0
p_old <- p_prime

image(p_prime,col = rainbow(3))

for(i in 1:nrow(p_prime)){
  for(j in 1:ncol(p_prime)){
    x <- getsInfected(p_prime,c(i,j))
    if(x == TRUE){
      p_prime[i,j] <- 0
      image(p_prime,col = rainbow(3))
      break
    }
    }
}
p_next <- p_prime

while(!(areIdentical(p_old, p_next))){
  p_old <- p_next
  for(i in 1:nrow(p_next)){
    for(j in 1:ncol(p_next)){
      x <- getsInfected(p_next,c(i,j))
      if(x == TRUE){
        p_next[i,j] <- 0
      }
    }
  }
  image(1:nrow(p_next),1:ncol(p_next),p_next,col = rainbow(3))
  Sys.sleep(0.5)
}






fractionInfected <- infectedFraction(p_next)
fractionInfected



# Test code
print(fractionInfected) # Output may vary, but should be about 0.9


