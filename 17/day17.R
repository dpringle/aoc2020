rm(list = ls()); gc()
setwd('/Users/daniel/Code/aoc2020/17')
library(tidyverse)
library(data.table)
library(abind)

m0 <- scan('input17.txt', what = character()) 
#m0 <- scan('input17test.txt', what = character()) 

make_mat <- function(m0){
  m0 <- gsub("#", 1, m0)
  m0 <- gsub("\\.", 0, m0)
  m0 <- data.table(m0)
  
  mx = copy(m0)
  nrow <- nrow(mx) 
  nch <- nchar(mx[1,])
  cols <- c(paste0("c", seq(1,nch)))
  
  mx[, (cols) := tstrsplit(m0, "", fixed=TRUE)]
  mx[, m0 := NULL]
  m <- as.matrix(mx)
  storage.mode(m) <- "numeric"
  return(m)
}

stack_mat <- function(m,iters) {
  sm <- m
  for (i in 1:(iters-1)){
    smi =  m
    sm <- abind(sm,smi, along = 3)
  }
  return(sm)
}

build_grid <- function(m,iters=iters) {

  nr_m <- nrow(m)
  nc_m <- ncol(m)
  
  zero_mat <- matrix(0, nrow = 2*iters + nr_m, ncol = 2*iters + nc_m)
  zero_ends <- matrix(0, nrow = iters,       ncol = 2*iters + nc_m)
  zero_side <- matrix(0, nrow = nr_m, ncol = iters)
  
  pad_layer <- rbind(zero_ends,
                     cbind(zero_side, m, zero_side),
                     zero_ends)
  
  zero_mats <- stack_mat(zero_mat, iters)
  grid <- abind(zero_mats, pad_layer, zero_mats, along = 3) 
  return(grid)
}

count_nn_cube <- function(a,i,j,k) {
  
  rows = max(1, i-1):min(i+1,dim(a)[1])
  cols = max(1, j-1):min(j+1,dim(a)[2])
  mats = max(1, k-1):min(k+1,dim(a)[3])
  
  nnx <- sum(a[rows,cols,mats])-a[i,j,k]
  return(nnx)
}

update_cube <- function(a,i,j,k){

  # If a cube is active and exactly 2 or 3 of its neighbors are also active, 
  # the cube remains active. Otherwise, the cube becomes inactive.
  
  # If a cube is inactive but exactly 3 of its neighbors are active, the cube 
  # becomes active. Otherwise, the cube remains inactive.
  
  status  <- a[i,j,k]
  nn_on <- count_nn_cube(a,i,j,k)

  stat <- 0
  if(status == 1 & nn_on  %in% c(2,3)) stat <- 1
  if(status == 0 & nn_on == 3) stat <- 1
  
  return(stat)
  
  }

iterate_array <- function(a, iters){
  
  print(paste0("Sum = ", sum(a)))
  
  stopifnot(iters>0)
  
  nr <- dim(a)[1]
  nc <- dim(a)[2]
  nm <- dim(a)[3]
  
  for(t in 1:iters) {
    a_t <- a
    
    for (i in 1:nr) {
      for (j in 1:nc) {
        for (k in 1:nm) {
          
          #a[i,j,k] <- incr_on_1(a_t,i,j,k)
          a[i,j,k] <- update_cube(a_t,i,j,k)
          
        }
      }
    }
    print(paste0("Iterations done = ", t))
    print(paste0("Sum = ", sum(a)))
  }
  
  return(a)
  
}

# Part 1 ----

iters <- 6
m <- make_mat(m0)

grid <- build_grid(m,iters)

result <- iterate_array(grid,iters)
answer1 = sum(result)
answer1

# Part 2 ----

# 4-D hypercube
# need to go to 4-D array etc
