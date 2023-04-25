rm(list = ls()); gc()
setwd('/Users/daniel/Code/aoc2020/11')
library(tidyverse)
library(data.table)

#d <- as.data.frame(scan('input11test.txt', what = character()))
d <- as.data.frame(scan('input11.txt', what = character()))
setDT(d)
colnames(d) <- c("raw")
n_cols <- nchar(d[1,1])
cols <- c(paste0('col', seq(1,n_cols)))
d[, (cols) := tstrsplit(raw, "", fixed=TRUE)]
d[['raw']] = NULL
table(unlist(d))

# functions

is_floor <- function(d,r,c) as.matrix(d)[r,c]=="."
is_empty <- function(d,r,c) as.matrix(d)[r,c]=="L"
is_occ   <- function(d,r,c) as.matrix(d)[r,c]=="#"

count_nn_x <- function(d,r,c, x = "#") {
  m = as.matrix(d)
  rows = max(1, r-1):min(r+1,nrow(m))
  cols = max(1, c-1):min(c+1,ncol(m))
  nnx <- sum(m[rows,cols]==x) - as.numeric(m[r,c]==x)
  return(nnx)
}

print_newseats <- function(){
  print('new_seats')
  print(new_seats)
}

# retain data.table for speed
seats <- as.matrix(d)
new_seats <- copy(seats)

nr <- nrow(seats)
nc <- ncol(seats)

constant = 0
id = 0

while (constant == 0) {
  
  id <- id + 1
  print(id)
  
  for (r in 1:nrow(seats)) {
    for (c in 1:ncol(seats)){
        
        if (is_empty(seats,r,c) & (count_nn_x(seats,r,c, x = "#") == 0) ) {
          #new_seats[[c]][r] <- "#"
          new_seats[r,c] <- "#"
        }
        
        if (is_occ(seats,r,c) & (count_nn_x(seats,r,c, x="#") >= 4) ) {
          #new_seats[[c]][r] <- "L"
          new_seats[r,c] <- "L"
        }
    }
  }
  
  print(paste0('seats the sames: ', 100*sum(seats == new_seats) / (nrow(seats)*ncol(seats)), " %"))
  constant <- identical(seats, new_seats)
  #print_newseats()
  seats <- copy(new_seats)
  
}

answer1 <- sum(seats=="#")
answer1
