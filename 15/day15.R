rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/15')
library(tidyverse)
library(data.table)

# Given the starting numbers 1,3,2, the 2020th number spoken is 1.
# Given the starting numbers 2,1,3, the 2020th number spoken is 10.
# Given the starting numbers 1,2,3, the 2020th number spoken is 27.
# Given the starting numbers 2,3,1, the 2020th number spoken is 78.
# Given the starting numbers 3,2,1, the 2020th number spoken is 438.
# Given the starting numbers 3,1,2, the 2020th number spoken is 1836.

# Problem input: "13,16,0,12,15,1"

r <- "13,16,0,12,15,1"
#r <- "3,1,2"
answer_row <- 2020

start = as.numeric(unlist(str_split(r,",")))
d <- data.table('value' = start, 'count' = 1)
d[,'ID' := .I]
col_order <- c('ID', 'value', 'count')
setcolorder(d, col_order)

next_row <- function(d){
  
  ID_last   <- as.numeric(d[nrow(d),'ID'])
  value_last  <- as.numeric(d[nrow(d),'value'])
  count_last<- as.numeric(d[nrow(d),'count'])
  
  count_last1 <- max(1,count_last -1)
  
  ID_count_prev <- as.numeric(d[(get('count') == count_last1 & get('value') == value_last), 'ID'])
  
  next_value <- ID_last - ID_count_prev 
  count_next_value <- 1 + nrow(d[get('value') == next_value,])
  ID_next_value <- nrow(d)+1
  
  d_next <- data.table('ID' = ID_next_value,
                       'value' = next_value,
                       'count' = count_next_value)
  return(d_next)
}

# Part 1

n_start = nrow(d)
n_end <- answer_row

for (i in n_start:(n_end-1)) {
  
  if (i %% 1000 == 0) print(paste0("i = ", i))
  d_next <- next_row(d)
  d <- rbind(d, d_next)
  
}

(answer = as.numeric(d[get('ID')==n_end,'value']))

# Part II 
# Requires a much faster approach as above takes ~ 1 s for 1000 rows, so would take 8-9 hrs for 30M rows ..
# Can probably set up as a matrix problem instead
