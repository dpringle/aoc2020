rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/09')
library(tidyverse)
library(data.table)

#r <- read_file('input09test.txt')
#n <- 5
r <- read_file('input09.txt')
n <- 25

d <- data.table(raw = as.numeric(gsub("\n", " ", unlist(strsplit(r, "\n")))))


n_row <- nrow(d)

d[, valid := 0]
d[1:n, valid := 1]
d[, row_nm := .I]
d

calc_good_sums <- function(v) {
  good_sums <- expand.grid(n1 = v, n2 = v) %>%
    mutate(sum = n1 + n2) %>%
    filter(n1 != n2) %>%
    select(sum) %>%
    unique()
  gsv <- good_sums$sum
  return(gsv)
}

for (i in (n+1):n_row) {
  last_n <- as.vector(d[(i - n):(i - 1), raw])
  allowed <- as.numeric(calc_good_sums(last_n))
  valid = as.numeric(d[i, raw])  %in% allowed
  if (valid == 0) {
    row = i
    first_bad = as.numeric(d[i,raw])
    break
  }
}

answer1 <- first_bad
answer1
# Part 2

target = first_bad
found <- 0
v <- d[,raw]

 
for (n in 2:(n_row-1)) {
  for (i in 1:n_row) {
    
    if (i+n > n_row) {
      break
    }
    
    print(paste0('i = ', i))
    print(paste0('n = ', n))
    print(paste0('i+n = ', i+n))
    sum <- sum(v[i:(i + n - 1)])
    print(paste0('s = ', sum))
    
    if (sum == target) {
      found <- 1
      i_star <- i
      n_star <- n
    }
    if(found == 1){break}
  }
  if(found == 1){break}
}


i_star
n_star
sub <- v[i_star:(i_star+n_star-1)]
sum(sub)
target
answer2 = min(sub)+max(sub)
answer2
