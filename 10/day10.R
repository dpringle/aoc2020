rm(list = ls())
setwd('/Users/daniel/Code/aoc2020/10')
library(tidyverse)
library(data.table)
library(data.tree)

#r <- read_file('input10test.txt')
r <- read_file('input10.txt')

input <- as.vector(as.numeric(gsub("\n", " ", unlist(strsplit(r, "\n")))))
socket <- 0
device <- 3 + max(input)
jolt <- sort(c(socket, input, device))

d <- data.table('jolt' = jolt)
d[, 'diff' := jolt - shift(jolt, 1)]
d
table(d$diff)

ones <- sum(d[["diff"]]==1, na.rm = T)
threes <- sum(d[["diff"]]==3, na.rm = T)
answer1 <- ones*threes
answer1

# Part 2 - 
# For a general solution, grow a data tree with options that work
# Inspect the data and look at current differences, all ones and threes:

# Get the number of '1's in between each '3'
ones <-  nchar(unlist(strsplit(paste0(as.character(d[!is.na(diff),diff]),collapse=""), "3", fixed=TRUE)))
max(ones)
hist(d$diff)
hist(ones)

# If there are 0 or 1 consecuritve diffs of 1, these cannot be replaced and there is only
# one sequence available . For the other three cases, the numebr of alternatives is 
#   2 ones : 2 options (+1+1, +2)
#   3 ones : 4 options (+1+1+1, +1+2, +2+1, +3)
#   4 ones : 7 options (+1+1+1+1, +1+1+2, +2+1+1, +1+2+1, +1+3, +3+1, +2+2)
# Total number options is product of how many ways each string of '1s' can be replaced. 
# '3s' cannot be replaced, and there are no '2s' gaps

opts <- rep(1, length(ones))
opts[ones==2] <- 2
opts[ones==3] <- 4
opts[ones==4] <- 7

#need as.character() to avoid scientific notation
answer2 = as.character(prod(opts))
answer2
