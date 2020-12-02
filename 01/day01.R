rm(list = ls())
setwd("/Users/danielpringle/Code/aoc2020/01")
library(tidyverse)

# Test data
#d <- as.integer(c(1721, 979, 366, 299, 675, 1456))

df <- read.delim('input01.txt', header = F)
d <- as.numeric(df[[1]])

## Part 1
d2 <- as.data.table(expand.grid(d,d))
d2[, sum := rowSums(.SD), .SDcols = 1:2]
d2[, prod := Var1*Var2]
d2[sum == 2020,]
d2[sum == 2020,prod][1]

## Part 2
d3 <- as.data.table(expand.grid(d,d,d))
d3[, sum := rowSums(.SD), .SDcols = 1:3]
d3[, prod := Var1*Var2*Var3]
d3[sum == 2020,]
d3[sum == 2020,prod][1]
