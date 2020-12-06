rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/06')
library(tidyverse)
library(data.table)

r <- read_file('input06.txt')
rs <- unlist(strsplit(r, "\n\n"))
rs <- gsub("\n"," ", rs)
rs <- gsub(" ","", rs)
d <- data.table(group = seq(1:length(rs)), all_ans = rs)
d[, nch := nchar(get("all_ans"))]

for (i in 1:nrow(d)) {
    d$n_unq_ch[i] <- n_distinct(strsplit(d$all_ans[i], "", fixed = TRUE)[[1]])
}

head(d)
sum(d$n_unq_ch)

# Part 2
r <- read_file('input06.txt')
rs <- unlist(strsplit(r, "\n\n"))
rs <- gsub("\n"," ", rs)
d <- data.table(grp = rs)
d[,nrow := 1+str_count(get('grp')," ")]
d[,grp1 := gsub(" ","", get('grp'))]

# Again, this is rough. Would like to know how to do in a 1-line dt command
# This doesn't work:
# > d[,unq := sum(table(str_split(get('grp1'),""))==get('nrow'))]
# Error in table(str_split(get("grp1"), "")) : 
#     all arguments must have the same length

for (i in 1:nrow(d)) {
    N = d$nrow[i]
    d$unq[i] <- sum(table(str_split(d[i,grp1],""))==N)
}

head(d)
sum(d[,unq])
