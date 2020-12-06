rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/06')
library(tidyverse)
library(data.table)
library(stringi)

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
rs <- strsplit(rs, " ")
rs <- lapply(rs, FUN=function(X) data.table(X))
rs <- lapply(rs, FUN=function(X) `colnames<-`(X, "raw"))


as.character(rs[[1]][1])

'w' %in% strsplit(as.character(rs[[1]][1]), "", fixed = TRUE)[1]

'w' %in% strsplit(as.character(rs[[1]][1]), "", fixed = TRUE)[[1]]
