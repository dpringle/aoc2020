rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/16')
library(tidyverse)
library(data.table)

r <- unlist(str_split(read_file('input16.txt'), "\n"))

prep_rules<- function(r){
  
  rules <- data.table(raw = unlist(tstrsplit(r[1:20], ",", fixed=TRUE)))
  rules[, field := tstrsplit(raw, ":", fixed=TRUE)[1]]
  rules[, ranges := tstrsplit(raw, ":", fixed=TRUE)[2]]
  rules[, c('ranges', 'range1', 'range2') := c(list(NULL), tstrsplit(ranges, "or", fixed=TRUE))]
  rules[, c('range1', 'from1', 'to1') := c(list(NULL), tstrsplit(range1, "-", fixed=TRUE))]
  rules[, c('range2', 'from2', 'to2') := c(list(NULL), tstrsplit(range2, "-", fixed=TRUE))]
  rules[["raw"]] <- NULL
  rules[, `:=`(from1 = as.numeric(from1),
               to1 = as.numeric(to1),
               from2 = as.numeric(from2),
               to2 = as.numeric(to2))]

  glimpse(rules)
  return(rules)
  }

prep_my_ticket<- function(r){
  mytx <- data.table(num = unlist(tstrsplit(r[23], ",", fixed=TRUE)))
  return(mytx)
}

prep_tickets<- function(r){
  t <- r[36:length(r)]
  n_t = length(t)
  tx <- data.table()
  for (i in 1:n_t) {
    nmi <- paste0("t",i)
    txi <- data.table(unlist(tstrsplit(t[i], ",", fixed=TRUE)))
    names(txi) <- nmi
    tx <- cbind(tx, txi)
  }
  return(tx)
}

rules <- prep_rules(r)
mytx <- prep_my_ticket(r)
tx <- prep_tickets(r)

