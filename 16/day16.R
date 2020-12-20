rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/16')
library(tidyverse)
library(data.table)


r <- unlist(str_split(read_file('input16.txt'), "\n\n"))

prep_rules<- function(r){
  
  rules <- data.table(raw = unlist(tstrsplit(r, ",", fixed=TRUE)))
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

  return(rules)
  }


get_valid_numbers <- function(rs){
  
  rc <- data.table(number = seq(1,1000))
  
  for ( i in 1:nrow(rs)) {
    nm <- rs$field[i]
    rc_i <- data.table()
    range1 <- seq(rs$from1[i], rs$to1[i])
    range2 <- seq(rs$from2[i], rs$to2[i])
    range <- c(range1,range2)
    rc_i[, 'number' := seq(1,1000)]
    rc_i[, (nm) := 0]
    rc_i[(get('number') %in% range), (nm) := 1]
    rc_i$number <- NULL
    rc <- cbind(rc, rc_i)
  }
 
  nc_min = 2
  nc_max = ncol(rc)
  
  rc[, Sum := rowSums(.SD, na.rm = T), .SDcols = nc_min:nc_max]
  rc[, valid := 0]
  rc[get('Sum') > 0, valid := 1]
  valid_numbers <- as.numeric(rc[get('valid')==1, 'number'][["number"]])
  return(valid_numbers)
  
}

prep_my_ticket<- function(r){
  mytx <- data.table(num = unlist(tstrsplit(r[2], ",", fixed=TRUE)))
  return(mytx)
}

prep_tickets<- function(r){
  t <- r
  n_t = length(t)
  tx <- data.table()
  for (i in 1:n_t) {
    nmi <- paste0("t",i)
    txi <- data.table(as.numeric(unlist(tstrsplit(t[i], ",", fixed=TRUE))))
    names(txi) <- nmi
    tx <- cbind(tx, txi)
  }
  return(tx)
}

ticket_error<- function(t, valid){
  ch <- data.table(t)
  colnames(ch) <- 'num'
  ch[, check := 0]
  ch[get('num') %in% valid, check := 1]
  View(ch)
  err <- ch[get('check')==0,num]
  err <- as.numeric(err)
  err
  return(sum(err))
  }

all_ticket_errors <- function(tx, vn){
  
  all_errors<- rep(0,ncol(tx))
  for (i in 1:ncol(tx)) {
    t <- tx[,..i]
    err_i <- ticket_error(t, vn)
    all_errors[i] <- err_i
  }
  return(all_errors)
}

# Part 1 

header <- unlist(str_split(r[[1]], "\n"))
your_ticket <- unlist(str_split(r[[2]], "\n"))[-1]
other_tickets <- unlist(str_split(r[[3]], "\n"))[-1]

rules <- prep_rules(header)
valid_numbers <- get_valid_numbers(rules)

mytx <- prep_my_ticket(your_ticket)
tx <- prep_tickets(other_tickets)

errors <- all_ticket_errors(tx, valid_numbers)
errors

answer1 = sum(errors)
answer1

# Part 2

valid_idx = errors == 0
valid_tx <- tx[, ..valid_idx]
valid_tx

