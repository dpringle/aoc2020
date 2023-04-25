rm(list = ls()); gc()
setwd('/Users/daniel/Code/aoc2020/13')
library(tidyverse)
library(data.table)

d <- as.data.table(read.table('input13test.txt'))
d <- as.data.table(read.table('input13.txt'))

colnames(d) <- c("raw")

calc_bus_waits <- function(d) {
  
  ts <- as.numeric(d[["raw"]][1])
  
  bus <- d[["raw"]][2]
  bus <- data.table(nb = (unlist(tstrsplit(bus, ","))))
  bus <- bus[nb != 'x', ] 
  bus[, 'nb' := parse_number(trimws(get('nb')))]
  bus[, 'last' := (ts %/% get('nb'))*get('nb')] 
  bus[, 'wait' := get('last') + get('nb') - ts]
  bus[, 'prod' := get('wait')*get('nb')]
  
  bus_min <- bus[get('wait') == min(get('wait')),][['prod']]
  return(bus_min)
}

# Find answer
answer1 <- calc_bus_waits(d)
answer1

