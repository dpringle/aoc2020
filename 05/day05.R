rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/05')
library(tidyverse)
library(data.table)

d <- read.table('input05.txt')
setDT(d)
colnames(d) <- c("raw")

cols <- c(paste0('r', seq(1,7)), paste0('s', seq(1,3)))

d[, (cols) := tstrsplit(raw, "", fixed=TRUE)]

d[, rw_bin := gsub("F", "0", substr(raw,1,7))]
d[, rw_bin := gsub("B", "1", rw_bin)]

d[, st_bin := gsub("L", "0", substr(raw,8,10))]
d[, st_bin := gsub("R", "1", st_bin)]

d[, rw_num := as.numeric(rw_bin)]
d[, st_num  := as.numeric(st_bin)]

# Argh - kludge! Need to learn nice way to convert binary to decimal

d[, row := 
    (r1=='B')*64 + 
    (r2=='B')*32 + 
    (r3=='B')*16 + 
    (r4=='B')*8 + 
    (r5=='B')*4 + 
    (r6=='B')*2 + 
    (r7=='B')*1]

d[, seat := 
    (s1=='R')*4 + 
    (s2=='R')*2 + 
    (s3=='R')*1]

d[, ID :=  row*8 + seat]
head(d)
glimpse(d)


ID = data.table(d[,ID])

names(ID)
setorderv(ID,'V1')
ID = ID[,V1]

maxID <- max(ID)  

## Part 2
checks = seq(from = min(ID), to = max(ID))
myseat <- checks[!(checks %in% ID)]
myseat

