rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/07')
library(tidyverse)
library(data.table)
library(data.tree)

r <- read_file('input07test.txt')
#r <- read_file('input07.txt')

rs <- unlist(strsplit(r, "\n"))
rs <- gsub("\n"," ", rs)
d <- data.table(raw = rs)
d[, name := trimws(gsub("bags", "", tstrsplit(d$raw, 'contain', fixed=TRUE)[[1]]))]
d[, kids := trimws(gsub("bag", "", (gsub("\\.", "", gsub("bags", "", tstrsplit(d$raw, 'contain', fixed=TRUE)[[2]])))))]
d[, n_kids := 1+str_count(kids, ",")]
max_kids = max(d[,n_kids])
col_kids = paste0("kid", seq(1:max_kids))
d[, (col_kids) := tstrsplit(kids, ',', fixed=TRUE)]

f <- copy(d)
f[, c('raw', 'kids','n_kids') := NULL]
id.vars = c("name")
measure.vars = setdiff(names(f), id.vars)

fm = melt(f, id.vars = id.vars,
             measure.vars = measure.vars)
fm[, variable := NULL]
fm[, value := trimws(value)]
fm <- fm[!is.na(value)]
setorderv(fm,c('name'))
fm[, child:= trimws(gsub('[0-9]', "", value))]
fm[, number := parse_integer(gsub('[a-z]', "", value))]
fm[, value := NULL]
fm[child == "no other", `:=` (child = NA,
                              number = 0)]
dt <- fm[!is.na(child),]

top = unique(fm$name)[!unique(fm$name) %in% unique(fm$child)]
rt <- data.table('name' = rep('top',length(top)), 
                 'child' = top,
                 'number' = 1)
dt = rbind(rt, dt)
dt

dt[, name := gsub(" ", "", dt$name)]
dt [, child := gsub(" ", "", dt$child)]
dt
bags <- FromDataFrameNetwork(dt)

paths <- bags$Get('path')
pathsn <- bags$Get('number')
pathsn
sg <- paths[names(paths)=="shinygold"]

N = length(sg)

nm <- paste0("shinygold", seq(1:N))
names(sg) <- nm

dont = c('top', 'shinygold')
length(setdiff(unique(unlist(sg)), dont))


bags$totalin <- function(self) sum(sapply(self$children, function(x) x$number))
print(bags, "totalin")
       