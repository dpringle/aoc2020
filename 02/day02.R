rm(list = ls())
setwd('/Users/daniel/Code/aoc2020/02')
library(tidyverse)

dt <- read.delim('input02.txt', header = F)
setDT(dt)
colnames(dt) <- c('raw')

 # dt <- data.table( raw = c('1-3 a: abcde',
 #                          '1-3 b: cdefg',
 #                          '2-9 c: ccccccccc'))

# Part 1

cols <- c('rule', 'letter', 'pw')
dt[, (cols) := tstrsplit(raw, ' ', fixed=TRUE)]
dt[, letter := gsub(':','', letter)]

cols <- c('min', 'max')
dt[, (cols) := tstrsplit(rule, '-', fixed=TRUE)]
dt[, min := as.numeric(min)]
dt[, max := as.numeric(max)]

head(dt)
dt[, letter_count := as.numeric(str_count(get('pw'), pattern = get('letter')))]
dt[,valid1 := 0]
dt[(get('letter_count') >= get('min')) & (get('letter_count') <= get('max')), valid1 := 1]

head(dt, 20)
sum(dt$valid1)

# Part 2

dt[, min_ch := substr(get('pw'), start = get('min'), stop = get('min'))]
dt[, max_ch := substr(get('pw'), start = get('max'), stop = get('max'))]
zero_cols <- c('min_ch_match', 'max_ch_match', 'valid2')
dt[, (zero_cols) := 0]
dt[(get('min_ch') == get('letter')), min_ch_match := 1]
dt[(get('max_ch') == get('letter')), max_ch_match := 1]
dt[ get('min_ch_match')+get('max_ch_match') == 1, valid2 := 1]

head(dt)
sum(dt$valid2)
