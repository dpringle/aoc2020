rm(list = ls())
setwd('/Users/daniel/Code/aoc2020/08')
library(tidyverse)
library(data.table)
library(igraph)

#r <- read_file('input08test.txt')
r <- read_file('input08.txt')

d <- data.table(raw = gsub("\n"," ", unlist(strsplit(r, "\n"))))
d[, ID := .I]
cols = c('act', 'val') 
d[, (cols) := tstrsplit(raw, ' ', fixed=TRUE)]
d[, val := as.numeric(val)]
d[, raw := NULL]
d[, n_call := 0]

# Unchanged version
d0 = copy(d)

# Functions t call in loop

jump <- function(dt,ID) {
  act = as.character(dt[ID,'act'])
  val = as.numeric(dt[ID,'val'])
  jump = 1
  if (act == 'jmp') { jump = val}
  return(jump)
  }

add <- function(dt,ID) {
  add = 0
  if (dt[ID,'act'] == 'acc') {
    add = as.numeric(dt[ID,'val'])
    }
  return(add)
}

change_act <- function(outstep, dt = d0) {
  
  d = copy(dt)
  action = as.character(d[ID == outstep,'act'])
  
  if (outstep == 0) {
    d = copy(d0)
  } else {
    
    if (action == 'nop'){
      d[outstep, act := 'jmp']
    }
    
    if (action == 'jmp'){
      d[outstep, act := 'nop']
    }
  }
  return(d)
}

# n = 0
# d <- change_act(n, dt = d0)
# d0
# d
# all.equal(d,d0)

# Part 1 Loop----
# Loop following instructions

tot = 0
max = 0
row = 1
step = 0
max_row = nrow(d)

while (max <= 1 & row <= max_row){

  # Values at start of iteration
  # row_in is for reporting
  row_in = row
  step = step + 1
  tot = tot + add(d,row)
  jmp = jump(d,row)
  row = row + jmp
  row_out = row

  move = data.table(step = step,
                    row_in = row_in,
                    jmp = jmp,
                    row_out = row_out,
                    tot = tot)
  ifelse(step == 1,
         moves <- move,
         moves <- rbind(moves, move)
         )

  # Values checked in whlie condition, next iteration
  d[row, n_call := 1 + get("n_call")]
  max = max(d[,n_call])
  row = row

}
d
moves
answer = max(moves$tot) %>% print()


# Part 2 Loop----

rows_to_swap <- d0[act %in% c('nop', 'jmp') & val != 0, ID]

d0
outstep = 0
complete = 0
max_row = nrow(d)

while (complete == 0 | outstep < 10) {
  
  print(paste0('outer step = ', outstep))
  
  # Run inner loop with no changes, and then make successive changes until complete == 1
  tot = 0
  max = 0
  row = 1
  step = 0

  while (max <= 1 & complete == 0){
    
    #print(paste0('step(inner) = ', step))
    
    # Values at start of iteration
    # row_in is for reporting
    row_in = row
    step = step + 1
    tot = tot + add(d,row)
    jmp = jump(d,row)
    row = row + jmp
    row_out = row
    
    move = data.table(step = step, 
                      row_in = row_in,
                      jmp = jmp,
                      row_out = row_out,
                      tot = tot)
    ifelse(step == 1,  
           moves <- move, 
           moves <- rbind(moves, move)
    )
    
    # Values checked in while condition, next iteration
    row = row
    
    if(row > max_row) {
      complete <-  1
    } else {
      d[row, n_call := 1 + get("n_call")]
      max <-  max(d[,n_call])  
    }
    
  }
  
  outstep = outstep + 1
  row_swap <- rows_to_swap[outstep] 
  print(paste0('row_swap = ', row_swap))
  d <- change_act(row_swap, d0)
}

d0
d
moves
answer = max(moves$tot) %>% print()



