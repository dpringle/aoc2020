rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/18')
library(tidyverse)
library(data.table)

m <- read_lines('input18test.txt') 
m <- read_lines('input18.txt') 
m  <- gsub(" ","", m)

# 1. Find deepest bracketed expression
# 2. Extract it as a string, incl ( )
# 3. Evaluate inside, one step at a time
# 4. Replace the expr in 2 with its value
# 5. Loop

total <- 0
#m <- m[40]

for (i in 1:length(m)){
  
  s <- m[i]

  print(paste0("Line = ", i))
  
  while(str_count(s, "\\(")  > 0) {
    
    sp <- unlist(strsplit(s, ""))
    n_brac <- cumsum(ifelse(sp=="(", 1, ifelse(sp ==  ")", -1, 0)))
    depth = max(n_brac)
    exp <- unlist(str_split(paste(sp[n_brac == depth], collapse = ""),"\\("))[2]
    exp_rep <- paste0("\\(", exp,"\\)")
    exp_rep <- gsub("[+]", "\\\\+", exp_rep)
    exp_rep <- gsub("[*]", "\\\\*", exp_rep)
    
    n_ops <- length(strsplit(gsub("[0-9]","",exp),"")[[1]])
    
    for (i in 1:n_ops){
      print(paste0(" n = ", i))
      n1 <- as.numeric(unlist(strsplit(exp, "\\D+"))[1])
      n2 <- as.numeric(unlist(strsplit(exp, "\\D+"))[2])
      op <- unlist(strsplit(gsub("[0-9]","",exp),""))[1]
      exp_i <- paste0(n1,op,n2)
      val_i <- eval(parse(text = exp_i))
      exp_i_gsub <- paste0(n1,"\\",op,n2)
      exp <- sub(exp_i_gsub, val_i, exp)
      print(exp)
    }
      s <- sub(exp_rep, exp, s)
  }
  
  # Deal with  case where output here has no parens
  # but has a mix of + and  * so the problem communtativity rules
  # require step by step calc
  
  exp <- s
  
  n_ops <- length(strsplit(gsub("[0-9]","",exp),"")[[1]])
  
  for (i in 1:n_ops){
    n1 <- as.numeric(unlist(strsplit(exp, "\\D+"))[1])
    n2 <- as.numeric(unlist(strsplit(exp, "\\D+"))[2])
    op <- unlist(strsplit(gsub("[0-9]","",exp),""))[1]
    exp_i <- paste0(n1,op,n2)
    val_i <- eval(parse(text = exp_i))
    exp_i_gsub <- paste0(n1,"\\",op,n2)
    exp <- sub(exp_i_gsub, val_i, exp)
    print(exp)
  }
  
  answer <- eval(parse(text = exp))
  print(paste0("answer=",answer))
  total <- total + answer
  
}
print(paste0("total=",total))

# Part 2 ----

# Now addition has preedence over * 
# Just replace x+y with (x+y) everywhere
# Using a gsub

s <- "3+4*6"

grep(D,s)
