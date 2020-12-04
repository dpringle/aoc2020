rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/04')
library(tidyverse)
library(data.table)

r <- read_file('input04.txt')

rs <- unlist(strsplit(r, "\n\n"))
rs <- gsub("\n"," ", rs)
rs <- strsplit(rs, " ")
rs <- lapply(rs, FUN=function(X) data.table(X))
rs <- lapply(rs, FUN=function(X) `colnames<-`(X, "raw"))
rs <- lapply(rs, FUN=function(X) 
  X[, c('raw', 'field', 'value') := c(list(NULL), tstrsplit(raw, ':', fixed=TRUE))])
rs

#rs is a list, each element is data for one document
# each element is a data.table with two columns: field and value
# Check each doc has all first 7 of fields:
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
#
# and optionally
# cid (Country ID)

need  <- data.table(
  field = c('byr',
            'ecl',
            'eyr',
            'hcl',
            'hgt',
            'iyr',
            'pid'))
setkey(need, field)

n_pass <- length(rs)
checks1 <- data.table(element = seq(1,n_pass), 
                     count = rep(0, n_pass), 
                     has_cid = rep(0, n_pass))

for (i in 1:n_pass){
    
    rsi <- rs[[i]]
    setkey(rsi,field)
    nr <- need[rsi, .N, by = .EACHI]                               
    checks1$count[i] = sum(nr$N)
    
    checks1$has_cid[i] = 'cid' %in% unique(rsi$field)

}
checks1[ ,valid :=  get("count") == 7]
checks1

sum(checks1$valid)


# Part 2

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
checks2 <- NULL
checks2 <- data.table(count = rep(0, n_pass), 
                      byr = rep("", n_pass),
                      iyr = rep("", n_pass),
                      eyr = rep("", n_pass),
                      hgt = rep("", n_pass),
                      hcl = rep("", n_pass),
                      ecl = rep("", n_pass),
                      pid = rep("", n_pass),
                     byr_check = rep(0, n_pass),
                     iyr_check = rep(0, n_pass),
                     eyr_check = rep(0, n_pass),
                     hgt_check = rep(0, n_pass),
                     hcl_check = rep(0, n_pass),
                     ecl_check = rep(0, n_pass),
                     pid_check = rep(0, n_pass))
checks2
for (i in 1:n_pass){
  
  rsi <- rs[[i]]
  setkey(rsi,field)
  nr <- need[rsi, .N, by = .EACHI]                               
  checks2$count[i] = sum(nr$N)==7
  
  checks2$byr_check[i] = 'byr' %in% unique(rsi$field)
  if ('byr' %in% rsi$field) {
  rsi_byr = rsi[field == 'byr', value]
  checks2$byr[i] = rsi_byr
  checks2$byr_check[i] = 
    rsi_byr %between% c(1920, 2002)
  }
  
  checks2$iyr_check[i] = 'iyr' %in% unique(rsi$field)
  if ('iyr' %in% rsi$field) {
    rsi_iyr = rsi[field == 'iyr', value]
    checks2$iyr[i] = rsi_iyr
    checks2$iyr_check[i] = 
      rsi_iyr %between% c(2010, 2020)
  }
  
  checks2$eyr_check[i] = 'eyr' %in% unique(rsi$field)
  if ('eyr' %in% rsi$field) {
    rsi_eyr = rsi[field == 'eyr', value]
    checks2$eyr[i] = rsi_eyr
    checks2$eyr_check[i] =  
      rsi_eyr %between% c(2020, 2030)
  }
  
  checks2$hgt_check[i] = 'hgt' %in% unique(rsi$field)
  if ('hgt' %in% rsi$field) {
    rsi_hgt = rsi[field == 'hgt', value]
    checks2$hgt[i] = rsi_hgt
    checks2$hgt_check[i] =  
      
      substr(rsi_hgt, nchar(rsi_hgt)-1, nchar(rsi_hgt)) == 'cm' &
      substr(rsi_hgt, 1, nchar(rsi_hgt)-2) %between% c(150, 193) |
      
      substr(rsi_hgt, nchar(rsi_hgt)-1, nchar(rsi_hgt)) == 'in' &
      substr(rsi_hgt, 1, nchar(rsi_hgt)-2) %between% c(59, 76)
  }
  
  checks2$hcl_check[i] = 'hcl' %in% unique(rsi$field)
  if ('hcl' %in% rsi$field) {
    rsi_hcl = rsi[field == 'hcl', value]
    checks2$hcl[i] = rsi_hcl
    checks2$hcl_check[i] =  
      grepl('^\\#[a-f0-9]{6}$', rsi_hcl)
  }
  
  checks2$ecl_check[i] = 'ecl' %in% unique(rsi$field)
  if ('ecl' %in% rsi$field) {
    rsi_ecl = rsi[field == 'ecl', value]
    checks2$ecl[i] = rsi_ecl
    checks2$ecl_check[i] =  
      rsi_ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
  }
  
  checks2$pid_check[i] = 'pid' %in% unique(rsi$field)
  if ('pid' %in% rsi$field) {
    rsi_pid = rsi[field == 'pid', value]
    checks2$pid[i] = rsi_pid
    checks2$pid_check[i] =  
      grepl('^[[:digit:]]{9}$', rsi_pid)
  }
}

valid2 <- apply(checks2[,9:15], 1, min)
checks2[, valid2 := valid2]
checks2
sum(valid2)
