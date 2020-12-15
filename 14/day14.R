rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/14')
library(tidyverse)
library(data.table)

#r <- read_file('input14test.txt')
r <- read_file('input14.txt')

prepare_data <- function(r){
  
  rs <- unlist(strsplit(r, "mas"))
  rs <- rs[2:length(rs)]
  rs <- gsub("k", "mask", rs)
  rs <- lapply(rs, FUN=function(X) unlist(strsplit(X, "\n")))
  rs <- lapply(rs, FUN=function(X) as.data.table(X))
  rs <- lapply(rs, FUN=function(X) `colnames<-`(X, "raw"))
  rs <- lapply(rs, FUN=function(X) 
    X[, c('raw','field', 'value') := c(list(NULL), tstrsplit(raw, '=', fixed=TRUE))])
  rs <- lapply(rs, FUN=function(X) 
    X[, c('mem_loc') := parse_number(get('field'))])
  rs <- lapply(rs, FUN=function(X) 
    X[, field := trimws(get('field'))])
  rs <- lapply(rs, FUN=function(X) 
    X[, value := trimws(get('value'))])
  for (i in 1:length(rs)){
    rs[[i]][,'ID'] = i
  }
  return(rs)
}

dec_bit36 <- function(dec) {
  bit <- paste(rev(as.integer(intToBits(dec))), collapse="") #32 bit
  bit36 <- paste0("0000",bit)
  return(bit36)
  }

bit36_dec <- function(bit36) {
  bit36_sp <- strsplit(bit36, "")[[1]]
  dec <- sum(2^(which(rev(bit36_sp)=='1')-1))
  return(dec)
  }

apply_mask <- function(mask, bit36, n = 36) {
  
  stopifnot(nchar(mask) == n  & nchar(bit36) == n)

  mask_sp = strsplit( mask, "")[[1]]
  mask_sp
  bit36_sp = strsplit(bit36, "")[[1]]
  bit36_sp
  mb_sp <- bit36_sp
  
  for (i in 1:n) {
    if(!mask_sp[i]=='X') mb_sp[i] <- mask_sp[i]
  }
  mb <- paste(as.character(mb_sp), collapse = "")
  return(mb)
}


# Apply to each bit mask ------

rs <- prepare_data(r)

memory <- data.table(loc = 1:100000, value = 0)

for (i in 1:length(rs)){
  # extract list element : mask and instructions
  
  rs_tmp <- copy(rs[[i]])
  mask <- rs_tmp[field == 'mask', value]
  bits <- rs_tmp[!field == 'mask', c('mem_loc', 'value')]
  
  # print(paste0("i = ", i, " and mask is: ", mask))

  # loop over instructions, 
  # - convert dec to bit36
  # - apply mask to bit36
  # - convert back to dec
  # - assign to mem location
  
  for (j in 1:nrow(bits)) {

    loc <- as.numeric(bits[j,'mem_loc'])
    dec <- as.numeric(bits[j,'value'])
    bit36 <- dec_bit36(dec)
    bit36_m <- apply_mask(mask, bit36, n = 36) 
    dec_m <- bit36_dec(bit36_m)
    memory[loc, 'value' := dec_m]

    # Debugging output
    # print(paste0("loc = ", loc))
    # print(paste0("dec = ", dec))
    # print(paste0("bit36 = ", bit36))
    # print(paste0("mask = ", mask))
    # print(paste0("masked = ", bit36_m))
    # print(paste0("dec_m = ", dec_m))
    }
}
  
(answer = as.character(sum(memory$value)))
