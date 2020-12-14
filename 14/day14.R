rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/14')
library(tidyverse)
library(data.table)

r <- read_file('input14test.txt')
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
  
dec_bit36 <- function(dec) {
  dec = 73
  bit <- paste(rev(as.integer(intToBits(dec))), collapse="")
  bit36 <- paste0("0000",bit)
  bit36
  }


powers.of.two <- 2^(0:(n - 1))
toID <- function(vec) { as.integer(vec %*% powers.of.two) }

fff<-function(x) sum(2^(which(rev(x))-1))
x = mask
m <- gsub('X', '0',mask)

apply_mask <- function(mask, bit36, n = 36) {
  stopifnot(nchar(mask) == n  & nchar(bit36) == n)
  mask_sp = strsplit(gsub("X","0", mask), "")
  bit36_sp = strsplit(bit36, "")
  out <- mapply(function(x,y) paste(as.numeric(as.numeric(x) | as.numeric(y)), collapse = ""), mask_sp, bit36_sp)
  
  
  return(out)
}

# Apply to each bit mask
memory <- data.table(loc = 1:36, value = "")


for (i in 1:length(rs)){
 
  rs_tmp <- copy(rs[[i]])
  mask <- rs_tmp[field == 'mask', value]
  bits <- rs_tmp[!field == 'mask', c('mem_loc', 'value')]
  print(paste0(" i = ", i, " and mask is: ", mask))
  
  for (i in 1:length(bits)) {
    
    mem_loc
    
    }

}
  
  
dec = 101
bin = 
mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

# rev_mask <- paste(rev(strsplit(mask, "")[[1]]), collapse = "")
# rev_mask <- substr(rev_mask, 1, length())
# rev_bit <- paste(rev(strsplit(as.character(bit), "")[[1]]), collapse = "") 
