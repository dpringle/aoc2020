rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/03')
library(tidyverse)
library(data.table)

#m0 <- data.table(scan('input03test.txt', what = character()))
m0 <- data.table(scan('input03.txt', what = character()))

count_tree <- function(ls) {
# mx = m0
# dc = 1
# dr = 2

m0 = ls$m
dc = ls$dc
dr = ls$dr

mx = copy(m0)
nrow <- nrow(mx) 
nch <- nchar(mx[1,])
cols <- c(paste0("c", seq(1,nch)))

mx[, (cols) := tstrsplit(V1, split = "", fixed=TRUE)]
mx[, V1 := NULL]
m <- as.matrix(mx)

# View(m)
# matrix indexing is m[r,c]
# r = row
# c = col

start_r = 1
start_c = 1

# each move is right 3, down 1
#dc = 3
#dr = 1

moves = (nrow-1)/dr +1
slide = moves*dc
blocks_right = ceiling(slide/nch)

mm <- do.call("cbind", replicate(n = blocks_right, m, simplify = FALSE))

stopifnot(nrow(mm)==nrow)
stopifnot(ncol(mm)>=slide)

p <- data.table(r = seq(from = start_r, by = dr, length.out = moves),
                c = seq(from = start_c, by = dc, length.out = moves))
p
for (i in 1:moves){
  row = p[i,r]
  col = p[i,c]
  p[i,tree := mm[row,col]] 
  
}

trees = sum(p$tree == "#")
# Clean up data.tables
mx <- NULL
p  <- NULL
mm <- NULL
return(trees)
}

# Output the product of trees hit in these cases:
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

l11 = list(m = m0, dc = 1, dr =1)
l31 = list(m = m0, dc = 3, dr =1)
l51 = list(m = m0, dc = 5, dr =1)
l71 = list(m = m0, dc = 7, dr =1)
l12 = list(m = m0, dc = 1, dr =2)

l = list(l11,l31,l51,l71,l12)
trees <- prod(unlist(lapply(l, count_tree)))
trees
