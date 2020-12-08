rm(list = ls())
setwd('/Users/danielpringle/Code/aoc2020/07')
library(tidyverse)
library(data.table)
library(data.tree)
library(igraph)

r <- read_file('input07test.txt')
#r <- read_file('input07test2.txt')

rs <- unlist(strsplit(r, "\n"))

d <- data.table(raw = gsub("\n"," ", rs))

d[, name := trimws(gsub("bags", "", tstrsplit(d$raw, 'contain', fixed=TRUE)[[1]]))]
d[, kids := trimws(gsub("bag", "", (gsub("\\.", "", gsub("bags", "", tstrsplit(d$raw, 'contain', fixed=TRUE)[[2]])))))]
d[, n_kids := 1+str_count(kids, ",")]

max_kids = max(d[,n_kids])
col_kids = paste0("kid", seq(1:max_kids))
d[, (col_kids) := tstrsplit(kids, ',', fixed=TRUE)]

d[, c('raw', 'kids','n_kids') := NULL]
id.vars = c("name")
measure.vars = setdiff(names(d), id.vars)

dm = melt(d, id.vars = id.vars,
             measure.vars = measure.vars)

dm[, variable := NULL]
dm[, value := trimws(value)]
dm <- dm[!is.na(value)]

setorderv(dm,c('name'))

dm[, child:= trimws(gsub('[0-9]', "", value))]
dm[, number := parse_integer(gsub('[a-z]', "", value))]
dm[, value := NULL]
dm[child == "no other", `:=` (child = NA, number = 0)]
#dm <- dm[!is.na(child),]
saveRDS(dm, 'test1.RDS')
dm
un <- unique(dm$name)
uc <- unique(dm$child)

# 1a. Tree from outside to inside

top = un[!un %in% uc]
tops <- data.table('name' = rep('top',length(top)), 
                   'child' = top,
                   'number' = 1)
d = rbind(tops, dm)
d[, name := gsub(" ", "", d$name)]
d[, child := gsub(" ", "", d$child)]

bags <- FromDataFrameNetwork(d)
paths <- bags$Get('path')
#pathsn <- bags$Get('number')

paths_sg <- paths[names(paths)=="shinygold"]
names(paths_sg) <- paste0("shinygold", seq(1:length(paths_sg)))
rm_nm = c('top', 'shinygold')
number_unique_bags_above_shiny_gold <- length(setdiff(unique(unlist(paths_sg)), rm_nm))
number_unique_bags_above_shiny_gold 

#----------------------------------------------------------------
# Reddit Solution with igraph methods: 
#----------------------------------------------------------------

create_graph <- function(relations) {
  g <- graph_from_data_frame(relations %>% drop_na(), directed=TRUE)
  E(g)$weight <- as.numeric(drop_na(relations)$number)
  return(g)
}

count_bags <- function(g) {
  paths <- all_simple_paths(g, from = "shiny gold", mode = "out")
  prod_edges <- function(path) {
    EP = rep(path, each=2)[-1]
    EP = EP[-length(EP)]
    E(g)$weight[get.edge.ids(g, EP)]
    prod(E(g)$weight[get.edge.ids(g, EP)])
  }
  
  sum(unlist(map(paths, prod_edges)))
}

gdm <- create_graph(dm)
plot(gdm)
# Count all the unique bags on the "in" path.
# Subtract 1 to remove "shiny gold" itself.
answer1 <- length(unique(names(unlist(all_simple_paths(gdm, "shiny gold", mode = "in"))))) - 1
answer1

answer2 <- count_bags(gdm)
answer2

#----------------------------------------------------------------




# # 1b. Tree from inside to outside : not used
# 
# un <- unique(dm$name)
# uc <- unique(dm$child)
# 
# bot = uc[!uc %in% un]
# bot
# bots <- data.table('name' = bot,
#                    'child' = rep('bot',length(bot)), 
#                    'number' = 1)
# 
# db = rbind(bots, dm)
# db[, name := gsub(" ", "", db$name)]
# db[, child := gsub(" ", "", db$child)]
# 
# bags2 <- FromDataFrameNetwork(db)
# print(bags2, limit = 100)
# paths2 <- bags2$Get('path')
# #pathsn <- bags$Get('number')
# paths2
# 
# paths2_sg <- paths2[names(paths2)=="shinygold"]
# names(paths2_sg) <- paste0("shinygold", seq(1:length(paths2_sg)))
# rm_nm = c('bot', 'shinygold')
# paths2_sg
# number_unique_bags_above_shiny_gold <- length(setdiff(unique(unlist(paths2_sg)), rm_nm))
# number_unique_bags_above_shiny_gold 
# 

# Part 2

# Count the number of bags contained, according to the rules, in your one shiny gold bag
# test data 2 has 126 bags:

# Consider again your shiny gold bag and the rules from the above example:
# 
# faded blue bags contain 0 other bags.
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
# So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
# 
# Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
# 
# Here's another example:
#   
# shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.

# In this example, a single shiny gold bag must contain 126 other bags.

print(bags, "level")

d2 <- d[!child == 'shinygold' & !name == 'top',]
top_sg <- data.table('name' = 'top', 
                   'child' = 'shinygold',
                   'number' = 1)
d2 = rbind(top_sg, d2)
d2

bags2 <- FromDataFrameNetwork(d2)

paths <- bags$Get('path')


bags$totalin <- function(self) sum(sapply(self$children, function(x) x$number))
print(bags, "totalin")

# 
dm <- readRDS('test1.RDS')

