# Modified from  https://www.reddit.com/r/adventofcode/comments/k8a31f/2020_day_07_solutions/gf0a32d?utm_source=share&utm_medium=web2x&context=3

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(igraph)

# Read data ---------------------------------------------------------------

#d <- readLines("input07.txt")
d <- readLines("input07test.txt")


# Functions ---------------------------------------------------------------

parse_rules <- function(d) {
  as_tibble(d) %>%
    separate(value, into = c("bag", "contains"), sep = " contain ") %>%
    mutate(bag = str_replace(bag, "\\s(bags)$", ""))
}

parse_relations <- function(rules) {
  relations <- tibble(from = character(),
                      to = character(),
                      num = character())
  
  for (i in seq(nrow(rules))) {
    row <- rules[i,]
    res <- as_tibble(str_extract(strsplit(row$contains, ", ")[[1]], "(?<=[0-9]\\s)(.*)(?=\\sbag)")) %>%
      mutate(from = row$bag) %>%
      rename("to" = value) %>%
      select(from, to)
    
    if (!NA %in% res$to) {
      res$num <- str_extract_all(row$contains, "[0-9]")[[1]]
    } else {
      res$num <- NA
    }
    relations <- bind_rows(relations, res)
  }
  
  return(relations)
}

create_graph <- function(relations) {
  g <- graph_from_data_frame(relations %>% drop_na(), directed=TRUE)
  E(g)$weight <- as.numeric(drop_na(relations)$num)
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

# Question 1 --------------------------------------------------------------
rules <- parse_rules(d)
relations <- parse_relations(rules)
relations
g <- create_graph(relations)
plot(g)
# Count all the unique bags on the "in" path.
# Subtract 1 to remove "shiny gold" itself.
answer1 <- length(unique(names(unlist(all_simple_paths(g, "shiny gold", mode = "in"))))) - 1
answer1

# Question 2 --------------------------------------------------------------

answer2 <- count_bags(g)
answer2
