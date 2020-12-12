rm(list = ls()); gc()
setwd('/Users/danielpringle/Code/aoc2020/12')
library(tidyverse)
library(data.table)

d <- as.data.frame(scan('input12test.txt', what = character()))
d <- as.data.frame(scan('input12.txt', what = character()))
setDT(d)
colnames(d) <- c("raw")
d[, act  := trimws(gsub('[0-9]', "", raw))]
d[, val  := parse_integer(gsub('[A-Z]', "", raw))]
head(d)

table(d[act %in% c('R','L'), act],
      d[act %in% c('R','L'), val])

table(d[,act])

# Converting L/R rotatins into changes in head(ing)

rot_on_head <- function(head_in, RL, rot){
  
  stopifnot(head_in %in% c('N', 'E', 'S', 'W'))
  stopifnot(rot %in% c(0,90,180,270))
  stopifnot(RL %in% c('R','L'))
  
  heading <- data.table(
    head = c('N', 'E', 'S', 'W'),
    quad = c(0,1,2,3))
  
  dir = 2*(RL == 'R')-1
  quad_rot <- dir*rot/90
  head_num_in  <- as.numeric(heading[head == head_in, quad])
  head_num_out <- (head_num_in + quad_rot) %% 4 
  head_out <- as.character(heading[quad == head_num_out, head])
  return(head_out)
  }

# rot_on_head(head_in = 'S',
#             rot = 270,
#             RL = 'L')

do_instruction <- function(boat, instruction){
  
  easting <- boat$easting
  northing <- boat$northing
  heading <- boat$heading
  
  act <- instruction$act
  val <- instruction$val
  
  if (act %in% c('N','E','W','S')) {
    easting = easting + val*(act=='E') - val*(act=='W')
    northing = northing + val*(act=='N') - val*(act=='S')
    heading = heading
  }
  
  if (act %in% c('R','L')) {
    easting = easting
    northing = northing
    heading <- rot_on_head(head_in = heading, RL = act, rot = val)
  }
  
  if (act %in% c('F')) {
    easting <- easting + val*(heading=='E') - val*(heading=='W')
    northing <- northing + val*(heading=='N') - val*(heading=='S') 
    heading <- heading
  }
  
  new_boat <- data.table(easting = easting, northing = northing, heading = heading)
  return(new_boat)
}
boats <- data.table(easting = 0, northing = 0, heading = 'E')
boat <- copy(boats[1,])

for (i in 1:nrow(d)) {

  instruction <- d[i,]
  new_boat <- do_instruction(boat, instruction)
  boats <- rbind(boats, new_boat)
  boat <- copy(new_boat)

}

manhattan <- abs(boats[.N,easting]) + abs(boats[.N,northing])
manhattan
