####################
# function:     cdrmove()
# purpose:        
# parameters:	l:Starting number of elements inclusive - must be <=r  (natural number),
#               Example:countcdr(3,5,pretty=F) returns a named list with the total number of 
#                       states and moves possible for sets of 1 to 3,4,5 along with exectution time.
# Author:       Joshua Watson & Alyssa Seidman 2015
# Dependancies: combinat

playcdr <- function(game,pointer){