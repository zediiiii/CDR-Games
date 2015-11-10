####################
# function:     cdrindex()
# purpose:      Get the index number of a particular gamestate in a CDR game
# parameters:   n:Number of binary elements per list entry
# Author:       Joshua Watson Nov 2015
# Dependancies: library(combinat)
# TODO:         Make this draw on a database rather than doing the calculations on the fly.
# install.packages('combinat') #run the first time only to get the package on your computer
library(combinat) 


cdrindex <- function(gamestate){
    n<-length(gamestate)
    list<-gen.cdrpile(n)
    which(sapply(list, identical, gamestate ))
}