####################
# function:     gen.matlab.r()
# purpose:      Generate the matlab objects for indexing
# parameters:   
# Author:       Joshua Watson Nov 2015
# Dependancies: library(combinat)
# TODO:         Make this draw on a database rather than doing the calculations on the fly.
# install.packages('combinat') #run the first time only to get the package on your computer





filename <- paste("A", ".mat", sep="")
writeMat(filename, A=x)
