####################
# function:     gen.cdrpile()
# purpose:    	Generates a sorted list of the elements in the CDR strategic pile.
#               Each S_n is placed before all combinations of +/- in binary order.
# parameters:	n:The number of elements in the set G^n 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; combinat
#install.packages('combinat') #run the first time only to get the package on your computer
library(combinat) 
source("gen.bincomb.r")
source("sort.listss.r")

gen.cdrpile <- function(n){
    
    #instantiate variables
    alt.list <-list()   
    
    symset<-permn(n)
    ordered.symset<-sort.listss(symset) #get list of sym_n elements and order it
    
    #Generate the binary sorted list mask
    gen.bincomb(n)->binlist
    bin.length<-length(binlist)
    
    #Loop over the S_n set
    for(k in ordered.symset){
            #Then loop over the binlist
            for(i in 1:bin.length){
                temp<-binlist[[i]]*k
                alt.list[[length(alt.list)+1]] <- temp
            }
        
    
    }
    alt.list
}