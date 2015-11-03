####################
# function:     gen.cdrpile()
# purpose:    	Generates a sorted list of the elements in the CDR strategic pile.
#               Each S_n is placed before all combinations of +/- in binary order.
# parameters:	n:The number of elements in the set G^n 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r
source(sort.listss.r)
gen.cdrpile <- function(n){
    symset<-permn(n)
    ordered.symset<-sort.listss(symset) #get list of sym_n elements and order it
    
    for(k in ordered.symset){
        
        
    
    }
    
}