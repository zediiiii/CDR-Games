
# All script dependancies are declared per script, but also inserted here for convenience
#install.packages('combinat'); #run the first time only to get the package on your computer

library(combinat) 
source("gen.bincomb.r")
source("sort.listss.r")

####################
# function:     gen.cdspile()
# purpose:		Generates a sorted list of the elements in the CDS strategic pile.
# parameters:	n: number of elements
# Author:       Joshua Watson Oct 2015
# Dependancies: combinat


library(combinat) 

gen.cdspile <- function(n){
    permn(n)->list.of.lists
    list.of.lists[order(sapply(list.of.lists,'[[',1))]

}


####################
# function:     gen.cdrpile()
# purpose:        Generates a sorted list of the elements in the CDR strategic pile.
#               Each S_n is placed before all combinations of +/- in binary order.
# parameters:	n:The number     of elements in the set G^n 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; combinat
#install.packages('combinat') #run the first time only to get the package on your computer

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


####################
# function:     gen.bincomb()
# purpose:      Generates a list sorted in binary order of n binary elements per list entry.
#               Since it sorts by value magnitude, you may or may not need to use the rev argument to reverse the order
# parameters:    n:Number of binary elements per list entry
#               bin:A two element vector comtaining numeric binary values. Default is c(-1,1).
#               rev:reverse the order if true. Needed depending on the numbers chosen.
# Author:       Joshua Watson Nov 2015
# Dependancies: library(combinat)


gen.bincomb <- function(n,bin=c(1,-1),rev=TRUE){
    lst <- lapply(numeric(n), function(x) bin)
    mat <- as.matrix(expand.grid(lst))
    
    #Generate the code to sort in binary order.
    
    ix<-paste("binarray<-order(",collapse="")
    for(k in 1:ncol(mat)){
        ix<-paste(ix,"mat[,",k,"]",collapse="")
        if(k<ncol(mat)){
            ix<-paste(ix,",",collapse="")
        }
        if(k==ncol(mat)){
            ix<-paste(ix,")",collapse="")  
        }
        
    }
    
    #parse the string to sort and run it
    eval(parse( text=ix ))
    
    #get the nicely sorted matrix
    out<-mat[binarray,]          
    
    #Convert to list of lists
    out<-tapply(out,rep(1:nrow(out),ncol(out)),function(i)i)
    
    #reverse the order to make it in binary order if needed
    if(rev==TRUE){
        rev(out)
    }
    else{
        out
    }
}

####################
# function:     cdrindex()
# purpose:      Get the index number of a particular gamestate in a CDR game
# parameters:   n:Number of binary elements per list entry
# Author:       Joshua Watson Nov 2015
# Dependancies: library(combinat)
# TODO:         Make this draw on a database rather than doing the calculations on the fly.


cdrindex <- function(gamestate){
    n<-length(gamestate)
    list<-gen.cdrpile(n)
    which(sapply(list, identical, gamestate ))
}
