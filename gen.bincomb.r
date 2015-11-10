####################
# function:     gen.bincomb()
# purpose:      Generates a list sorted in binary order of n binary elements per list entry.
#               Since it sorts by value magnitude, you may or may not need to use the rev argument to reverse the order
# parameters:	n:Number of binary elements per list entry
#               bin:A two element vector comtaining numeric binary values. Default is c(-1,1).
#               rev:reverse the order if true. Needed depending on the numbers chosen.
# Author:       Joshua Watson Nov 2015
# Dependancies: library(combinat)
# install.packages('combinat') #run the first time only to get the package on your computer

library(combinat)

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