## Name nodes uniquely, dont be assigning to the .Globalenv like
## you are in `assn`, which wont work becuse `i` isn't being incremented.
## You could invcrement `i` in the global, but, instead,
## I would encapsulate `i` in the function's parent.frame, avoiding possible conflicts

library(data.tree)
library(igraph)

nodeNamer <- function() {
    i <- 0
    ## Note: `i` is incremented outside of the scope of this function using `<<-`
    function(node) sprintf("v%g", (i <<- i+1))
}

cdrtree <- function(root.value) {
    
    root <- Node$new('v0')  # make root node
    #root  <- Node$new(paste(root.value,sep=" ")) #causes graphing to break
    #root$name <- paste(root$value,collapse=" ") #doesn't assign name in function (it does outside function)
    
    #root$label <-root$value
    root$value <- root.value  # There seems to be a separation of value from name
    root$name <- paste(unlist(root$value),collapse=' ')
    name_node <- nodeNamer()   # initialize the node counter to name the nodes
    
    ## Define your recursive helper function
    ## Note: you could do without this and have `cdrtree` have an additional
    ## parameter, say tree=NULL.  But, I think the separation is nice.
    
    have.kids <- function(node) {
        ## this function (`cdrpointers`) needs work, it should return a 0 length list, not print
        ## something and then error if there are no values
        ## (or throw and error with the message if that is what you want)
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            
            child_val <- cdrmove(node$value, pointer)  # does this always work?
            
            child <- Node$new(name_node())
            #child <- Node$new(as.character(paste(child_val,collapse=" ")))            # give the node a name
            
            child$value <- child_val
            child$name <- paste(unlist(child$value),collapse=' ')
            #child$name <- paste(child_val,collapse=" ")  #name the node
            #child$name <- child$value
            child <- node$AddChildNode(child)
            Recall(child)                              # recurse with child
        }
    }
    have.kids(root)
    return( root )
}



cdrforrest <- function(pile){
    dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
    wd <- getwd()
    setwd(paste(pile[[1]],collapse=' '))
    for (i in pile){
        
        filename<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".txt",sep="")
        
            write.table(cdrtree(i), filename,quote = FALSE,col.names = FALSE, row.names = FALSE)
    }
    setwd(wd)       
}
      
biome<- function(range){
    wd<-getwd()
    for(i in range){
        cdrforrest(gen.cdrpile(i))
    }
    setwd(wd)
}
    