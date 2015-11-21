####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r

require(combinat)
require(data.tree)

#Two helper functions for keeping names distinct.
nodeNamer <- function() {
    i <- 0
    function(node) sprintf("v%g", (i <<- i+1))
}

nodeNamer2 <- function() {
  j <- 0
  function(node) sprintf("%g", (j <<- j+1))
}

cdrtree <- function(root.value, make.igraph=FALSE) {
    
    templist<- list()
    
    root <- Node$new('v0')  
    root$value <- root.value  
    root$name <- paste(unlist(root$value),collapse=' ') #name this the same as the value collapsed in type char
    
    name.node <- nodeNamer()   # initialize the node counters to name the nodes
    name.node2 <- nodeNamer2()
    
    #recursive function that produces chidlren and names them appropriately
    have.kids <- function(node) {
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            
            child.val <- cdrmove(node$value, pointer)  #make the cdr move on the first pointer
            
            child <- Node$new(name.node())
            child$value <- child.val
            #child$name <- paste(" ",unlist(child$value),collapse=' ') # Name it For text
            child$name <- paste(unlist(child$value),collapse=' ')  # Name it For Graphics
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches duplicates. Names WIN+, WIN-, and DRAW outcomes
            endname<-paste(unlist(tail(gen.cdrpile(length(root.value)), n=1)[[1]]),collapse=' ')
            startname<-paste(unlist(root$value),collapse=' ')
            
            if(child$name==endname){
                child$name <- paste(name.node2(),"WIN-",child$name,sep='')  
            } else {
                    if(child$name==startname){
                        child$name <- paste(name.node2(),"WIN+",child$name,sep='')  
                    } else {
                            if((sum(child$value < 0) == length(root.value)) || (sum(child$value < 0 ) == 0 )){
                                child$name <- paste(name.node2(),"DRAW",child$name,sep='')
                            }
                    }
                
                append(child$name,templist)->templist
            } 
                #if all negative or all postitive then it is terminal and could be a duplicate, rename it
                
            Recall(child)    # recurse with child
            }
            
            
        }
    
    have.kids(root)
    return( root )
    
}

treegraph<-function(tree){

        plot(as.igraph(tree, directed = TRUE, direction = "climb"),layout=layout.reingold.tilford)
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
    