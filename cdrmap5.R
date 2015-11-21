####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r
# TODO:         Rename identical terminal nodes that aren't the first or last value of the pile

require(combinat)
require(data.tree)

#Two helper functions for keeping names distinct.
nodeNamer <- function() {
    i <- 0
    function(node) sprintf("v%g", (i <<- i+1))
}

nodeNamer2 <- function() {
  j <- 0
  function(node) sprintf("v%g", (j <<- j+1))
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
            
            child$name <- paste(name.node2(),child$name,sep=' ') #to make the names unique for as.igraph
            #this is a hack until logic works for finding all duplicates
            #duplicates are always terminal nodes, and most duplicates are ordered and accounted for
            #possibly create a function to find list of all states for which cdrmove produces null
            #which would find the list of possible terminal end duplicates
            
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches most duplicates, but not all yet, so hacked above
            #endname<-paste(unlist(tail(gen.cdrpile(length(root.value)), n=1)[[1]]),collapse=' ')
            #startname<-paste(unlist(root$value),collapse=' ')
            #
            #if(child$name==endname){
            #    child$name <- paste(name.node2(),"END2",child$name,sep='')  
            #} else {
            #        if(child$name==startname){
            #            child$name <- paste(name.node2(),"END1",child$name,sep='')  
            #        }
            #    
            #    append(child$name,templist)->templist
            }
            
            Recall(child)                              # recurse with child
        }
    #}
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
    