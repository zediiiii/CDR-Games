####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015, help from TheTime @stackoverflow
# Dependancies: sort.listss.r ; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile

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
    thispile<-gen.cdrpile(length(root.value))
    root <- Node$new('v0')  
    root$value <- root.value  
    root$name <- paste("ROOT",paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
    
    name.node <- nodeNamer()   # initialize the node counters to name the nodes
    name.node2 <- nodeNamer2()
    
    #recursive function that produces children and names them appropriately
    have.kids <- function(node) {
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            
            child.val <- cdrmove(node$value, pointer)  #make the cdr move on the first pointer
            
            child <- Node$new(name.node())
            child$value <- child.val

            child$name <- paste(unlist(child$value),collapse=' ')  # Name it 
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches duplicates. Names WIN+, WIN-, and DRAW outcomes
            endname<-paste(unlist(tail(thispile, n=1)[[1]]),collapse=' ')
            startname<-paste(unlist(thispile[[1]]),collapse=' ')
            
            if(child$name==endname){
                child$name <- paste(name.node2(),"-WIN ",child$name,sep='')  
            } else {
                    if(child$name==startname){
                        child$name <- paste(name.node2(),"+WIN ",child$name,sep='')  
                    } else {
                        #if all negative (!win) or all positive (!win) then it is terminal and could be a duplicate, rename it for igraph
                            if((sum(child$value < 0) == length(root.value)) || ((sum(child$value < 0 ) == 0 && !(child$name==endname) ) )){
                                child$name <- paste(name.node2(),"DRAW ",child$name,sep='')
                            } else {
                                #catch the other duplicate cases that aren't listed above
                                if((child$name %in% templist == TRUE) || (child$name == root$name)){
                                    child$name <- paste(name.node2(),"DUP ",child$name,sep='')
                                    #templist[[length(pointerlist)+1]] <-
                                } 
                            }
                    }
                
            }
            #make a list of names for the last duplicate catcher
            append(child$name,templist)->>templist
            child$name <- paste(" ",child$name,collapse=' ') #add a space for cosm
            
            Recall(child)    # recurse with child
            }
        }
    have.kids(root)
    return( root )
}

#to plot
#plot(as.igraph(a, directed = TRUE, direction = "climb"),layout=layout.reingold.tilford,edge.arrow.size=0.2,vertex.size=10,vertex.color="light blue",vertex.label.color="black")
#library(networkD3)
#tempnetwork <- ToDataFrameNetwork(cdrtreeoutput, "name")
#simpleNetwork(tempnetwork[-3], fontSize = 16)




#makes a forrest in text format
cdrtextforrest <- function(pile){
    dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
    wd <- getwd()
    setwd(paste(pile[[1]],collapse=' '))
    for (i in pile){
        
        filename<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".txt",sep="")
        
            write.table(cdrtree(i), filename,quote = FALSE,col.names = FALSE, row.names = FALSE)
    }
    setwd(wd)       
}
      
#Doesn't work yet somethin to do with line 119
treegraph<-function(tree){
    
    plot(as.igraph(tree, directed = TRUE, direction = "climb"),layout=layout.reingold.tilford,edge.arrow.size=0.2,vertex.size=10,vertex.color="light blue",vertex.label.color="black")
}

library(igraph)
cdrimageforrest <- function(pile){
    
    dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
    wd <- getwd()
    setwd(paste(pile[[1]],collapse=' '))
    for (i in pile){
        
        filenamevar<-paste(cdrindex(i),"  ",paste(i,collapse=' '),".png",sep="")
        png(filename=filenamevar)
        treegraph(cdrtree(i))
        dev.off()
    }
    setwd(wd)       
}



textbiome<- function(range){
    wd<-getwd()
    for(i in range){
        cdrtextforrest(gen.cdrpile(i))
    }
    setwd(wd)
}

imagebiome<- function(range){
    wd<-getwd()
    for(i in range){
        cdrimageforrest(gen.cdrpile(i))
    }
    setwd(wd)
}

    