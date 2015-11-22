####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015, help from TheTime @stackoverflow
<<<<<<< HEAD
# Dependancies: sort.listss.r ; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
=======
# Dependancies: sort.listss.r ; gen.bincomb.r
>>>>>>> e03c2b94a81d5973a17ef8ac4d431307dd6b41b8

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

cdrtree <- function(root.value, shorthand=FALSE) {
    
    templist<- list()
    thispile<-gen.cdrpile(length(root.value))
    root <- Node$new('v0')  
    root$value <- root.value  
    if(shorthand==FALSE){
        root$name <- paste("ROOT",paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
    } else{
        root$name <-paste("R^",length(root.value),"_",cdrindex(root.value),collapse=",")
    }
    
    name.node <- nodeNamer()   # initialize the node counters to name the nodes
    name.node2 <- nodeNamer2()
    #recursive function that produces children and names them appropriately
    have.kids <- function(node) {
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            
            child.val <- cdrmove(node$value, pointer)  #make the cdr move on the first pointer
            child.index<-cdrindex(child.val)
            child <- Node$new(name.node())
            child$value <- child.val
            
            if(shorthand==TRUE){
                prefix<-name.node2()
                namevar<-paste("R_",child.index,sep="")
                child$name<-namevar
            }
            if(shorthand==FALSE){
                child$name <- paste(unlist(child$value),collapse=' ')  # Name it 
                namevar<-child$name
                prefix<-name.node2()
                
            }
            
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches duplicates. Names WIN+, WIN-, and DRAW outcomes
            endname<-paste(unlist(tail(thispile, n=1)[[1]]),collapse=' ')
            startname<-paste(unlist(thispile[[1]]),collapse=' ')
            
            
            
            if(child$name==endname){
                child$name <- paste(prefix,"-WIN ",namevar,sep='')  
            } else {
                if(child$name==startname){
                    child$name <- paste(prefix,"+WIN ",namevar,sep='')  
                } else {
                    #if all negative (!win) or all positive (!win) then it is terminal and could be a duplicate, rename it for igraph
                    if((sum(child$value < 0) == length(root.value)) || ((sum(child$value < 0 ) == 0 && !(child$name==endname) ) )){
                        child$name <- paste(prefix,"DRAW ",namevar,sep='')
                    } else {
<<<<<<< HEAD
                        #catch the other duplicate cases that aren't listed above
                        if((child$name %in% templist == TRUE) || (child$name == root$name)){
                            child$name <- paste(prefix,"DUP ",namevar,sep='')
                        } 
=======
                        #if all negative or all postitive then it is terminal and could be a duplicate, rename it
                            if((sum(child$value < 0) == length(root.value)) || (sum(child$value < 0 ) == 0 )){
                                child$name <- paste(name.node2(),"DRAW",child$name,sep='')
                            } else {
                                #catch the other duplicate cases that aren't listed above
                                if((child$name %in% templist == TRUE) || (child$name == root$name)){
                                    child$name <- paste(name.node2(),"DUP",child$name,sep='')
                                    #templist[[length(pointerlist)+1]] <-
                                } 
                            }
>>>>>>> e03c2b94a81d5973a17ef8ac4d431307dd6b41b8
                    }
                }
                
            }
            #make a list of names for the last duplicate catcher
            append(child$name,templist)->>templist
<<<<<<< HEAD
            
            if(shorthand==FALSE){
                child$name <- paste(" ",child$name,collapse=' ') #add a space for cosmetics
=======
            Recall(child)    # recurse with child
>>>>>>> e03c2b94a81d5973a17ef8ac4d431307dd6b41b8
            }
            
            Recall(child)    # recurse with child
        }
    }
    have.kids(root)
        return( root )
}


# WOA!
# count_isomorphisms(as.igraph(cdrtree(x[[877]])),as.igraph(cdrtree(x[[1877]])))
#to plot
#plot(as.igraph(a, directed = TRUE, direction = "climb"),layout=layout.reingold.tilford,edge.arrow.size=0.2,vertex.size=10,vertex.color="light blue",vertex.label.color="black")
#library(networkD3)
#tempnetwork <- ToDataFrameNetwork(cdrtreeoutput, "name")
#simpleNetwork(tempnetwork[-3], fontSize = 16)




#makes a forrest in text format
cdrtextforrest <- function(pile,...){ #allows passing cdrtree arguments such as shorthand=TRUE
    if(dir.exists("txt forrest")==FALSE){
        dir.create("txt forrest")
    } 
        
    wd <- getwd()
    setwd("txt forrest")
    dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
    setwd(paste(pile[[1]],collapse=' '))
    for (i in pile){
        
        filename<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".txt",sep="")
        
        write.table(cdrtree(i), filename,quote = FALSE,col.names = FALSE, row.names = FALSE)
    }
    setwd(wd)       
}


cdrimageforrest <- function(pile){ #allows one to describe shorthand or longhand format as a direct argument
    
    require(igraph)
    
    if(dir.exists("IMG forrest")==FALSE){
        dir.create("IMG forrest")
    } 
    
    wd <- getwd()
    setwd("IMG forrest")
    
    dir.create(paste(pile[[1]],collapse=' '))
    setwd(paste(pile[[1]],collapse=' '))
    for (i in pile){
      
        if(length(cdrpointers(i))>0){

            filenamevar<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".pdf",sep="")
            a <- cdrtree(i,shorthand=FALSE)
            b<-as.igraph(a)
            
            if(ecount(b)>0){
                V(b)$label.cex<-1
            }
            if(ecount(b)>3){
                V(b)$label.cex<-.9
            }
            if(ecount(b)>6){
                V(b)$label.cex<-.8
            }
            if(ecount(b)>12){
                V(b)$label.cex<-.7
            }
            if(ecount(b)>17){
                V(b)$label.cex<-.6
            }
            if(ecount(b)>28){
                V(b)$label.cex<-.5
            }
            
            pdf(filenamevar, height=11, width=8.5)
            
            plot(b,layout=layout.reingold.tilford,rescale=TRUE,vertex.shape='none',vertex.color='white')
            
            dev.off()

        } 
    }
    setwd(wd)       
}

# a<-"c:/users/joshua/documents/cds"

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

biosphere <- function(range){
   textbiome(range)
    imagebiome(range)

}

