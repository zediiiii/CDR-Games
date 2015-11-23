####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015, help from TheTime @stackoverflow
# Dependancies: sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" 
# Example:      cdrtree(gen.cdrpile(5)[[877]],shorthand=FALSE)
# NOTES:        Presently, shorthand=TRUE is broken; compensated by collapsing WIN states. Use shorthand=FALSE.

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

nodeNamer3 <- function() { #NEED clean this up
    k <- 0
    function(node) sprintf("%g", (k <<- k+1))
}

cdrtree <- function(root.value, shorthand=FALSE) {
    
    name.node <- nodeNamer()   # initialize the node counters to name the nodes
    name.node2 <- nodeNamer2()
    name.node3 <- nodeNamer3() # NEED clean this up
    
    templist<- list()
    pointerlist<- list()
    thispile<-gen.cdrpile(length(root.value))
    root <- Node$new('v0')  
    root$value <- root.value  

    
    #NEED Clean this up
#    if(shorthand==FALSE){
#        root$name <- paste("ROOT",paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
#    } else{
#        root$name <-paste("R^",length(root.value),"_",cdrindex(root.value),collapse=",")
#    }
    
    #edge naming probing
    if(shorthand==FALSE){
        root$name <- paste(name.node3(),"ROOT",paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
    } else{
        root$name <-paste(name.node3(),"R^",length(root.value),"_",cdrindex(root.value),collapse=",")
    }
    
    #recursive function that produces children and names them appropriately
    have.kids <- function(node) {
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            #append pointer to precisely ordered pointer list
            append(paste(pointer[1],pointer[2],sep=','),pointerlist)->>pointerlist

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
                child$name <- paste(prefix,"-WIN ",sep='')  #full name not needed here
            } else {
                if(child$name==startname){
                    child$name <- paste(prefix,"+WIN ",sep='')  #full name not needed here
                } else {
                    #if all negative (!win) or all positive (!win) then it is terminal and could be a duplicate, rename it for igraph
                    if((sum(child$value < 0) == length(root.value)) || ((sum(child$value < 0 ) == 0 && !(child$name==endname) ) )){
                        child$name <- paste(prefix,"DRAW ",namevar,sep='')
                    } else {

                        #catch the other duplicate cases that aren't listed above
                        if((child$name %in% templist == TRUE) || (child$name == root$name)){
                            child$name <- paste(prefix,"DUP ",namevar,sep='')
                        } 
                    }
                }
                
            }
            #make a list of names for the last duplicate catcher
            append(child$name,templist)->>templist
            
            if(shorthand==FALSE){
                
                
                
                
                
                #try to find the order for edge naming
                child$name <- paste(name.node3()," ",child$name,collapse=' ') #NEED clean this up
                #child$name <- paste(" ",child$name,collapse=' ') #add a space for cosmetics
            }
            
            Recall(child)    # recurse with child
        }
    }
    have.kids(root)
    
    #NEED clean this up
    #Explicit edge single label rename for the 1st edge - only for igraph data type
    #E(root)[1]$label<-'testedge'
    
    ###attempt at labeling the edges NEED clean this up
#    root<-as.igraph(root)
    
#    V(root)$label.cex<-.7
#    E(root)$label<-pointerlist #name all the edge labels from list, ordering?
    
#    E(root)$label.cex<-.7    
#    pointerlist
        return( root )
}

# This could be useful.
# count_isomorphisms(as.igraph(cdrtree(x[[877]])),as.igraph(cdrtree(x[[1877]])))

####################
# function:     cdrforrest()
# purpose:      Generates a set of CDR trees from a generated pile and exports as text to dir.out
# parameters:	pile: The list of gamestates. Generate a list using gen.cdrpile()
#               dir.out: The name of the directory where the forrest will be stored.
#               forrest.type: The type of forrest to be generated. Must be either 'text' or 'image'.
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" 
# Example:      cdrforrest(gen.cdrpile(4),forrest.type='text',dir.out='R_4 Text Trees')

cdrforrest <- function(pile,forrest.type='text', dir.out='cdrforrest',...){
    
    forrest.type <- match.arg(c('text','image'),c('text','image'))

    if(dir.exists(dir.out)==FALSE){
        dir.create(dir.out)
    } 
    
    setwd(dir.out)
    wd <- getwd()
    
    #cdr text forrest
    if(forrest.type=='text'){
        
        dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
        setwd(paste(pile[[1]],collapse=' '))
        for (i in pile){
            filename<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".txt",sep="")
            write.table(cdrtree(i), filename,quote = FALSE,col.names = FALSE, row.names = FALSE)
        }
    
    if(forrest.type=='image'){
        require(igraph)
        
        dir.create(paste(pile[[1]],collapse=' '))
        setwd(paste(pile[[1]],collapse=' '))
        for (i in pile){
            
            if(length(cdrpointers(i))>0){
                
                filenamevar<-paste(cdrindex(i),"    ",paste(i,collapse=' '),".pdf",sep="")
                a <- cdrtree(i,shorthand=FALSE)
                b<-as.igraph(a)
                
                #educated adjustment of label.cex to minimize overlaps in output.
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
                if(ecount(b)>40){
                    V(b)$label.cex<-.4
                }
                if(ecount(b)>50){
                    V(b)$label.cex<-.3
                }
                if(ecount(b)>70){
                    V(b)$label.cex<-.2
                }
                if(ecount(b)>90){
                    V(b)$label.cex<-.1
                }
                
                pdf(filenamevar, height=11, width=8.5)
                plot(b,layout=layout.reingold.tilford,rescale=TRUE,vertex.shape='none',vertex.color='white')
                dev.off()
            } 
        }
    }
        setwd(wd)       
    }
}

####################
# function:     cdrbiome()
# purpose:      Generates a set of CDR trees from a generated pile. Takes arguments from cdrforrest (listed).
# parameters:	range:a vector of the numbers to be included in tree generation.
#               dir.out: The name of the directory where the forrest will be stored.
#               forrest.type: The type of forrest to be generated. Must be either 'text' or 'image'..
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" 
# Example:      cdrbiome(3:4,forrest.type='text',dir.out='cdrtextforrest')

cdrbiome<- function(range,...){
    wd<-getwd() #to realign directories if there are errors in nested functions.
    for(i in range){
        cdrforrest(gen.cdrpile(i))
    }
    setwd(wd)
}


####################
# function:     cdrbiosphere()
# purpose:      Generates a set of text and image CDR trees for the given range.
# parameters:	range:a vector of the numbers to be included in tree generation.
#               dir.out: The name of the directory where the forrest will be stored (through cdrforrest).
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" 
# Example:      cdrbiome(3:4,,forrest.type='text',dir.out='cdrtextforrest')

cdrbiosphere <- function(range,...){
    
    cdrbiome(range,forrest.type="text")
    cdrbiome(range,forrest.type="image")

}

