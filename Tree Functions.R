####################
# Helper functions for cdrtree

#Two helper functions iterate through "vn" where n is a sequential number. Used for keeping names distinct.
#which is required for plotting igraph objects.
nodeNamer <- function() {
    i <- 0
    function(node) sprintf("v%g", (i <<- i+1))
}
nodeNamer2 <- function() {
    j <- 0
    function(node) sprintf("%g", (j <<- j+1))
}

# Edge labeling helper function takes a two element character string and splits it 
# to vector as integer format

split_edge<-function(edge, n){
    
    left<-edge[1]
    
    right<-edge[2]
    right<-strsplit(right," ")[[1]]
    right<-right[(length(right)-n+1):length(right)]
    right<-as.integer(unlist(strsplit(right," ")))
    
    left<-strsplit(left," ")[[1]]
    left<-left[(length(left)-n+1):length(left)]
    left<-as.integer(unlist(strsplit(left," ")))
    
    return(list(left=left, right=right))
}

####################
# function:     cdrtree()
# purpose:      Generates a CDR tree with uniquely named nodes (uniqueness is required for igraph export)
# parameters:	root.value: the value of the seed to generate the tree. Values of length>6 are not recommended.
# Author:       Joshua Watson Nov 2015, help from TheTime @stackoverflow
# Dependancies: sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile; permn
#               All dependancies can be sourced from 'Generating Scripts.r" 
# Example:      cdrtree(gen.cdrpile(5)[[877]],make.igraph=FALSE)

require(combinat)
require(data.tree)
require(igraph)

cdrtree <- function(root.value,make.igraph=TRUE,...) {
    
    name.node <- nodeNamer()   # initialize the node counters to name the nodes
    name.node2 <- nodeNamer2()
    
    templist<- list()
    pointerlist<- list()
    thispile<-gen.cdrpile(length(root.value))
    root <- Node$new('v0')  
    root$value <- root.value  
    
    root$name <- paste("ROOT",paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
    
    #recursive function that produces children and names them appropriately
    have.kids <- function(node) {
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        for (pointer in pointers) {
            #append pointer to precisely ordered pointer list
            append(paste(pointer[1],pointer[2],sep=','),pointerlist)->>pointerlist
            
            child.val <- cdrmove(node$value, pointer)  #make the cdr move on the first pointer
            
            #This isn't currently necessary and adds tons of time for large piles
            #child.index<-cdrindex(child.val)
            
            child <- Node$new(name.node()) #give the new node a unique name
            child$value <- child.val
            child$name <- paste(unlist(child$value),collapse=' ')  # Name it correctly 
            namevar<-child$name
            prefix<-name.node2() #designate a unique prefix in case of a duplicate (required for igraph)
            
            
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches duplicates. Names WIN+, WIN-, and DRAW outcomes
            endname<-paste(unlist(tail(thispile, n=1)[[1]]),collapse=' ') #this is the name of the last element in the pile
            startname<-paste(unlist(thispile[[1]]),collapse=' ') # this is the name of the first element in the pile
            
            if(child$name==endname){
                child$name <- paste(prefix,"-WIN ",child$name,sep='')  #full name is only needed for naming edges with pointers
            } else {
                if(child$name==startname){
                    child$name <- paste(prefix,"+WIN ",child$name,sep='')  #full name is only needed for naming edges with pointers
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
            #make a list of names for comparison in the last duplicate catcher
            append(child$name,templist)->>templist
            child$name <- paste(" ",child$name,collapse=' ') #add a space for style
            Recall(child)    # recurse with child
        }
    }
    have.kids(root)
    
    if(make.igraph==TRUE){
        #Edge labeling handling only needed for making igraphs 
        root<-as.igraph(root)
        for(i in seq(length(E(root)))){
            
            edgeslisted<-split_edge(get.edgelist(root)[i,],length(root.value))
            labelgot<-what.pointer(edgeslisted$left,edgeslisted$right)
            
            E(root)[i]$label<-labelgot
        }
        E(root)$label.color<-'blue'
        V(root)$label.color<-'black'
        root
    } else{
        return( root )
    }
}

# This could be useful.
# count_isomorphisms(as.igraph(cdrtree(x[[877]])),as.igraph(cdrtree(x[[1877]])))

####################
# function:     cdrfreezecount()
# purpose:      Counts the number of gamestates that have no moves in a given R^n
# parameters:	n: which n in R^n to consider 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
# Example:      

cdrfreezecount <- function(n){
    count<-0
    pile<-gen.cdrpile(n)
    for(i in pile){
        thepointers<-cdrpointers(i)
        if(length(thepointers)<1){
            count<-count+1    
        }
    }
    count
}

####################
# cdrfreezecountloop()
# purpose : get freeze counts for multiple n, defined from a:b

cdrfreezecountloop<- function(a,b){
    freezelist<-list()
    for(i in a:b){
        freezelist[[length(freezelist)+1]]<-cdrfreezecount(i)
    }
    names(freezelist)<-paste("R^",a:b,sep='')
    freezelist
}




####################
# function:     cdrforrest()
# purpose:      Generates a set of CDR trees from a generated pile and exports as text to dir.out
# parameters:	pile: The list of gamestates. Generate a list using gen.cdrpile()
#               dir.out: The name of the directory where the forrest will be stored.
#               forrest.type: The type of forrest to be generated. Must be either 'text' or 'image'.
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
# Example:      cdrforrest(gen.cdrpile(4),forrest.type='text',dir.out='R_4 Text Trees')

cdrforrest <- function(pile,forrest.type='image', dir.out='cdrforrest',...){
    
    require(igraph)
    require(combinat)
    progress.counter<-0
    # inoperative argument matching that I don't fully understand yet
    # forrest.type <- match.arg(1:100,c('text','image'),*)
    
    wd <- getwd()
    as.numeric(length(pile))->pile.length
    #cdr text forrest
    if(forrest.type=='text'){
        if(dir.exists(dir.out)==FALSE){
            dir.create(dir.out)
        } 
        setwd(dir.out)
        
        dir.create(paste(pile[[1]],collapse=' '),showWarnings = FALSE)
        setwd(paste(pile[[1]],collapse=' '))
        for (i in pile){
            progress.counter<-progress.counter+1
            
            thisindex<-which(sapply(pile, identical, i ))
            filename<-paste(thisindex,"    ",paste(i,collapse=' '),".txt",sep="")
            write.table(cdrtree(i,make.igraph=FALSE), filename,quote = FALSE,col.names = FALSE, row.names = FALSE)
            outline<-paste(progress.counter, " out of ",pile.length," complete.",sep='')
            print(outline)
            
        }
    } else{
        
        
        
        if(forrest.type=='image'){
            if(dir.exists(dir.out)==FALSE){
                dir.create(dir.out)
            } 
            setwd(dir.out)
            
            dir.create(paste(pile[[1]],collapse=' '))
            setwd(paste(pile[[1]],collapse=' '))
            for (i in pile){
                
                if(length(cdrpointers(i))>0){
                    thisindex<-which(sapply(pile, identical, i ))
                    filenamevar<-paste(thisindex,"    ",paste(i,collapse=' '),".pdf",sep="")
                    b <- cdrtree(i)
                    progress.counter<-progress.counter+1
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
                        E(b)$label.cex<-.7
                    }
                    if(ecount(b)>17){
                        V(b)$label.cex<-.6
                        E(b)$label.cex<-.7
                    }
                    if(ecount(b)>23){
                        V(b)$label.cex<-.5
                        E(b)$label.cex<-.6
                    }
                    if(ecount(b)>32){
                        V(b)$label.cex<-.4
                        E(b)$label.cex<-.45
                    }
                    if(ecount(b)>45){
                        V(b)$label.cex<-.3
                        E(b)$label.cex<-.3
                    }
                    if(ecount(b)>62){
                        V(b)$label.cex<-.2
                        E(b)$label.cex<-.2
                    }
                    if(ecount(b)>78){
                        V(b)$label.cex<-.1
                        E(b)$label.cex<-.1
                    }
                    if(ecount(b)>96){
                        V(b)$label.cex<-.05
                        E(b)$label.cex<-.07
                    }
                    if(ecount(b)>140){
                        V(b)$label.cex<-.025
                        E(b)$label.cex<-.035
                    }
                    if(ecount(b)>168){
                        V(b)$label.cex<-.016
                        E(b)$label.cex<-.022
                    }
                    
                    pdf(filenamevar, height=11, width=8.5)
                    plot(b,layout=layout.reingold.tilford,rescale=TRUE,vertex.shape='none',vertex.color='white',main=paste("R^ ",length(i),"_",thisindex," has ",ecount(b)," children"))
                    dev.off()
                }
                outline<-paste(progress.counter, " out of ",pile.length," complete.",sep='')
                print(outline)
            }
        }
    }
    setwd(wd)
}

####################
# function:     cdrbiome()
# purpose:      Generates a set of CDR trees from a generated pile. Takes arguments from cdrforrest (listed).
# parameters:	range:a vector of the numbers to be included in tree generation.
#               dir.out: The name of the directory where the forrest will be stored.
#               forrest.type: The type of forrest to be generated. Must be either 'text' or 'image'..
# Author:       Joshua Watson Nov 2015
# Dependancies: igraph; sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" except igraph (CRAN)
# example:      cdrbiome(3:4,forrest.type='text',dir.out='cdrtextforrest')
# example2:     cdrbiome(3:6,forrest.type='image',dir.out='cdrtextforrest')

cdrbiome<- function(range,...){
    require(igraph)
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
# Dependancies: igraph; sort.listss.r; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
#               All dependancies can be sourced from 'Generating Scripts.r" except igraph (CRAN)
# Example:      cdrbiome(3:4,,forrest.type='text',dir.out='cdrtextforrest')
# Notes:        Currently only generates text output for some reason.

cdrbiosphere <- function(range,...){
    require(igraph)
    cdrbiome(range,forrest.type="text")
    cdrbiome(range,forrest.type="image")
}

####################
# function:     cdrwincount()
# purpose:      Counts how many distinct gamestates have at least one win in their tree
# parameters:	pile: The list of gamestates. Generate a list using gen.cdrpile()
# Author:       Joshua Watson Dec 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; cdrpointers; cdrmove; cdrindex; gen.cdrpile
# Example:      cdrwincount(gen.cdrpile(5))

require(combinat)

cdrwincount <- function(pile,...){
    
    win.counter<<-0
    countswitch<<-0
    as.numeric(length(pile[[1]]))->pile.length
    #cdr text forrest
    
    ######begin cdr tree maker
    
    #recursive function that produces children and names them appropriately
    have.kids <- function(node) {
        
        pointers <- tryCatch({cdrpointers(node$value)}, error=function(e) return( list() ))
        if (!length(pointers)) return()
        pointersleft<-length(pointers)
        # for(pointer in pointers){
        # countswitch<-0
        
        # while(pointersleft>0 && countswitch==0){
        while(pointersleft>0){
            # countswitch<-0
            child.val <- cdrmove(node$value, pointers[[pointersleft]])  #make the cdr move on the first pointer
            #child.val <- cdrmove(node$value, pointer)  #make the cdr move on the first pointer
            
            child <- Node$new('nameofthisnode') #give the new node a unique name
            child$value <- child.val
            child$name <- paste(unlist(child$value),collapse=' ')  # Name it correctly 
            
            child <- node$AddChildNode(child)
            
            #identical ending name handling catches duplicates. Names WIN+, WIN-, and DRAW outcomes
            endname<-paste(unlist(tail(pile, n=1)[[1]]),collapse=' ') #this is the name of the last element in the pile
            startname<-paste(unlist(pile[[1]]),collapse=' ') # this is the name of the first element in the pile
            
            if(child$name==endname){
                #   child$name <- paste("-WIN ",child$name,sep='')
                win.counter<<-win.counter+1
                countswitch<<-1
                #    print(win.counter)
                #    print("is a --------- win counter")
                
                return(eval.parent(parse(text="next"),1))
                #full name is only needed for naming edges with pointers
            } 
            if(child$name==startname){
                #   child$name <- paste("+WIN ",child$name,sep='')
                win.counter<<-win.counter+1
                countswitch<<-1
                #   print(win.counter)
                #   print("is a ++++++++ win counter")
                
                return(eval.parent(parse(text="next"),1))
                #full name is only needed for naming edges with pointers
            } 
            
            pointersleft<-pointersleft-1
            Recall(child)    # recurse with child
        }
        
    }
    #####end cdr tree maker
    for(gamestate in pile){
        root <- Node$new('v0')  
        root$value <- gamestate
        root$name <- paste(paste(unlist(root$value),collapse=' ')) #name this the same as the value collapsed in type char
        have.kids(root)
    }
    
    output.text<-paste("R^",pile.length," has ",win.counter," winnable gamestates.") 
    return(output.text)
}


###############
# cdrwincountlooper, use a vector of numbers to define which gamestate lists are processed
# Example : cdrwincountlooper(2:6)

cdrwincountlooper<-function(range){
    outlist<-list()
    for(i in range){
        wincounted<-cdrwincount(gen.cdrpile(i))
        outlist[[length(outlist)+1]]<-paste0("R^",i," has ",wincounted," winnable gamestates.")
    }
}