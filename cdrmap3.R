#dataframes only support atomic elements (not vectors) so will need an apply function to convert all at end
#convert to character            paste(a,collapse=" ")
#convert a back from character   string scan(text = a, what = 0L, sep=" ")
#another way                     as.integer(unlist(strsplit(a," ")))

#for(j in cdrpointers(x[[3]])){ass(cdrmove(gamestate,j))}

data.frame()->df
i=0
ass<- function(child,parent){
    child<-paste(child,collapse=" ")
    parent<-paste(parent,collapse=" ")
    nam <- paste("v", i, sep = "")
    assign(nam, paste0(child))
    
    assign(nam,append(parent,child))->a
    a
    #i+1
    #df$nam <-a
}


i=0
ass<- function(child,parent,varname){
    child<-paste(child,collapse=" ")
    parent<-paste(parent,collapse=" ")
    varname<-append(parent,child)    
}

    make.tree <- function(gamestate){
    
    #generate a list of vector names that will be longer that the depth of the tree for assignment v1,v2,vn
    my.keys <- paste("v",1:1000,sep="")
    keylist <- vector(mode="list", length=length(my.keys))
    names(keylist) <- my.keys
    my.keys[1]
    
    
    
    
    }
    
    for(j in cdrpointers(gamestate)){
        ass(cdrmove(gamestate,j))
    }
    
    
    ass(i,gamestate)
    
    v1<-gamestate

    
    
    #Generate the first set of children
    #note, generating the new vector outside the loops will optimize code use
    #v2 <- character(length(cdrpointers(v1)))
    
    v2<-character()
    for(i in seq(length(cdrpointers(gamestate)))){
        temp<-cdrmove(gamestate,cdrpointers(gamestate)[i])
        append(v2,temp)
    }
    v2
    
}