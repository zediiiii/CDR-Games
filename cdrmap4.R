

#v0 <- Node$new(paste(x[[2]],collapse=" "))


cdrtree<<- function(root){

    i=1
    ass <- function(child,parentvarname){
        child<-paste(child,collapse=" ")
        nam <- paste("v", i, sep = "")
        assign(nam, parentvarname$AddChild(child),envir = .GlobalEnv)
        noquote(nam)->a
        i+1
        a                   #output the child variable name, ie, vn
    
    }
    #assign root
    v0 <<- Node$new(root)
    node<-root
    kidparentname<-v0
        have.kids<- function(node){   #this is unfortunately asexual reproduction...
            for(pointer in cdrpointers(node)){
                cdrmove(node,pointer)->newkid
                kidparentname<-ass(newkid,kidparentname)
                    have.kids(newkid)
            }
    #find kids
    
}
}