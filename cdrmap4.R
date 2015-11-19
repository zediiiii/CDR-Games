require(data.tree)

# Helper function to insert nodes as child of parent
i=1
assn <- function(child,parentvarname){
    child<-paste(child,collapse=" ")
    nam <- paste("v", i, sep = "")
    assign(nam, parentvarname$AddChild(child),envir = .GlobalEnv) # tree accessible outside the function
    noquote(nam)->a
    i+1
    a                   #output the child variable name, ie, vn, for the sake of recursion
}

cdrtree<<- function(root){

    #assign root
    v0 <<- Node$new(root)
    node<-root
    kidparentname<-v0
        have.kids<-function(node){   #this is unfortunately asexual reproduction...
                for(pointer in cdrpointers(node)){
                    cdrmove(node,pointer)->newkid
                    assn(newkid,kidparentname) #enter this node in the tree hierarchy
                    kidparentname<-assn(newkid,kidparentname)   #get the name of newkid for the next iteration
                    node<-newkid    
                    have.kids(newkid)   #recurse
                }
    #return the tree
    return(v0)
        }
}