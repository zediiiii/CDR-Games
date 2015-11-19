## Name nodes uniquely, dont be assigning to the .Globalenv like
## you are in `assn`, which wont work becuse `i` isn't being incremented.
## You could invcrement `i` in the global, but, instead,
## I would encapsulate `i` in the function's parent.frame, avoiding possible conflicts
nodeNamer <- function() {
    i <- 0
    ## Note: `i` is incremented outside of the scope of this function using `<<-`
    function(node) sprintf("v%g", (i <<- i+1))
}

## Load your functions, havent looked at these too closely,
## so just gonna assume they work
#source(file="https://raw.githubusercontent.com/zediiiii/CDS/master/Generating%20Scripts.r")

cdrtree <- function(root.value) {
    root <- Node$new('root')  # assign root
    root$value <- root.value  # There seems to be a separation of value from name
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
            child <- Node$new(name_node())             # give the node a name
            child$value <- child_val
            child <- node$AddChildNode(child)
            Recall(child)                              # recurse with child
        }
    }
    have.kids(root)
    return( root )
}

library(data.tree)
res <- cdrtree(root.value=c(1,-2,3))