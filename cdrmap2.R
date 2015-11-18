## Function to apply to nodes to create children
nodeNamer <- function() {
    i <- 0
    function(parentnode) sprintf("%s -> %g", parentnode$name, (i <<- i+1))
}

make_tree <- function(root) { #let pointer length determine m
    depth<-0
    node<-root
    root <- Node$new(root)
    
    
    
    ## make a function that pops out all the babies for the present node at depth
    f <- function(node, depth) {
        if (depth <= 0) return( x[[300]] )
        #define children number for this node length(cdrpointers(node))
        
        #make all the babies
        for (l in seq.int(length(cdrpointers(node)))) { #m is number of pointers at parent node
            val <- cdrmove(node,cdrpointers(node)[l])  # apply cdr
            child <- node$AddChild(val)
            Recall(child, depth-1)  # recurse, Recall refers to 'f'
        }
    
    }
    
    f(root, depth)
    return( root )
}





## Function to apply to nodes to create children
nodeNamer <- function() {
    i <- 0
    function(node) sprintf("%s -> %g", node$name, (i <<- i+1))
}

make_tree <- function(depth, m, fn) {
    root <- Node$new('root')
    
    ## Some helper function to recurse
    f <- function(node, depth) {
        if (depth <= 0) return( root )
        for (i in seq.int(m)) {
            val <- fn(node)  # apply some function to parent node
            child <- node$AddChild(val)
            Recall(child, depth-1)  # recurse, Recall refers to 'f'
        }
    }
    f(root, depth)
    return( root )
}

## Make a tree with depth of '2' and 3 branches from each node
## Note that the way I defined the naming function, you must call it 
## in order to reset the the counter to 0
res <- make_tree(2, 3, nodeNamer())

res
#                  levelName
# 1  root                   
# 2   ¦--root -> 1          
# 3   ¦   ¦--root -> 1 -> 2 
# 4   ¦   ¦--root -> 1 -> 3 
# 5   ¦   °--root -> 1 -> 4 
# 6   ¦--root -> 5          
# 7   ¦   ¦--root -> 5 -> 6 
# 8   ¦   ¦--root -> 5 -> 7 
# 9   ¦   °--root -> 5 -> 8 
# 10  °--root -> 9          
# 11      ¦--root -> 9 -> 10
# 12      ¦--root -> 9 -> 11
# 13      °--root -> 9 -> 12
