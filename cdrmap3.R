#dataframes only support atomic elements (not vectors) so will need an apply function to convert all at end
#convert to character            paste(a,collapse=" ")
#convert a back from character   string scan(text = a, what = 0L, sep=" ")
#another way                     as.integer(unlist(strsplit(a," ")))

make.tree <- function(gamestate){
    
    #generate a list of vector names that will be longer that the depth of the tree for assignment v1,v2,vn
    my_keys <- letters[1:26]
    mylist <- vector(mode="list", length=length(my_keys))
    names(mylist) <- my_keys
    
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