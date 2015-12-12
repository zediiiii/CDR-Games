####################
# Notes about this script
# An initial misconception led to the use of the word "pile" to represent the list of all game state permutations which we
# denote R_n. For the sake of preserving function, the word "pile" has been left in various places in the script.
# Also note that this package is does not follow some global variable rules that CRAN enforces, and could 
# theoreticall break some packages - though this is unlikely. 

####################
# function:     gen.cdspile()
# purpose:		Generates a sorted list of the elements in the CDS strategic pile.
# parameters:	n: number of elements
# Author:       Joshua Watson Oct 2015

require(combinat) 

gen.cdspile <- function(n){
    permn(n)->x
    x[ order( sapply(x, paste0, collapse=".")) ]
}


####################
# function:     gen.cdrpile()
# purpose:        Generates a sorted list of the elements in the CDR strategic pile.
#               Each S_n is placed before all combinations of +/- in binary order.
# parameters:	n:The number     of elements in the set G^n 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss.r ; gen.bincomb.r; combinat
require(combinat)

gen.cdrpile <- function(n){
    
    #instantiate variables
    alt.list <-list()   
    
    gen.cdspile(n)->ordered.symset #get list of sym_n elements and order it
    
    #Generate the binary sorted list mask
    gen.bincomb(n)->binlist
    bin.length<-length(binlist)
    
    #Loop over the S_n set
    for(k in ordered.symset){
        #Then loop over the binlist
        for(i in 1:bin.length){
            temp<-binlist[[i]]*k
            alt.list[[length(alt.list)+1]] <- temp
        }
    
    }
    alt.list
}

####################
# function:     gen.bincomb()
# purpose:      Generates a list sorted in binary order of n binary elements per list entry.
#               Since it sorts by value magnitude, you may or may not need to use the rev argument to reverse the order
# parameters:    n:Number of binary elements per list entry
#               bin:A two element vector comtaining numeric binary values. Default is c(-1,1).
#               rev:reverse the order if true. Needed depending on the numbers chosen.
# Author:       Joshua Watson Nov 2015
require(combinat)

gen.bincomb <- function(n,bin=c(1,-1),rev=TRUE){
    lst <- lapply(numeric(n), function(x) bin)
    mat <- as.matrix(expand.grid(lst))
    
    #Generate the code to sort in binary order.
    
    ix<-paste("binarray<-order(",collapse="")
    for(k in 1:ncol(mat)){
        ix<-paste(ix,"mat[,",k,"]",collapse="")
        if(k<ncol(mat)){
            ix<-paste(ix,",",collapse="")
        }
        if(k==ncol(mat)){
            ix<-paste(ix,")",collapse="")  
        }
        
    }
    
    #parse the string to sort and run it
    eval(parse( text=ix ))
    
    #get the nicely sorted matrix
    out<-mat[binarray,]          
    
    #Convert to list of lists
    out<-tapply(out,rep(1:nrow(out),ncol(out)),function(i)i)
    
    #reverse the order to make it in binary order if needed
    if(rev==TRUE){
        rev(out)
    }
    else{
        out
    }
}

####################
# function:     cdrindex()
# purpose:      Get the index number of a particular gamestate in a CDR game
# parameters:   n:Number of binary elements per list entry
# Author:       Joshua Watson Nov 2015
# TODO:         Make this draw on a database rather than doing the calculations on the fly.

require(combinat)

cdrindex <- function(gamestate){
    n<-length(gamestate)
    list<-gen.cdrpile(n)
    which(sapply(list, identical, gamestate ))
}

####################
# function:     makecdrfiles()
# purpose:      Create CDR data files seperated by space for each number and return for each 
#               gamestate (list element) in a range from m to n elements per gamestate 
# parameters:   m: the number of elements n in R^n for the first data file to be generated
#               n: the number of elements m in R^m for the last data file to be generated 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss; gen.bincomb; gen.cdrpile

require(combinat)
makecdrfiles <- function(m,n){
    
    for(i in n:m){
        gen.cdrpile(i)->temp
        lapply(temp, write, paste("R^",i,".txt",sep=""), append=TRUE, ncolumns=length(x)) 
    }

}

####################
# function:     makecdsfiles()
# purpose:      Create CDS data files seperated by space for each number and return for each 
#               gamestate (list element) in a range from m to n elements per gamestate.
# parameters:   m: the number of elements n in S^n for the first data file to be generated
#               n: the number of elements m in S^m for the last data file to be generated 
# Author:       Joshua Watson Nov 2015
# Dependancies: sort.listss; gen.bincomb; gen.cdrpile
require(combinat)
makecdrfiles <- function(m,n){
    
    for(i in n:m){
        gen.cdrpile(i)->temp
        lapply(temp, write, paste("S^",i,".txt",sep=""), append=TRUE, ncolumns=length(x)) 
    }
    
}

####################
# function:     cdrmove()
# purpose:      Perform a CDR move given a certain pointer.
# parameters:   game:a gamestate such as c(-2,-1,4,-5,3)
#               pointer: a particular pointer of the form c(n,-m) generated by the cdrpointers script
# Author:       Joshua Watson & Alyssa Seidman 2015
# Example:      cdrmove(gen.cdrpile(3)[5],cdrpointers(gen.cdrpile(3)[5])[1])
# Dependancies: gen.cdrpile and cdrpointers are recommended (though not needed)

cdrmove <- function(gamestate,pointer){
    
    ##### This block of code finds the index of the block to have CDR performed on it ####
  
      if(length(pointer)<=0){
        return(list())
        break
    }
    which(unlist(pointer)[1] == gamestate)[[1]]->p1
    which(unlist(pointer)[2] == gamestate)[[1]]->p2
    
    if(abs(p1-p2)==1){ #ie the two numbers are adjacent
        if(p1<p2){ #ie p1 is left of p2
            if(abs(gamestate[p1]>abs(gamestate[p2]))){ #ie if the value of p1 is bigger than val p2
                block<-p1              #then set the block to be just p1
            } else {
                block<-p2
            }
        } else {   #ie p1 is right of p2
            if(abs(gamestate[p1]>abs(gamestate[p2]))){ #ie if the value of p1 is bigger than val p2
                block<-p2
            } else {
                block<-p1 
            } #end else            
        } #end else
        
    } else {
        if(p1<p2){
            if(abs(gamestate[p1]<abs(gamestate[p2]))){ #ie if the value of p1 is smaller than val p2
                block<-(p1+1):p2          #then set the block to be the indexes of p+1 through p2
            } else {
                block<-p1:(p2-1)
                }  #ie, if p1 is left of p2 not adjacent
        } else {
        if(p1>p2){
            if(abs(gamestate[p1]<abs(gamestate[p2]))){
                block<-(p2+1):p1
            } else {
                block<-p2:(p1-1)
                }
        } #end else
            } #end else
        } #end else

    #### Performs the flip and negation and overwrites block index elements with new neg.block ####
    neg.block<-rev(gamestate[block])*-1
    new.gamestate<-gamestate
    new.gamestate[block]<-neg.block
    new.gamestate
}

####################
# function:     cdrpointers()
# purpose:      Finds the pointers of a particular CDR gamestate and outputs a list of pointer vectors
#               Note that the code works so that the second number is always the negative number.
# parameters:	game: a cdr gamestate in vector form, ie, c(1,2,3,4,5)
# Example:      cdrpointers(c(-2,1,3,-4,5))
# Author:       Joshua Watson & Alyssa Seidman 2015


cdrpointers <- function(gamestate){
    
    poscount<-0
    pointerlist<-list()
    
    for(i in 1:length(gamestate)){  #Check for all positive or all negative
        if(gamestate[i]>0){
            poscount<-(poscount+1)
        }
    }
    if((poscount==length(gamestate)) || (poscount==0)){
        return(list())
    }
    
    for(i in 1:length(gamestate)){
        pointer<-gamestate[i]
        if(pointer>0){
            for(j in 1:length(gamestate)){
                pointercheck<-gamestate[j]
                if((pointercheck==-(pointer+1)) | (pointercheck==-(pointer-1))){
                    pointerlist[[length(pointerlist)+1]] <- c(gamestate[i],gamestate[j])
                }
            }
        }
    }
    pointerlist
}


####################
# function:     what.pointer()
# purpose:      Given two gamestates, finds the pointer from the first to the second.
# parameters:	state1: a cdr gamestate in vector form, ie, c(1,2,3,4,5)
#               state2: a cdr gamestate in vector form, ie, c(1,2,3,4,5)
# example:      what.pointer(c(2,3,-5,-4,-1),c(2,3,4,5,-1))
# Author:       Joshua Watson 2015


what.pointer<-function(state1,state2){
    
    movelist<-list()
    orderedpointerlist<-list()
    
    list.of.pointers<-cdrpointers(state1)
    for(i in 1:length(list.of.pointers)){
        append(paste(cdrmove(state1,list.of.pointers[[i]]),collapse=" "),movelist)->movelist
        append(paste(list.of.pointers[[i]],collapse=","),orderedpointerlist)->orderedpointerlist
    }
    
    state2<-paste(state2,collapse=" ")
    pointerindex<-match(state2,movelist)
    orderedpointerlist[[pointerindex]]

}